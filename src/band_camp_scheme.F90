!> \file
!> Prototype for a CAMP<->CPF interface
module band_camp_scheme

  use ccpp_kinds,            only : kind_phys
  use pmc_camp_core,         only : camp_core_t
  use pmc_camp_state,        only : camp_state_t
  use pmc_constants,         only : dp, i_kind
  use pmc_solver_stats,      only : solver_stats_t
  use pmc_util,              only : assert_msg, string_array_find, string_t, &
                                    to_string

  implicit none

  !> Pointer for CAMP core objects (for building arrays of cores)
  type :: camp_core_ptr
    type(camp_core_t), pointer :: core
  end type camp_core_ptr

  !> Pointer for CAMP state objects (for building arrays of states)
  type :: camp_state_ptr
    type(camp_state_t), pointer :: state
  end type camp_state_ptr

  !> Map between CPF and CAMP species states
  type :: state_map_t
    !> Index of the species in a camp_state_t state array
    integer :: CAMP_idx
    !> Index of the species in CPF
    !! (This could be something other than an integer if needed)
    integer :: CPF_idx
  end type state_map_t

  !> CAMP cores for solving chemistry
  type(camp_core_ptr), allocatable :: camp_cores(:)
  !> CAMP states
  type(camp_state_ptr), allocatable :: camp_states(:)
  !> Species map between CAMP and CPF
  type(state_map_t), allocatable :: species_map(:)

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> \section arg_table_band_camp_scheme_init Argument Table
  !! \htmlinclude band_camp_scheme_init.html
  !!
  subroutine band_camp_scheme_init( num_chem_species, chem_species_names,    &
                                    path_to_camp_config_file, num_threads,   &
                                    error_message, error_flag )

    !> Number of chemical species in the host model
    integer, intent(in)             :: num_chem_species
    !> Chemical species names in the host model
    character(len=50), dimension(num_chem_species), intent(in) ::            &
      chem_species_names
    !> Path to CAMP config.json file (used to initialize the camp_core)
    character(len=512), intent(in)  :: path_to_camp_config_file
    !> Number of threads used during solving
    integer, intent(in)             :: num_threads
    !> Error message
    character(len=512), intent(out) :: error_message
    !> Error flag (0 for success)
    integer, intent(out)            :: error_flag

    ! species names for map building
    type(string_t), allocatable :: CAMP_species_names(:)

    ! buffer for passing core data
    character, allocatable :: buffer(:)
    integer :: pack_size, pos

    integer :: i_spec, i_thread

    ! hope for the best
    error_message = ''
    error_flag    = 1

    ! load and process the CAMP input data
    camp_cores( 1 )%core => camp_core_t( path_to_camp_config_file )
    call camp_cores( 1 )%core%initialize( )

    ! pack the core data and interface
    pack_size = camp_cores( 1 )%core%pack_size( )
    allocate( buffer( pack_size ) )
    pos = 0
    call camp_cores( 1 )%core%bin_pack( buffer, pos )

    ! initialize remaining cores from the packed data
    do i_thread = 2, num_threads
      camp_cores( i_thread )%core => camp_core_t( )
      pos = 0
      call camp_cores( i_thread )%core%bin_unpack( buffer, pos )
    end do

    ! Initialize the solver and create a camp state for each thread
    do i_thread = 1, num_threads
      call camp_cores( i_thread )%core%solver_initialize( )
      camp_states( i_thread )%state  =>                                      &
        camp_cores( i_thread )%core%new_state( )
    end do

    ! Build the species map
    CAMP_species_names = camp_cores( 1 )%core%unique_names( )
    allocate( species_map( size( CAMP_species_names ) ) )
    do i_spec = 1, size( species_map )
      species_map%CAMP_idx = i_spec
      species_map%CPF_idx  = &
        string_array_find( chem_species_names,                               &
                           CAMP_species_names( i_spec )%string )
    end do

  end subroutine band_camp_scheme_init

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> \section arg_table_band_camp_scheme_run Argument Table
  !! \htmlinclude band_camp_scheme_run.html
  !!
  subroutine band_camp_scheme_run( dt__s, num_chem_species,                  &
                                   chem_species_conc__ppm, temperature__K,   &
                                   pressure__Pa, i_thread, error_message,    &
                                   error_flag )

    use pmc_solver_stats

    !> Time step for chemistry [s]
    real(kind=kind_phys), intent(in) :: dt__s
    !> Number of chemical species
    integer, intent(in) :: num_chem_species
    !> Chemical species concentrations (ppm)
    real(kind=kind_phys), dimension(num_chem_species), intent(inout) ::      &
      chem_species_conc__ppm
    !> Temperature [K]
    real(kind=kind_phys), intent(in) :: temperature__K
    !> Pressure [Pa]
    real(kind=kind_phys), intent(in) :: pressure__Pa
    !> Index of the current thread
    integer, intent(in)              :: i_thread
    !> Error message
    character(len=512), intent(out)  :: error_message
    !> Error flag (0 for success)
    integer, intent(out)             :: error_flag

    type(solver_stats_t)        :: solver_stats
    type(camp_core_t), pointer  :: camp_core
    type(camp_state_t), pointer :: camp_state

    integer :: i_spec

    ! hope for the best
    error_message = ''
    error_flag    = 1

    ! attach to the core and state for this thread
    camp_core  => camp_cores(  i_thread )%core
    camp_state => camp_states( i_thread )%state

    ! Set the environmental conditions and state
    call camp_state%env_states( 1 )%set_temperature_K( temperature__K )
    call camp_state%env_states( 1 )%set_pressure_Pa(     pressure__Pa )
    do i_spec = 1, size( species_map )
      if( species_map( i_spec )%CPF_idx .eq. 0 ) cycle
      camp_state%state_var( species_map( i_spec )%CAMP_idx ) =               &
        chem_species_conc__ppm( species_map( i_spec )%CPF_idx )
    end do

    ! Solve the chemistry for this cell
    call camp_core%solve( camp_state, dt__s, solver_stats = solver_stats )

    ! Analyze the results
    if( solver_stats%status_code.ne.0 ) then
      error_flag    = 1
      error_message = "CAMP solver failed with code "//                      &
                        to_string( solver_stats%solver_flag )
      return
    end if

    ! Update the host model state
    do i_spec = 1, size( species_map )
      if( species_map( i_spec )%CPF_idx .eq. 0 ) cycle
      chem_species_conc__ppm( species_map( i_spec )%CPF_idx ) =     &
        camp_state%state_var( species_map( i_spec )%CAMP_idx )
    end do

  end subroutine band_camp_scheme_run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> \section arg_table_band_camp_scheme_finalize Argument Table
  !! \htmlinclude band_camp_scheme_finalize.html
  !!
  subroutine band_camp_scheme_finalize( error_message, error_flag )

    !> Error message
    character(len=512), intent(out)  :: error_message
    !> Error flag (0 for success)
    integer, intent(out)             :: error_flag

    integer :: i_elem

    ! hope for the best
    error_message = ''
    error_flag    = 1

    if( allocated( camp_cores ) ) then
      do i_elem = 1, size( camp_cores )
        if( associated( camp_cores( i_elem )%core ) )                        &
          deallocate( camp_cores( i_elem )%core )
      end do
      deallocate( camp_cores )
    end if

    if( allocated( camp_states ) ) then
      do i_elem = 1, size( camp_states )
        if( associated( camp_states( i_elem )%state ) )                      &
          deallocate( camp_states( i_elem )%state )
      end do
      deallocate( camp_states )
    end if

    if( allocated( species_map ) ) deallocate( species_map )

  end subroutine band_camp_scheme_finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module band_camp_scheme
