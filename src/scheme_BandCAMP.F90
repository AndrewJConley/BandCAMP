!> \file
!> Prototype for a CAMP<->CPF interface
module scheme_CAMP

  use pmc_camp_core
  use pmc_camp_state
  use pmc_constants
  use pmc_solver_stats
  use pmc_util

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
  type(camp_core_ptr), allocatable :: cores(:)
  !> CAMP states
  type(camp_state_ptr), allocatable :: states(:)
  !> Species map between CAMP and CPF
  type(state_map_t), allocatable :: species_map(:)

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize (modify this to work with CPF)
  subroutine initialize( CPF_data )

    !> CPF data (modify as needed)
    type(cpf_data_t), intent(inout) :: CPF_data

    ! buffer for passing core data
    character, allocatable :: buffer(:)
    integer :: pack_size, pos

    ! species names for map building
    type(string_t), allocatable :: CAMP_species_names(:)

    ! How do we get the number of threads that will be used during calls to
    ! run( )
    allocate( cores( CPF_data%n_threads ) )
    allocate( states( CPF_data$n_threads ) )

    ! load and process the CAMP input data on the first core
    cores(1)%core => camp_core_t( CPF_data%path_to_CAMP_config_file )
    call cores(1)%core%initialize( )

    ! pack the core data
    pack_size = cores(1)%core%pack_size( )
    allocate( buffer( pack_size ) )
    pos = 0
    call cores(1)%core%bin_pack( buffer, pos )

    ! initialize remaining cores from the packed data
    do i_thread = 2, CPF_data%n_threads
      cores( i_thread )%core => camp_core_t( )
      pos = 0
      call cores( i_thread )%core%bin_unpack( buffer, pos )
    end do

    ! initialize solvers and get states for each thread
    do i_thread = 1, CPF_data%n_threads
      call cores( i_thread )%core%solver_initialize( )
      states( i_thread )%state => cores( i_thread )%core%new_state( )
    end do

    ! Build the species map
    CAMP_species_names = cores( i_thread )%core%unique_names( )
    allocate( species_map( size( CAMP_species_names ) ) )
    do i_spec = 1, size( species_map )
      species_map%CAMP_idx = i_spec
      species_map%CPF_idx  = &
        CPF_data%lookup_species_by_CAMP_name( CAMP_species_names( i_spec ) )
    end do

    deallocate( buffer )
    if( allocate( CAMP_species_names ) ) deallocate( CAMP_species_names )

  end subroutine initialize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Run (modify this to work with CPF)
  subroutine run( CPF_data )

    use pmc_solver_stats

    !> CPF data (modify as needed)
    type(cpf_data_t), intent(inout) :: CPF_data

    integer :: i_thread
    type(camp_state_t), pointer :: camp_state
    type(solver_stats_t) :: solver_stats

    i_thread = CPF_data%i_thread

    ! Solve chemistry on each grid cell associated with this thread
    do i_cell = CPF_data%first_cell, CPF_data%last_cell

      ! Set the environmental conditions and state
      camp_state => states( i_thread )%state
      camp_state%env_states( 1 )%set_temperature_K( &
        CPF_data%get_temperature( i_cell ) )
      camp_state%env_states( 1 )%set_pressure_Pa( &
        CPF_data%get_pressure( i_cell ) )
      do i_spec = 1, size( species_map )
        camp_state%state_var( species_map( i_spec )%CAMP_idx ) = &
          CPF_data%get_species_conc( species_map( i_spec )%CPF_idx )
      end do

      ! Solve the chemistry on this cell
      call cores( i_thread )%core%solve( camp_state, solver_stats )

      ! Analyze the results
      if( solver_stats%status_code.ne.0 ) call do_meltdown( )

      ! Update the CPF state
      do i_spec = 1, size( species_map )
        CPF_data%get_species_conc( species_map( i_spec )%CPF_idx ) = &
          camp_state%state_var( species_map( i_spec )%CAMP_idx )
      end do

  end subroutine run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize the module variables
  subroutine finalize( CPF_data )

    !> CPF data (modify as needed)
    type(cpf_data_t), intent(inout) :: CPF_data

    integer :: i_elem

    do i_elem = 1, size( cores  )
      if( associated( cores( i_elem ) deallocate( cores( i_elem )%core )
    end do
    do i_elem = 1, size( states )
      if( associated( states( i_elem ) deallocate( states( i_elem )%state )
    end do

    deallocate( cores )
    deallocate( states )
    deallocate( species_map )

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module scheme_CAMP
