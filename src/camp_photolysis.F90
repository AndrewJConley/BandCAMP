!> \file
!> CAMP<->CPF photolysis interface
module camp_photolysis

  use pmc_camp_core
  use pmc_constants
  use pmc_mechanism_data
  use pmc_rxn_data
  use pmc_rxn_factory
  use pmc_rxn_photolysis
  use pmc_util

  implicit none

  type camp_photolysis_t
    private
    !> Photolysis reaction updaters
    type(rxn_update_data_photolysis_t), allocatable :: photo_rxn_updater(:)
    !> Index for photolysis species in the CPF data for each reaction
    integer(kind=i_kind), allocatable :: cpf_species_id(:)
  contains
    !> Initialize a photolysis interface
    procedure :: initialize
    !> Update the photolysis reaction rates based on CPF photolysis data
    procedure :: update_rates
    !> Determine the size of a buffer needed to pack the object
    procedure :: pack_size
    !> Pack the object to a buffer, advancing position
    procedure :: bin_pack
    !> Unpack an object from a buffer, advancing position
    procedure :: bin_unpack
  end type camp_photolysis_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize the photolysis interface (modify this to work woth CPF)
  subroutine initialize( CPF_data, camp_core )

    !> CAMP photolysis interface
    class(camp_photolysis_t), intent(in) :: this
    !> CPF data (modify as needed)
    type(cpf_data_t), intent(inout) :: CPF_data
    !> CAMP core
    type(camp_core_t), intent(inout) :: camp_core

    integer :: i_mech, i_rxn, i_photo_rxn, n_photo_rxn
    character(len=:), allocatable :: photo_name
    type(mechanism_data_t), pointer :: mechanism
    type(rxn_factory_t) :: rxn_factory
    class(rxn_data_t), pointer :: rxn

    ! Count the number of photolysis reactions with a tuv_name
    n_photo_rxn = 0
    do i_mech = 1, size( camp_core%mechanism )
      mechanism => camp_core%mechanism( i_mech )
      do i_rxn = 1, mechanism%size( )
        select type( rxn )
        class is( rxn_photolysis_t )
          if( rxn%property_set%get_string( "tuv_name", photo_name ) ) then
            n_photo_rxn = n_photo_rxn + 1
          end if
        end select
      end do
    end do

    ! Allocate the update objects and map
    allocate( this%photo_rxn_updater( n_photo_rxn ) )
    allocate( this%cpf_species_id(    n_photo_rxn ) )

    ! Initialize an update object for each reaction and set the cpf ids
    i_photo_rxn = 0
    do i_mech = 1, size( camp_core%mechanism )
      mechanism => camp_core%mechanism( i_mech )
      do i_rxn = 1, mechanism%size( )
        select type( rxn )
        class is( rxn_photolysis_t )
          if( rxn%property_set%get_string( "tuv_name", photo_name ) ) then
            if( .not.CPF_data%get_photo_id_by_name( photo_name, cpf_id ) ) then
              call die_msg( 795564457, "Cannot find photolysis species "// &
                            photo_name )
            end if
            i_photo_rxn = i_photo_rxn + 1
            this%cpf_species_id( i_photo_rxn ) = cpf_id
            call rxn_factory%initialize_udpate_data( rxn, &
                                       this%photo_rxn_updater( i_photo_rxn ) )
          end if
        end select
      end do
    end do

  end subroutine initialize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Update the photolysis rates
  subroutine update_rates( CPF_data, camp_core )

    !> CAMP photolysis interface
    class(camp_photolysis_t), intent(in) :: this
    !> CPF data (modify as needed)
    type(cpf_data_t), intent(inout) :: CPF_data
    !> CAMP core
    type(camp_core_t), intent(inout) :: camp_core

    real(kind=dp) :: new_rate
    integer :: i_photo_rxn

    do i_photo_rxn = 1, size( this%photo_rxn_updater )
      new_rate = CPF_data%get_photo_rate( this%cpf_species_id( i_photo_rxn ) )
      call this%photo_rxn_updater( i_photo_rxn )%set_rate( new_rate )
      call camp_core%update_rxn_data( this%photo_rxn_updater( i_photo_rxn ) )
    end do

  end subroutine update_rates

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Determine the size of a buffer needed to pack the object
  integer function pack_size( this, comm )

    !> CAMP photolysis interface
    class(camp_photolysis_t), intent(in) :: this
    !> MPI communicator
    integer, intent(in), optional :: comm

    integer :: i_rxn, l_comm

    if( present( comm ) ) then
      l_comm = comm
    else
      l_comm = MPI_COMM_WORLD
    end if

    pack_size = &
      pmc_mpi_pack_size_logical( allocated( this%photo_rxn_updater ) )

    if( allocated( this%photo_rxn_updater ) ) then
      pack_size = pack_size + &
                  pmc_mpi_pack_size_integer( size( this%photo_rxn_updater ) )

      do i_rxn = 1, size( this%photo_rxn_updater )
        pack_size = pack_size + &
                  this%photo_rxn_updater( i_rxn )%pack_size( l_comm )
      end do
    end if

    pack_size = pack_size + &
      pmc_mpi_pack_size_integer_array( size( this%cpf_species_id ), l_comm )

  end function pack_size

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine bin_pack( this, buffer, pos, comm )

    !> CAMP photolysis interface
    class(camp_photolysis_t), intent(in) :: this
    !> Memory buffer
    character, intent(inout) :: buffer(:)
    !> Current buffer position
    integer, intent(inout) :: pos
    !> MPI communicator
    integer, intent(in), optional :: comm

    integer :: prev_pos, i_rxn, l_comm

    if( present( comm ) ) then
      l_comm = comm
    else
      l_comm = MPI_COMM_WORLD
    end if

    prev_pos = pos
    call pmc_mpi_pack_logical( buffer, pos, &
                               allocated( this%photo_rxn_updater ), l_comm )
    if( allocated( this%photo_rxn_updater ) ) then
      call pmc_mpi_pack_integer( buffer, pos, &
                                 size( this%photo_rxn_updater ), l_comm )
      do i_rxn = 1, size( this%photo_rxn_updater )
        call this%photo_rxn_updater( i_rxn )%bin_pack( buffer, pos, l_comm )
      end do
    end if

    pmc_mpi_pack_integer_array( buffer, pos, this%cpf_species_id, l_comm )

  end subroutine bin_pack

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine bin_unpack( this, buffer, pos, comm )

    !> CAMP photolysis interface
    class(camp_photolysis_t), intent(out) :: this
    !> Memory buffer
    character, intent(in) :: buffer(:)
    !> Current buffer position
    integer, intent(inout) :: pos
    !> MPI communicator
    integer, intent(in), optional :: comm

    logical :: is_allocated
    integer :: prev_pos, i_rxn, n_rxns, l_comm

    if( present( comm ) ) then
      l_comm = comm
    else
      l_comm = MPI_COMM_WORLD
    end if

    prev_pos = pos
    call pmc_mpi_unpack_logical( buffer, pos, is_allocated, l_comm )
    if( is_allocated ) then
      call pmc_mpi_unpack_integer( buffer, pos, n_rxns, l_comm )
      allocate( this%photo_rxn_updater( n_rxns ) )
      do i_rxn = 1, n_rxns
        call this%photo_rxn_updater( i_rxn )%bin_unpack( buffer, pos, l_comm )
      end do
    end if

    pmc_mpi_unpack_integer_array( buffer, pos, this%cpf_species_id, l_comm )

  end subroutine bin_pack

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module camp_photolysis
