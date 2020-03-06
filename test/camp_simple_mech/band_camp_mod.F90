!> \file
!> Data module for the BandCAMP mock host model
module band_camp_mod

  use ccpp_kinds,            only : kind_phys

  implicit none
  public

  !> \section arg_table_band_camp_mod Argument Table
  !! \htmlinclude arg_table_band_camp_mod.html
  !!
  real(kind_phys) :: dt
  real(kind_phys) :: time_start
  real(kind_phys) :: time_end
  real(kind_phys) :: temperature
  real(kind_phys) :: pressure

  integer, parameter :: number_of_chemical_species = 3
  integer            :: number_of_threads
  integer            :: thread_index

  character(len=512) :: path_to_camp_config_file

  real(kind_phys), dimension(number_of_chemical_species) ::                  &
    chemical_species_concentrations

  character(len=50), dimension(number_of_chemical_species) ::                &
    chemical_species_names

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize the module data
  subroutine initialize_data( )

    number_of_threads = 1
    i_thread          = 1

    path_to_camp_config_file = "../config.json"

    chemical_species_names = (/"species A","species B","species C"/)
    chemical_species_concentrations = (/12.0, 30.1, 50.0/)

  end subroutine intiialize_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Output model state
  subroutine output_model_state( )

    integer :: i_species

    write(6,*) "Temperature",trim(to_string(temperature),"K"
    write(6,*) "Pressure   ",trim(to_string(pressure   ),"Pa"
    do i_species = 1, number_of_species
      write(6.*) trim( chemical_species_names( i_species ) ),":",            &
                 chemical_species_concentrations( i_species ),"ppm"
    end do

  end subroutine output_model_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module band_camp_mod
