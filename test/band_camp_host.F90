!> \file
!> Mock model for scheme tests
module band_camp_host_main

  use ccpp_kinds,            only : kind_phys
  use band_camp_mod,         only : time_start__s => time_start,             &
                                    time_end__s   => time_end,               &
                                    dt__s         => dt,                     &
                                    initialize_data, output_model_state
  use band_camp_ccpp_cap,    only : band_camp_ccpp_physics_initialize
  use band_camp_ccpp_cap,    only : band_camp_ccpp_physics_timestep_initial
  use band_camp_ccpp_cap,    only : band_camp_ccpp_physics_run
  use band_camp_ccpp_cap,    only : band_camp_ccpp_physics_timestep_final
  use band_camp_ccpp_cap,    only : band_camp_ccpp_physics_finalize
  use band_camp_ccpp_cap,    only : ccpp_physics_suite_list
  use band_camp_ccpp_cap,    only : ccpp_physics_suite_part_list
  use pmc_util,              only : to_string

  implicit none

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> \section arg_table_band_camp_host_sub  Argument Table
  !! \htmlinclude arg_table_band_camp_host_sub.html
  !!
  subroutine band_camp_host_sub( )

    !> Error message returned by ccpp subroutines
    character(len=512) :: error_message
    !> Error flag returned by ccpp subroutine (0 for success)
    integer            :: error_flag
    !> Total simulation time [s]
    real               :: simulation_time__s

    time_start__s = 0.0
    time_end__s   = 100.0
    dt__s         = 5.0

    ! Initialize the test-specific data
    call initialize_data( )

    ! Initialize the mock suite
    call band_camp_ccpp_physics_initialize("band_camp_suite",                &
                                           error_message,                    &
                                           error_flag )
    call check_error_flag( error_flag, error_message )

    ! Set the simulation time
    simulation_time__s = time_start__s

    ! Output the initial conditions
    write(6,*) "Model state at ", simulation_time__s, " s"
    call output_model_state( )

    ! Run the model
    time_loop : do while( simulation_time__s .lt. time_end__s )

      ! Announce a new time step
      call band_camp_ccpp_physics_timestep_initial("band_camp_suite",        &
                                                   error_message,            &
                                                   error_flag )
      call check_error_flag( error_flag, error_message )

      ! Run the BandCAMP scheme
      call band_camp_ccpp_physics_run("band_camp_suite",                     &
                                      "band_camp_group",                     &
                                      error_message,                         &
                                      error_flag )
      call check_error_flag( error_flag, error_message )

      ! Announce the end of the timestep
      call band_camp_ccpp_physics_timestep_final("band_camp_suite",          &
                                                 error_message,              &
                                                 error_flag )
      call check_error_flag( error_flag, error_message )

      ! Advance the simulation time
      simulation_time__s = simulation_time__s + dt__s

      ! Output current model state
      write(6,*) "Model state at ", trim( to_string( simulation_time__s ) ), &
                 " s"
      call output_model_state( )

    end do time_loop

    ! Finalize the band camp suite
    call band_camp_ccpp_physics_finalize("band_camp_suite",                  &
                                         error_message,                      &
                                         error_flag )
    call check_error_flag( error_flag, error_message )

  end subroutine band_camp_host_sub

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Check an error flag and fail the run if necessary
  subroutine check_error_flag( error_flag, error_message )

    !> Error flag to evaluate
    integer, intent(in) :: error_flag
    !> Error message to return on failure
    character(len=512), intent(in) :: error_message

    if( error_flag .ne. 0 ) then
      write(6,*) trim( error_message )
      stop
    end if

  end subroutine check_error_flag

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module band_camp_host_main

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> Mock model driver
program band_camp_host
  use band_camp_host_main
  call band_camp_host_sub( )
end program band_camp_host

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
