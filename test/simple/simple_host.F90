!> \file
!> Mock ccpp host model for tests
module simple_host_main

  use ccpp_kinds,            only : kind_phys
  use simple_mod,            only : foo, bar, dt, time_start, time_end
  use simple_ccpp_cap,       only : simple_ccpp_physics_initialize
  use simple_ccpp_cap,       only : simple_ccpp_physics_timestep_initial
  use simple_ccpp_cap,       only : simple_ccpp_physics_run
  use simple_ccpp_cap,       only : simple_ccpp_physics_timestep_final
  use simple_ccpp_cap,       only : simple_ccpp_physics_finalize
  use simple_ccpp_cap,       only : ccpp_physics_suite_list
  use simple_ccpp_cap,       only : ccpp_physics_suite_part_list

  implicit none

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> \section arg_table_simple_host_sub  Argument Table
  !! \htmlinclude arg_table_simple_host_sub.html
  !!
  subroutine simple_host_sub()

    character(len=512) :: errmsg
    integer            :: errflg
    real               :: sim_time

    time_start = 0.0
    time_end   = 100.0
    dt         = 5.0
    foo        = 1.0
    bar        = 10

    ! Initialize the simple suite
    call simple_ccpp_physics_initialize("simple_suite", errmsg, errflg)
    if (errflg.ne.0) then
      write(6,*) trim(errmsg)
      stop
    end if

    sim_time = time_start
    time_loop : do while (sim_time.le.time_end)

      ! Announce a new timestep
      call simple_ccpp_physics_timestep_initial("simple_suite", errmsg, &
                                                     errflg)
      if (errflg.ne.0) then
        write(6,*) trim(errmsg)
        stop
      end if

      ! Run the simple suite
      call simple_ccpp_physics_run("simple_suite", "simple_group", errmsg, &
                                   errflg)
      if (errflg.ne.0) then
        write(6,*) trim(errmsg)
        stop
      end if

      ! Announce the end of the timestep
      call simple_ccpp_physics_timestep_final("simple_suite", errmsg, &
                                                   errflg)
      if (errflg.ne.0) then
        write(6,*) trim(errmsg)
        stop
      end if

      ! Advance the simulation time
      sim_time = sim_time + dt

    end do time_loop

    ! Finalize the simple suite
    call simple_ccpp_physics_finalize("simple_suite", errmsg, errflg)

  end subroutine simple_host_sub

end module simple_host_main

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> Simple model driver
program simple_host
  use simple_host_main
  call simple_host_sub()
end program simple_host

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
