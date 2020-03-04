!> \file
!> Mock ccpp host model for tests
program simple_host_main

  use ccpp_kinds,            only : kind_phys
  use simple_mod,            only : foo, bar, dt, time_start, time_end
  use simple_ccpp_cap,       only : simple_ccpp_simple_group_initialize
  use simple_ccpp_cap,       only : simple_ccpp_simple_group_timestep_initial
  use simple_ccpp_cap,       only : simple_ccpp_simple_group_run
  use simple_ccpp_cap,       only : simple_ccpp_simple_group_timestep_final
  use simple_ccpp_cap,       only : simple_ccpp_simple_group_finalize
  use simple_ccpp_cap,       only : ccpp_simple_group_suite_list
  use simple_ccpp_cap,       only : ccpp_simple_group_suite_part_list

  implicit none
  private

  public simple_host_sub

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Run the simple host model
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
    call simple_ccpp_simple_group_initialize("simple_suite", errmsg, errflg)
    if (errflg.ne.0) then
      write(6,*) trim(errmsg)
      stop
    end if

    sim_time = time_start
    time_loop : do while (sim_time.le.time_end)

      ! Announce a new timestep
      call simple_ccpp_simple_group_timestep_initial("simple_suite", errmsg, &
                                                     errflg)
      if (errflg.ne.0) then
        write(6,*) trim(errmsg)
        stop
      end if

      ! Run the simple suite
      call simple_ccpp_simple_group_run("simple_suite", errmsg, errflg)
      if (errflg.ne.0) then
        write(6,*) trim(errmsg)
        stop
      end if

      ! Announce the end of the timestep
      call simple_ccpp_simple_group_timestep_final("simple_suite", errmsg, &
                                                   errflg)
      if (errflg.ne.0) then
        write(6,*) trim(errmsg)
        stop
      end if

      ! Advance the simulation time
      sim_time = sim_time + dt

    end do time_loop

    ! Finalize the simple suite
    call simple_suite_ccpp_simple_group_finalize("simple_suite", errmsg, &
                                                 errflg)

  end subroutine simple_host_main

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> Mock host model driver
end module simple_host_main

  use simple_host_main
  call simple_host_sub()

program simple_host

end program simple_host
