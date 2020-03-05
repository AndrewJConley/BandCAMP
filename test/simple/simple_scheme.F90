!> \file
!> Simple scheme for testing CPF usage
module simple_scheme

  use ccpp_kinds,            only : kind_phys

  implicit none

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> \section arg_table_simple_scheme_init Argument Table
  !! \htmlinclude chemistry_driver_init.html
  !!
  subroutine simple_scheme_init(foo, bar, errmsg, errflg)

    real(kind_phys), intent(in)     :: foo
    integer, intent(in)             :: bar
    character(len=512), intent(out) :: errmsg
    integer, intent(out)            :: errflg

    write(*,*) "Initialized!", foo, bar

    errmsg = ''
    errflg = 0

  end subroutine simple_scheme_init

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> \section arg_table_simple_scheme_run Argument Table
  !! \htmlinclude chemistry_driver_run.html
  !!
  subroutine simple_scheme_run(foo, bar, errmsg, errflg)

    real(kind_phys), intent(in)     :: foo
    integer, intent(inout)          :: bar
    character(len=512), intent(out) :: errmsg
    integer, intent(out)            :: errflg

    write(*,*) "Running!", foo, bar
    bar = bar + 1

    errmsg = ''
    errflg = 0

  end subroutine simple_scheme_run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> \section arg_table_simple_scheme_finalize Argument Table
  !! \htmlinclude chemistry_driver_finalize.html
  !!
  subroutine simple_scheme_finalize(errmsg, errflg)

    character(len=512), intent(out) :: errmsg
    integer, intent(out)            :: errflg

    write(*,*) "That's all folks!"

    errmsg = ''
    errflg = 0

  end subroutine simple_scheme_finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module simple_scheme
