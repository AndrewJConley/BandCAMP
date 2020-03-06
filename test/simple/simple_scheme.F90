!> \file
!> Simple scheme for testing CPF usage
module simple_scheme

  use ccpp_kinds,            only : kind_phys
  use foobar,                only : foobar_t

  implicit none

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> \section arg_table_simple_scheme_init Argument Table
  !! \htmlinclude simple_scheme_init.html
  !!
  subroutine simple_scheme_init(foo, bar, foobar, errmsg, errflg)

    real(kind_phys), intent(in)     :: foo
    integer, intent(in)             :: bar
    type(foobar_t), intent(in)      :: foobar
    character(len=512), intent(out) :: errmsg
    integer, intent(out)            :: errflg

    write(*,*) "Initialized!", foo, bar, trim(foobar%to_string())

    errmsg = ''
    errflg = 0

  end subroutine simple_scheme_init

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> \section arg_table_simple_scheme_run Argument Table
  !! \htmlinclude simple_scheme_run.html
  !!
  subroutine simple_scheme_run(foo, bar, foobar, errmsg, errflg)

    real(kind_phys), intent(in)     :: foo
    integer, intent(inout)          :: bar
    type(foobar_t), intent(inout)   :: foobar
    character(len=512), intent(out) :: errmsg
    integer, intent(out)            :: errflg

    write(*,*) "Running!", foo, bar, trim(foobar%to_string())
    bar = bar + 1
    call foobar%do_foobar(bar)

    errmsg = ''
    errflg = 0

  end subroutine simple_scheme_run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> \section arg_table_simple_scheme_finalize Argument Table
  !! \htmlinclude simple_scheme_finalize.html
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
