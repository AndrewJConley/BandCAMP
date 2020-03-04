!> \file
!> Simple scheme for testing CPF usage
module simple_scheme

  use ccpp_kinds,            only : kind_phys

  implicit none

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialization
  !!
  subroutine simple_scheme_init(foo, bar, errmsg, errflag)

    real(kind_phys), intent(in)     :: foo
    integer, intent(in)             :: bar
    character(len=512), intent(out) :: errmsg
    integer, intent(out)            :: errflg

    write(*,*) "Initialized!", foo, bar

    errmsg = ''
    errflg = 0

  end subroutine simple_scheme_init

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Run
  !!
  subroutine simple_scheme_run(foo, bar, errmsg, errflag)

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

  !> Finalize
  !!
  subroutine simple_scheme_finalize(errmsg, errflg)

    character(len=512), intent(out) :: errmsg
    integer, intent(out)            :: errflg

    errmsg = ''
    errflg = 0

  end subroutine simple_scheme_finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module simple_scheme
