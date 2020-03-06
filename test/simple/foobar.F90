!> \file
!> Definition and functions of the foobar_t type
module foobar

  use ccpp_kinds,            only : kind_phys

  implicit none
  private

  public :: foobar_t

  !> \section arg_table_foobar_t
  !! \htmlinclude foobar_t.html
  !!
  type :: foobar_t
    private
    real(kind=kind_phys) :: foo = 1.0
    integer              :: bar = 1
  contains
    procedure :: do_foobar => foobar_do_foobar
    procedure :: to_string => foobar_to_string
  end type foobar_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Do foobar
  subroutine foobar_do_foobar(this, bar)

    class(foobar_t), intent(inout) :: this
    integer, intent(in)            :: bar

    this%foo = this%foo + 3.0
    this%bar = bar

  end subroutine foobar_do_foobar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Output to a string
  function foobar_to_string(this) result(string_out)

    character(len=50) :: string_out
    class(foobar_t), intent(in) :: this

    write(string_out, '(g30.20,x,i3)') this%foo, this%bar

  end function foobar_to_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module foobar
