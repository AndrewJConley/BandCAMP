!> \file
!> Data module for the simple host model
module simple_mod

  use ccpp_kinds,            only : kind_phys
  use foobar,                only : foobar_t

  implicit none
  public

  !> \section arg_table_simple_mod Argument Table
  !! \htmlinclude arg_table_simple_host.html
  !!
  real(kind_phys) :: foo
  real(kind_phys) :: dt
  real(kind_phys) :: time_start
  real(kind_phys) :: time_end

  integer         :: bar

  type(foobar_t)  :: foobar

end module simple_mod
