!> \file
!> Data module for the simple host model
module simple_mod

  use ccpp_kinds,            only : kind_phys
  use const_props_mod,       only : const_props_type

  implicit none
  public

  real(kind_phys) :: foo
  real(kind_phys) :: dt
  real(kind_phys) :: time_start
  real(kind_phys) :: time_end

  integer         :: bar

end module simple_mod
