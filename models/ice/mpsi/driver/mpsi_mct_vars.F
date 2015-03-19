module mpsi_mct_vars
  
  use seq_flds_mod
  use mct_mod

  implicit none

  save
  public                               ! By default make data private

  integer :: MPSI_MCT_ICEID
  type(mct_gsMap), pointer :: MPSI_MCT_gsMap_i
  type(mct_gGrid), pointer :: MPSI_MCT_dom_i
 
end module mpsi_mct_vars
