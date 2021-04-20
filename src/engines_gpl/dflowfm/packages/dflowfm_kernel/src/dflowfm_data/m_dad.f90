module m_dad
   use dredge_data_module, only: dredge_type
!
! dredging related
!
   logical                           :: dad_included  !< Include dredging and dumping
   type(dredge_type), target         :: dadpar        !< Dredging related parameters

end module m_dad
