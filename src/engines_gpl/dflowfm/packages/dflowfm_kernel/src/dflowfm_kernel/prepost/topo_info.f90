!> link-based mesh-topology information
double precision function topo_info(L)
   use m_netw
   use m_landboundary
   use m_missing

   implicit none

   integer :: L   !< link number

   integer                  :: k1, k2, kL, kR
   integer                  :: icellL, icellR
   integer                  :: k, n

   integer                  :: jalandbound     ! take landboundary into account (1) or not (0)

   logical                  :: Lproceed

   integer, external        :: nmk_opt         ! optimal nmk for the for nodes involved

!  default
   topo_info = DMISS

!  check if administration is in order
   if ( L.gt.ubound(lnn,1) ) goto 1234

!  check if the landboundary can be taken into account (not necessarily the up-to-date)
   if ( ubound(lanseg_map,1).ge.numk ) then
      jalandbound = 1
   else
      jalandbound = 0
   end if

   call comp_ntopo(L, jalandbound, k1, k2, kL, kR, icellL, icellR, n)

   topo_info = -dble(n)

   if ( topo_info.le.0d0 ) topo_info=DMISS

   return

1234 continue  ! error handling
   return

end function topo_info
