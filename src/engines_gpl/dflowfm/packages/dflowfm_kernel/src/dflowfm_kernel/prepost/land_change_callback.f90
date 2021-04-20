!> Updates derived bathymetry arrays, after zk values have been changed.
!! This routine updates the entire grid, so call this once after a (series of) zk update(s).
subroutine land_change_callback()
   use m_flowgeom
   use m_flow
   implicit none

   hs = s1 - bl
   call setbobs()
   s1 = bl + hs; s0=s1; s00 = s1

   call volsur()           ! dropland_zk
   call flow_f0isf1()      ! dropland_zk
   volerr = 0; volerrcum = 0

   if (kmx > 0) then
      call setkbotktop(1) ! dropland_zk
   endif
end subroutine land_change_callback
