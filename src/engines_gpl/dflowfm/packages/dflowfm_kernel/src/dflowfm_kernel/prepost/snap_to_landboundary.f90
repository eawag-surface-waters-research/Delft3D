!> snap netnodes to land boundary segment
subroutine snap_to_landboundary()
   use m_netw
   use m_landboundary

   implicit none

   double precision :: xn, yn, ddis, rL

   integer          :: k, numlanseg, jstart, jend, j, MXLAN_sav

!  save MXLAN
   MXLAN_sav = MXLAN

!  set MXLAN to actual value
   MXLAN = MXLAN_loc

!   if ( jasnap.ne.2 .and. jasnap.ne.3 ) return

   do k=1,numk
      if ( nb(k).eq.1 .or. nb(k).eq.2 .or. nb(k).eq.3 ) then
         numlanseg = lanseg_map(k)
         if ( numlanseg.lt.1 ) cycle
         jstart    = lanseg_startend(1,numlanseg)
         jend      = lanseg_startend(2,numlanseg)
         call toland(xk(k),yk(k),jstart,jend,0,xn,yn,ddis,j,rL)
         xk(k) = xn
         yk(k) = yn
      end if
   end do

!  restore MXLAN
   MXLAN = MXLAN_sav

   return
end subroutine snap_to_landboundary
