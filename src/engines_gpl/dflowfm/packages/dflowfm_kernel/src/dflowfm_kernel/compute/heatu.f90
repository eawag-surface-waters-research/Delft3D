subroutine heatu(timhr)
use m_flow
use m_flowgeom
use m_sferic
implicit none

double precision :: timhr

double precision :: qsnom
integer          :: n, kb, kt


heatsrc0 = 0d0                                              ! array of heat sources zero

if (jamapheatflux > 0 .or. jahisheatflux > 0) then          ! map output zero
   if ( jatem.eq.3 ) then
      Qtotmap=0d0
   else if ( jatem.eq.5 ) then
      Qtotmap=0d0
      Qsunmap=0d0
      Qevamap=0d0
      Qconmap=0d0
      Qlongmap=0d0
      Qfrevamap=0d0
      Qfrconmap=0d0
   end if
endif

call qsun_nominal(anglon, anglat, timhr, qsnom) ! for models not in spherical coordinates do this just once

!epshstem     = 0.001d0
!chktempdep   = 0.0d0
!Soiltempthick = 0.2d0

!$OMP PARALLEL DO   &
!$OMP PRIVATE(n,kb,kt)
do n = 1,ndxi
   if (nd(n)%lnx == 0) cycle   ! The need for this statement makes Santa Claus unhappy
   if (hs(n) < epshstem) cycle
   call heatun(n,timhr,qsnom)
   if (hs(n) < chktempdep) then
      call getkbotktop(n,kb,kt)
      heatsrc0 (kb:kt) = heatsrc0 (kb:kt)*hs(n)/chktempdep
   endif
enddo
!$OMP END PARALLEL DO

end subroutine heatu
