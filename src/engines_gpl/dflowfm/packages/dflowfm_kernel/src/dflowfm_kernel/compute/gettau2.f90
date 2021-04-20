subroutine gettau2(n,taucurc,czc,ustw2)
use m_flowgeom
use m_flow
use m_waves


implicit none
integer, intent(in)   :: n               !< Flow node number
double precision, intent(out) :: taucurc !< Bed shear stress from current or current plus wave
double precision, intent(out) :: czc     !< Chezy at flow node (taucurrent)
double precision, intent(out) :: ustw2   !< Ustarwave Swart (if Jawaveswartdelwaq == 1)


!
!           Local variables
!
integer :: LL, nn                            !< Local link counters
double precision ::  cf, cfn, cz, frcn, ar,  wa, ust, ust2, fw    !< Local intermediate variables

taucurc = 0d0 ; ustw2 = 0d0
czc = 0d0
cfn = 0d0
wa  = 0d0
ust = 0d0

do nn = 1,nd(n)%lnx
   LL = abs( nd(n)%ln(nn) )
   frcn = frcu(LL)
   if (frcn > 0 .and. hu(LL) > 0) then
      call getcz(hu(LL), frcn, ifrcutp(LL), cz,LL)
      cf  = ag/(cz*cz)
      ar  = au(LL)*dx(LL)
      wa  = wa + ar       ! area  weigthed
      cfn = cfn + cf*ar
      if (kmx > 0) then
         if (jawaveswartdelwaq <= 1) then
            ust = ust + ustb(LL)*ar
         else
            ust = ust + taubxu(LL)*ar
         endif
      endif
   endif
enddo
if (wa > 0) then
   cfn = cfn / wa
   ust = ust / wa
endif
if (cfn > 0) then
   czc = sqrt(ag/cfn)
endif

ust2 = 0d0
if (kmx == 0) then
    ust2 = cfn*(ucx(n)*ucx(n) + ucy(n)*ucy(n))
else
    ust2 = ust*ust
endif

if (jawaveswartdelwaq == 0) then
   taucurc = rhomean*ust2
else if (jawaveSwartDelwaq == 1) then
   if (twav(n) > 1d-2) then
      call Swart(Twav(n), uorb(n), z0wav, fw, ustw2)
      ust2  = ust2 + ustw2                    ! Swart
   endif
   taucurc = rhomean*ust2
else if (jawaveSwartDelwaq == 2) then
   taucurc = ust                              ! area averaged taubxu
endif

end subroutine gettau2
