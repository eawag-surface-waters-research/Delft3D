subroutine doforester()
use m_flow    ,   only : sa1, vol1, ndkx, kbot, ktop, kmxn, ndkx, maxitverticalforestersal, maxitverticalforestertem
use m_flowgeom,   only : ndx, ndxi
use m_turbulence, only : kmxx
use m_transport,  only : constituents, numconst, itemp
use timers

implicit none

integer          :: kk, km, kb
double precision :: a(kmxx), d(kmxx)

integer(4) ithndl /0/
if (timon) call timstrt ( "doforester", ithndl )

do kk = 1,ndxi
   km = ktop(kk) - kbot(kk) + 1
   if (maxitverticalforestersal > 0) then
      call foresterpoint(sa1(kbot(kk):), vol1(kbot(kk):), a, d, km, kmxn(kk), maxitverticalforestersal, 1) ! foresterpoint
   endif
   if (maxitverticalforestertem > 0) then
      call foresterpoint2(constituents, numconst, ndkx, itemp, vol1(kb:), a, d, km, kmxn(kk), kb, maxitverticalforestertem, -1)
   endif
enddo

if (timon) call timstop( ithndl )
end subroutine doforester
