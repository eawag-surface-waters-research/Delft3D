subroutine getfetch(k,U10,FetchL,FetchD) !and windspeed
use m_flow,  only:  Hs, Wx, Wy
use m_waves, only:  fetch, nwf, fetdp
use m_sferic
implicit none

integer          :: k
double precision :: U10,FetchL,FetchD

integer          :: L,nw1,nw2
double precision :: alfa1, alfa2, dir


FetchL = 0d0 ; FetchD = 0d0

if (Hs(k) > 0d0) then
   call getlink1(k,L) ! het is maar voor wind
   U10 = sqrt ( WX(L)*WX(L) + WY(L)*WY(L) )
   IF (U10 .LT. 1d0) return

   DIR   = ATAN2(WY(L), WX(L))
   IF (DIR < 0D0) DIR = DIR + TWOPI

   dir = dir/twopi
   if (dir >= 1d0) dir = 0d0
   NW1 = DIR*(nwf-1) + 1
   NW2 = NW1 + 1

   if (fetch(nw1,k) > 0d0 .or. fetch(nw2,k) > 0d0 ) then
      alfa2  = (nwf-1)*( dir - dble(nw1-1) / dble(nwf-1) )
      alfa1  = 1d0 - alfa2
      fetchL = alfa1*fetch(nw1,k) + alfa2*fetch(nw2,k)
      fetchD = alfa1*fetdp(nw1,k) + alfa2*fetdp(nw2,k)
   endif
endif
end subroutine getfetch
