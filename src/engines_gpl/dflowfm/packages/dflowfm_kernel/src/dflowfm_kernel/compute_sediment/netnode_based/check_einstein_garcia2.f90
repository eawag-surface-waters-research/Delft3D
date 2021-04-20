subroutine check_einstein_garcia2(aref,h,z0,rs,ein)
use m_sediment, only : numintverticaleinstein

implicit none
double precision :: aref,h,z0,rs,ein, ucrouse1, ucrouse2, dz, g,d1,di,d, z1, z2

double precision :: a,b,y1,y2,zl,zm,zlm,zlm2,alfa

integer :: n, k

ein = 0d0
n   = numintverticaleinstein
g   = 1.1
d1  = (h-aref)*(1-g)/(1-g**n)
di  = d1

!d   = 0d0 ! just checking
!do k = 1,n
!   d  = d + di
!   di = g*di
!enddo

!di       = d1
z2       = aref
y1       = z2/z0
zL       = log(y1)

!y2       = h/z0
!zm       = log(y2)
!alfa     = 1d0/6d0
!a        = (zL-zM) / (y1**alfa - y2**alfa)
!b        =  zL - a*y1**alfa

ucrouse2 = zl *  ( (h-z2)/z2 ) ** rs

do k = 1,n
   z1 = z2
   z2 = z1 + di
   ucrouse1 = ucrouse2
   ucrouse2 = 0
   if (k < n) then
      zlm      = log(z2/z0)

  !   zlm      = a*(z2/z0)**alfa + b

      ucrouse2 = zlm *  ( (h-z2)/z2 ) ** rs
   endif
   ein = ein + 0.5d0*(ucrouse1+ucrouse2)*di
   di  = g*di
enddo
ein = ein * (aref/(h-aref) )** rs
end subroutine check_einstein_garcia2
