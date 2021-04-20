subroutine einstein_garcia(da,rs,dj1,dj2)
use m_einstein_garcia
implicit none
double precision :: da,rs, dj1, dj2

double precision :: aa, cck, rsk, dj12, dj22
integer          :: i1, i2,k

if      (da < 0.001d0) then
   i1 = 1; i2  = 1
else if (da < 0.005d0) then
   i1 = 1; i2  = 2
else if (da < 0.01d0) then
   i1 = 2; i2  = 3
else if (da < 0.05d0) then
   i1 = 3; i2  = 4
else if (da < 0.1d0) then
   i1 = 4; i2  = 5
else
   i1 = 5; i2  = 5
endif
if (i1 == i2) then
    aa = 0d0
else
    aa = ( da - d(i1) ) / ( d(i2) - d(i1) )
endif

dj1   = 0d0
dj2   = 0d0
dj12  = 0d0
dj22  = 0d0

do k  = 0,6
  rsk = rs**k
  !cck = (1d0-aa)*c1(i1,k) + aa*c1(i2,k)
  !dj1 = dj1 + cck*rsk
  !cck = (1d0-aa)*c2(i1,k) + aa*c2(i2,k)
  !dj2 = dj2 + cck*rsk

  dj1  = dj1  + c1(i1,k)*rsk
  dj12 = dj12 + c1(i2,k)*rsk
  dj2  = dj2  + c2(i1,k)*rsk
  dj22 = dj22 + c2(i2,k)*rsk

enddo

dj1 = (1d0-aa)*dj1 + aa*dj12
dj2 = (1d0-aa)*dj2 + aa*dj22

if (dj1 .ne. 0d0) then
    dj1 = 1d0/dj1
endif


if (dj2 .ne. 0d0) then
    dj2 = -1d0/dj2
endif


end subroutine einstein_garcia
