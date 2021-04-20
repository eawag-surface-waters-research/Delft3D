subroutine foresterpoint(temp, vol, a, d, km, kmxx, maxit, ip)
use m_flow, only  : eps6, eps10
implicit none

double precision :: temp(kmxx), vol(kmxx), a(km), d(km)
integer          :: km, kmxx, maxit, ip

double precision :: dif
integer          :: k, m, n, ja

a(1:km) = temp(1:km)

do m = 1, maxit

   d(1:km) = a(1:km)
   ja      = 0

   do k = 1, km - 1
      dif = d(k+1) - d(k)
      if (dif*ip > eps6 .or. d(k) < 0d0 .or. d(k+1) < 0d0 ) then
         if ( vol(k) > eps10 .and. vol(k+1) > eps10 ) then
             ja     = 1
             dif    = 0.1666666666667d0*dif*(vol(k+1) + vol(k))
             a(k)   = a(k)   + dif / vol(k)
             a(k+1) = a(k+1) - dif / vol(k+1)
         else
             dif = 0d0
         endif
      endif
   enddo

   if (ja == 0) then
       exit
   endif

enddo

temp(1:km) = a(1:km)

if (kmxx > km) then
   temp(km+1:kmxx) = temp(km)
endif

   end subroutine foresterpoint
