subroutine compareanalytic(s,u,x,mmax)

use m_flowgeom
use m_flow

implicit none
integer :: mmax
double precision :: s(0:mmax),u(0:mmax),x(0:mmax)
double precision :: alf, dif, si, aa
integer          :: n, i, ii
logical inview

call statisticsnewstep()

call setcol(221)
do n = 1,ndx

   if (.not. inview( xz(n), yz(n) ) ) cycle

   i = 0
   do ii = 1, mmax-1
      if ( x(ii) <= xz(n) .and. xz(n) < x(ii+1) ) then
         i = ii
         exit
      endif
   enddo
   !i = (xz(n) + 0.5*dxw) / dxw
   if  ( i > 2 .and. i < mmax-1 ) then
       alf = (xz(n) - x(i) ) / ( x(i+1) - x(i) )
       if (alf < 0d0 .or. alf > 1d0) then
           si  = 0
       else
           si  = (1-alf)*s(i) + alf*s(i+1)
           dif = abs(s1(n) - si)
           call statisticsonemorepoint(dif)
        !   call ptabs(xz(n), bl(n) + 100d0*dif)
       endif
   endif
enddo
call statisticsfinalise()

end subroutine compareanalytic
