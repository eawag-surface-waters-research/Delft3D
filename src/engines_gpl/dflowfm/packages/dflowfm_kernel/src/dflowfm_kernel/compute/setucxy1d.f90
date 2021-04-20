! =================================================================================================
! =================================================================================================
subroutine setucxy1D() ! give ucx,ucy magnitude of uc1D, Pure1D

use m_flowgeom
use m_flow

implicit none
integer          :: n,LL,k2
double precision :: uxy

do n = 1,ndx
   if (uc1D(n) .ne. 0) then
      uxy    = sqrt( ucx(n)*ucx(n) + ucy(n)*ucy(n) )
      if (uxy > 0) then
          uxy    = abs(uc1D(n))/uxy
          ucx(n) = ucx(n)*uxy
          ucy(n) = ucy(n)*uxy
      endif
   endif
enddo

do LL = lnxi+1,lnx          ! bnd
    if (kcu(LL) == -1) then  ! 1D type link
        n = Ln(1,LL) ; k2 = Ln(2,LL)
        if (uc1D(k2) .ne. 0) then
            uxy    = sqrt( ucx(n)*ucx(n) + ucy(n)*ucy(n) )
            if (uxy > 0) then
                uxy    = abs(uc1D(n))/uxy
                ucx(n) = ucx(n)*uxy
                ucy(n) = ucy(n)*uxy
            endif
        endif
    endif
 enddo

 end subroutine setucxy1D
