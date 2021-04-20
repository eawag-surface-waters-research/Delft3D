 subroutine tekbathy(ja)
 use unstruc_display
 use m_flowgeom
 use m_flow
 use gridoperations
 implicit none
 integer :: nodemode, nodewhat,ndraw
 integer :: k, ja, nn, ncol
 double precision :: znod, zn
 common /drawthis/ ndraw(50)
 logical inview

 if (ndraw(39) == 0) return

 nodewhat  = ndraw(28)
 ndraw(28) = 3

 do k = 1,ndxi
    if (mod(k,200) == 0) then
       call halt(ja)
       if (ja == 1) then
          ndraw(28) = nodewhat
          return
       endif
    endif

    if (inview( xz(k), yz(k) ) ) then
       zn = znod(k)
       call isocol2(zn,ncol)
       nn = size( nd(k)%x )
       call PFILLER(nd(k)%x, nd(k)%y, nn,NCOL,NCol)
    endif
 enddo

 ndraw(28) = nodewhat
 end subroutine tekbathy
