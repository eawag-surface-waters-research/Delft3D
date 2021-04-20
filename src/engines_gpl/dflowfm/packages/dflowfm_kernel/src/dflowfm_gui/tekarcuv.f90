 subroutine tekarcuv(vfac,met)
 use M_arcuv
 implicit none
 double precision :: vfac
 integer          :: met

 integer          :: mx, nx, i, j

 mx = size(arcuv,2)
 nx = size(arcuv,3)
 do i = 1,mx
    do j = 1,nx
       call setcol(221)
       if (met == 6) then
          call arrowsxy( arcuv(1,i,j) , arcuv(2,i,j), arcuv(3,i,j) , arcuv(4,i,j), 50*VFAC)
       else
          call htext(arcuv(3,i,j), arcuv(1,i,j) , arcuv(2,i,j) )
       endif
    enddo
 enddo
 end subroutine tekarcuv
