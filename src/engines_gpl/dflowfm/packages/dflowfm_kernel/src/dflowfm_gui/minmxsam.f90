 subroutine minmxsam()

 use m_samples
 use m_missing
 use m_isoscaleunit

 implicit none

 double precision :: rmin, rmax
 double precision :: VMAX,VMIN,DV,VAL(256)
 integer :: NCOLS(256),NIS,NIE,nv,JAAUTO
 character(len=256) :: buffer
 common /depmax2/ vmax,vmin,dv,val,ncols,nv,nis,nie,jaauto
 integer :: k, i
 logical inview

 if (jaauto > 0) then
    rmin =  1d30
    rmax = -1d30

    do k = 1,ns
       if ( zs(k)==DMISS ) cycle
       if ( inview( xs(k), ys(k) ) ) then
           if (zs(k) < rmin) then
               rmin  = zs(k)
           endif
           if (zs(k) > rmax) then
               rmax  = zs(k)
           endif
       endif
    enddo
    vmax = rmax
    vmin = rmin
 endif

 dv   = vmax - vmin
 do i = 1,nv
    val(i) = vmin + (i-1)*dv/(nv-1)
 enddo

 !Samples have the same unit of the displayed values
 write(buffer, '(a,a)') 'Samples                              ',UNIT(1)
 CALL PARAMTEXT(buffer, 2 )

 end subroutine minmxsam
