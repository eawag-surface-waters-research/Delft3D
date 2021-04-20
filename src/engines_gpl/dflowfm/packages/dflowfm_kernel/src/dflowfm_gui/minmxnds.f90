 subroutine minmxnds()
 use unstruc_display                                                    ! bepaal minimum en maximum van znod in viewing area
 use m_flowgeom
 use m_flow
 use m_missing

 implicit none
 integer :: i
 double precision :: rmin, rmax
 double precision, external :: znod
 double precision :: zn
 integer          :: n, ja2, ndraw
 double precision :: VMAX,VMIN,DV,VAL(256)
 integer :: NCOLS(256),NIS,NIE,nv,JAAUTO
 common /depmax/ vmax,vmin,dv,val,ncols,nv,nis,nie,jaauto
 COMMON /DRAWTHIS/ ndraw(50)
 logical inview

 if (jaauto > 0) then
    rmin =  1d30; ndmin = 0
    rmax = -1d30; ndmax = 0


    do n = 1,ndx
       ja2 = 1
       if (wetplot > 0d0) then
          if (hs(n) < wetplot) then
             ja2 = 0
          endif
       endif
       if ( ja2 == 1 .or. ndraw(28) == 3) then        ! crash
          if ( inview( xz(n), yz(n) ) ) then
             zn = znod(n)
             if ( zn.eq.DMISS ) cycle
             if (zn < rmin) then
                 rmin = zn ; ndmin = n
             endif
             if (zn > rmax) then
                 rmax = zn ; ndmax = n
              endif
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

 end subroutine minmxnds
