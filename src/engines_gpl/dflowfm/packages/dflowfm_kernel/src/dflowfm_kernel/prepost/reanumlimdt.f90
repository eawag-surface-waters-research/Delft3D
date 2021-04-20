 subroutine reanumlimdt()
 use m_flowgeom
 use m_GlobalParameters, only: INDTP_ALL
 use MessageHandling, only: IdLen

 use m_flow
 use m_partitioninfo
 use m_samples
 implicit none
 character(len=IdLen) :: name, nams
 logical              :: jawel
 integer              :: mlim, k, numlimdtk, kk, jakdtree=1, jaoutside=0
 double precision     :: xdum, ydum
 integer, allocatable :: knum(:)

 if ( jampi.eq.0 ) then
    name = 'prev_numlimdt.xyz'
 else
    name = 'prev_numlimdt'//'_'//trim(sdmn)//'.xyz'
 endif
 inquire(file=name, exist = jawel)
 if (jawel) then

    call oldfil(mlim, trim(name))
    call increasesam(ndx)
    kk = 0
    do k = 1,ndx
       kk = kk + 1
       read(mlim, *, end=999) xs(kk), ys(kk), zs(kk)
    enddo
    999 continue
    call doclose(mlim)
    allocate(knum(ndx)) ; knum = 0
    kk = kk - 1
    call find_flownode(kk, xs, ys, nams, knum, jakdtree, jaoutside, INDTP_ALL)
    do k = 1,kk
       if (knum(k) > 0) then
          numlimdt(knum(k)) = zs(k)
       endif
    enddo

    deallocate(xs,ys,zs,knum)
 endif
 end subroutine reanumlimdt
