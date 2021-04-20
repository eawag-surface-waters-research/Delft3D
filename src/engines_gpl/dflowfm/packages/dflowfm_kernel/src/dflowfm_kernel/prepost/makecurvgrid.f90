!> generate the curvi-grid from the node indices (ic,jc)
subroutine makecurvgrid(ic, jc)

 use m_netw
 use m_grid
 use m_missing
 use m_alloc

 implicit none

 integer, dimension(1:numk) :: ic, jc            !< indices (i,j) of the nodes

 integer                    :: imin, jmin        ! minimum of the indices

 integer                    :: i, j, node

! integer, parameter         :: IMISS = -999999

!---------------------------------------------------------
! compute grid sizes and renumber
!---------------------------------------------------------
 imin  = minval(ic, ic.ne.IMISS)
 jmin  = minval(jc, jc.ne.IMISS)

 ic    = ic   - imin + 1
 jc    = jc   - jmin + 1

 mc = maxval(ic)
 nc = maxval(jc)

!---------------------------------------------------------
! allocate and initialize arrays
!---------------------------------------------------------
 call increasegrid(mc, nc)
 xc = dmiss
 yc = dmiss
 zc = dmiss

!---------------------------------------------------------
! compose the grid
!---------------------------------------------------------
 do node=1,numk
    i = ic(node)
    j = jc(node)
    if ( i.gt.0 ) then
       xc(i,j) = xk(node)
       yc(i,j) = yk(node)
       zc(i,j) = zk(node)
    end if
 end do

end subroutine makecurvgrid
