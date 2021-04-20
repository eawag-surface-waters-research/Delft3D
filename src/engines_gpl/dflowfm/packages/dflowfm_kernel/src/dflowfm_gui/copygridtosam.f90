!> Copy curvilinear grid to samples
 subroutine copygridtosam()
 use m_samples
 use m_grid
 USE M_MISSING
 implicit none
 integer :: in, k, m, n
 in = -1
 k  = MC*NC

 CALL INCREASESAM(k)

 K = 0
 MXSAM = MC
 MYSAM = NC
 xs = DMISS
 ys = DMISS
 zs = DMISS
 IPSTAT = IPSTAT_NOTOK
 do n=1,NC
    do m=1,MC
       k = k + 1
       if ( xc(m,n).ne.DMISS .and. yc(m,n).ne.DMISS ) then
         xs(k) = xc(m,n)
         ys(k) = yc(m,n)
         zs(k) = zc(m,n)
       end if
    end do
 enddo
 ns = k

 end subroutine copygridtosam
