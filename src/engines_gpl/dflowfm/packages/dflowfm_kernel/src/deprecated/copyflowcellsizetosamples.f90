 subroutine copyflowcellsizetosamples()
 use m_samples
 use m_netw
 use m_flowgeom
 USE M_MISSING
 implicit none
 integer :: k, n
 call flow_geominit(0)

 k = ns
 do n = 1,ndx
    k = k + 1
    call increasesam(k)
    xs(k) = xz(n) ; ys(k) = yz(n) ; zs(k) = sqrt(ba(n))
 enddo
 ns  = k
 ndx = 0
 end subroutine copyflowcellsizetosamples
