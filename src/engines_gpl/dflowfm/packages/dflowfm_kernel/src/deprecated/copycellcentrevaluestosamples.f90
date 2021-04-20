 subroutine copycellcentrevaluestosamples()
 use m_samples
 use m_flowgeom

 implicit none

 integer                    :: k, n
 double precision, external :: znod

 k = 0
 do n = 1,ndx
       k = k + 1
       call increasesam(k)
       xs(k) = xz(n) ; ys(k) = yz(n) ; zs(k) = znod(n)
 enddo
 ns  = k

 end subroutine
