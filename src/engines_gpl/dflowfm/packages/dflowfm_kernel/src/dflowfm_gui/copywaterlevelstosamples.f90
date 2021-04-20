 subroutine copywaterlevelstosamples()
 use m_samples
 use m_flowgeom
 use m_flow
 use unstruc_display, only: wetplot

 USE M_MISSING
 implicit none
 integer                    :: k, n
 double precision, external :: znod

 k = 0
 do n = 1,ndx
    if ( hs(n) >= wetplot ) then
       k = k + 1
       call increasesam(k)
       xs(k) = xz(n) ; ys(k) = yz(n) ; zs(k) = znod(n)
    endif
 enddo
 ns  = k
 end subroutine copywaterlevelstosamples
