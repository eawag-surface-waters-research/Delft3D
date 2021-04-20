 !> copy values that are displayed at flowlinks to samples
 subroutine copyzlintosamples()
 use m_samples
 use m_flowgeom
 use m_flow

 USE M_MISSING
 implicit none
 integer                    :: k, L
 double precision, external :: zlin

 k = 0
 do L = 1,Lnx
    if ( hu(L) .gt. epshu ) then
       k = k + 1
       call increasesam(k)
       xs(k) = xu(L)
       ys(k) = yu(L)
       zs(k) = zlin(L)
    endif
 enddo
 Ns  = k

 end subroutine copyzlintosamples
