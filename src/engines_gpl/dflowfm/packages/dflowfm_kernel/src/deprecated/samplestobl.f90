 subroutine samplestobl(key)
 use m_samples
 use m_flowgeom
 use m_GlobalParameters, only: INDTP_ALL
 use m_flow
 implicit none

 integer :: key

 integer :: k,n

 do n = 1,ns
    call inflowcell(xs(n), ys(n),k,1, INDTP_ALL)
    if (k > 0) then
       bl(k) = zs(n)
    endif
 enddo

 call setbobs()

 end subroutine samplestobl
