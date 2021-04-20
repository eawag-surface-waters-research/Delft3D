 !> Gets the local layer numbers for a given grid cell.
 subroutine getzlayerindices(n,nlayb,nrlay)
 use m_flowgeom
 use m_flow
 use m_missing
 implicit none

 integer, intent(in   ) :: n     !< Flow node/grid cell number
 integer, intent(  out) :: nlayb !< Layer number for the bottom layer (in 1:kmx)
 integer, intent(  out) :: nrlay !< Nr. of active layers for this flow node.

 integer          :: j,j1,j3,k, Ltn, mx ! layerdistribution indexes


 Ltn = laydefnr(n)
 mx  = laymx(Ltn)
 nlayb = mx ; nrlay = 1 ! default
! if (nlaybn(n) == 0) then
    do k = 1,mx
       if (numtopsig > 0 .and. janumtopsiguniform ==1) then
          if ( zslay(k,Ltn) > bl(n) .or. mx-k+1 <= numtopsig ) then
              nlayb = k
              nrlay = mx - k + 1
              exit
          endif
       else
       if ( zslay(k,Ltn) > bl(n) ) then
           nlayb = k
           nrlay = mx - k + 1
           exit
       endif
      endif
   enddo
!    nlayb = nlaybn(n)
!    nrlay = nrlayn(n)

! else
!    nlayb = nlaybn(n)
!    nrlay = nrlayn(n)
! endif

 end subroutine getzlayerindices
