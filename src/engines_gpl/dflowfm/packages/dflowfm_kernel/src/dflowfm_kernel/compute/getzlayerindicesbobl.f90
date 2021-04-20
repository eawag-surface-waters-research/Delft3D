 subroutine getzlayerindicesbobL(n,nlayb,nrlay,bobL)
 use m_flowgeom
 use m_flow
 use m_missing
 implicit none

 integer          :: n,nlayb, nrlay
 integer          :: j,j1,j3,k, Ltn, mx ! layerdistribution indexes
 double precision :: bobL

 Ltn = laydefnr(n)
 mx  = laymx(Ltn)
 nlayb = mx ; nrlay = 1 ! default
 do k = 1,mx
    if ( zslay(k,Ltn) > bobL ) then
        nlayb = k
        nrlay = mx - k + 1
        exit
    endif
 enddo

 end subroutine getzlayerindicesbobL
