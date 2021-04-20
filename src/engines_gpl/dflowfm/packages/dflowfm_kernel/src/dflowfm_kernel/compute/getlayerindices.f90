 !> Gets the local layer numbers for a given grid cell.
 !! Note: works both for sigma and z, but for sigma, the return values are trivial: nlayb==1, nrlay==kmx.
 subroutine getlayerindices(n,nlayb,nrlay)
 use m_flowgeom
 use m_flow
 use m_missing
 implicit none

 integer, intent(in   ) :: n     !< Flow node/grid cell number
 integer, intent(  out) :: nlayb !< Layer number for the bottom layer (in 1:kmx)
 integer, intent(  out) :: nrlay !< Nr. of active layers for this flow node.

 integer          :: Ltn


 Ltn = laydefnr(n)
 if (laytyp(Ltn) == 1) then ! sigma
    nlayb = 1          ! Bottom layers always the first
    nrlay = laymx(Ltn) ! Sigma: always all layers
 else                       ! z-layers
    call getzlayerindices(n, nlayb, nrlay)
 end if

 end subroutine getlayerindices
