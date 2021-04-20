 !> Gets the local layer numbers for a given flow link.
 !! Always uses the maximum possible layer range instead of the actual range.
 !! Note: works both for sigma and z, but for sigma, the return values are trivial: nlaybL==1, nrlayLx==kmx.
 subroutine getlayerindicesLmax(L, nlaybL, nrlayLx)
 use m_flow
 implicit none

 integer, intent(in)  :: L       !< Flow link (L \in [1,lnx] )
 integer, intent(out) :: nlaybL  !< Layer number for the bottom layer (in 1:kmxL(L))
 integer, intent(out) :: nrlayLx !< Max nr. of active layers for this flow link.

 nrlayLx = kmxL(L)
 nlaybL = kmx - nrlayLx + 1

 end subroutine getlayerindicesLmax
