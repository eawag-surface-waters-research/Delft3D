!> Copy the original polygons that define the current fixed weirs
!! to the active polygons in xpl,...
subroutine copyFixedWeirsToPol()
use m_fixedweirs
use m_polygon
implicit none
integer :: i, ip

npl    = 0
do i=1,nfxw
    call appendCRSPathToPol(fxw(i))
end do

end subroutine copyFixedWeirsToPol
