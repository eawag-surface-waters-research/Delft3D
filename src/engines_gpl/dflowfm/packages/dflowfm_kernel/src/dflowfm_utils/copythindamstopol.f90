!> Copy the original polygons that define the current thin dams
!! to the active polygons in xpl,...
subroutine copyThinDamsToPol()
use m_thindams
use m_polygon
implicit none
integer :: i, ip

npl    = 0
do i=1,nthd
    call appendCRSPathToPol(thd(i))
end do

end subroutine copyThinDamsToPol
