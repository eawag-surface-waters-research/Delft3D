!> Copy the original polygons that define the current cross sections
!! to the active polygons in xpl,...
subroutine copyCrossSectionsToPol()
use m_monitoring_crosssections
use m_polygon
use m_alloc
implicit none
integer :: i, ip

npl    = 0
call realloc(nampli, ncrs, fill=' ')
do i=1,ncrs
    nampli(i) = crs(i)%name
    call appendCRSPathToPol(crs(i)%path)
end do

end subroutine copyCrossSectionsToPol
