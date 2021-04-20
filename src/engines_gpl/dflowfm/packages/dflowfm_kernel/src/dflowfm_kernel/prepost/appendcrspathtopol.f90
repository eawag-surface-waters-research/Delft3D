!> Appends the polyline of a cross section path to the current global
!! polyline. Useful for converting cross sections, thin dams or thin
!! dykes back to editable polylines.
subroutine appendCRSPathToPol(path)
use m_crspath
use m_polygon
use m_alloc
use m_missing
implicit none
type(tcrspath), intent(in) :: path

integer :: i, ip

call increasepol(npl+1+path%np, 1)

! Insert dmiss seperator behind existing polylines, if any.
if (npl > 0) then
    npl = npl+1
    xpl(npl) = dmiss
    xpl(npl) = dmiss
    zpl(npl) = dmiss
end if

do ip=1,path%np
    npl = npl+1
    xpl(npl) = path%xp(ip)
    ypl(npl) = path%yp(ip)
    zpl(npl) = path%zp(ip)
end do
end subroutine appendCRSPathToPol
