subroutine getlink1(k,LL)
use m_flowgeom
integer :: k, LL
if (nd(k)%lnx == 0) then
    LL = 1
else
    LL = iabs(nd(k)%ln(1))
endif
end subroutine
