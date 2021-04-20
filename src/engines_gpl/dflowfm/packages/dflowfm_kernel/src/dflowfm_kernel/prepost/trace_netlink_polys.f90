subroutine trace_netlink_polys()

use network_data
use m_alloc
use gridoperations

implicit none

integer :: i, ip, L, kcur, knext, lcur, iloc

if ( .not.allocated(Lc) ) allocate(Lc(numL))
lc = 0
ip = 0

call realloc(netlinkpath_xk, numk+numl)
call realloc(netlinkpath_yk, numk+numl)
call realloc(netlinkpath_end, numl)

iloc  = 0
do i=1,numl
    ! Check whether link was already written to file.
    if (lc(i) == 1) then
        cycle
    end if

    ip = ip+1

    kcur = kn(1,i)
    iloc = iloc+1
    netlinkpath_xk(iloc) = xk(kcur)
    netlinkpath_yk(iloc) = yk(kcur)
    !zloc(1) = 0d0

    kcur = kn(2,i)
    iloc = iloc+1
    netlinkpath_xk(iloc) = xk(kcur)
    netlinkpath_yk(iloc) = yk(kcur)
    !zloc(2) = 0d0
    lc(i) = 1
    ! We started a new path, now trace connected links as long as possible.
    do
        lcur = 0
        ! Find an outgoing link of current net node that wasn't yet traced.
        do L=1,nmk(kcur)
            if (lc(nod(kcur)%lin(L)) == 0) then
                lcur = nod(kcur)%lin(L)
                exit
            end if
        end do
        if (lcur == 0) then ! no further links in string found, leave this loop and write it.
            exit
        end if

        ! lcur is new link: add it to linestring
        iloc = iloc+1
        call othernode(kcur, lcur, knext)
        netlinkpath_xk(iloc) = real(xk(knext))
        netlinkpath_yk(iloc) = real(yk(knext))
        lc(lcur) = 1
        kcur = knext
    end do
    netlinkpath_end(ip) = iloc
end do !i=1,numl
numpath = ip

end subroutine trace_netlink_polys
