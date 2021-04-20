subroutine copynetlinkstosam()
use m_samples
use m_netw
use m_missing
use m_polygon, only: NPL, xpl, ypl, zpl
use geometry_module, only: dbpinpol

implicit none
integer :: in, k, l, K1, K2
in = -1
k  = ns

!need to compute the coordinates of the links
if (.not. allocated(xe)) then
    allocate(xe(numl))
endif
if (.not. allocated(ye)) then
    allocate(ye(numl))
endif

LC = 0
do l = 1,numl
    if (rlin(l) .ne. dmiss) then
        K1 = KN(1,L)
        K2 = KN(2,L)
        ! calculate the centre of the link
        xe(l) = .5d0*(xk(K1) + xk(K2)) ! TODO: LC: make this sferic+3D-safe
        ye(l) = .5d0*(yk(K1) + yk(K2))
        CALL DBPINPOL(xe(l), ye(l), IN, dmiss, JINS, NPL, xpl, ypl, zpl)
        IF (IN == 1) THEN
            LC(l) = 1
            K     = K + 1
        ENDIF
    ENDIF
ENDDO

CALL INCREASESAM(k)

!assign the calculated value in rlin
K = NS
do l = 1,numl
    IF (LC(l) == 1) THEN
        k = k + 1
        xs(k) = xe(l) ; ys(k) = ye(l) ; zs(k) = rlin(l)
    endif
enddo
ns = k

end subroutine copynetlinkstosam
