subroutine copycellstosam()
use m_samples
use m_netw
use m_missing
use m_polygon, only: NPL, xpl, ypl, zpl
use geometry_module, only: dbpinpol


implicit none
integer                       :: in, k, c
double precision, external    :: znetcell
in = -1
k  = ns

!check if the netcells are included in the polygon
LC = 0
do c = 1,nump
    if (rlin(c) .ne. dmiss) then
        CALL DBPINPOL(xzw(c), yzw(c), IN,  dmiss, JINS, NPL, xpl, ypl, zpl)
        IF (IN == 1) THEN
            LC(c) = 1
            K     = K + 1
        ENDIF
    ENDIF
ENDDO

CALL INCREASESAM(k)

!assign the calculated value in rlin
K = NS
do c = 1,nump
    IF (LC(c) == 1) THEN
        k = k + 1
        xs(k) = xzw(c) ; ys(k) = yzw(c) ; zs(k) = znetcell(k)
    endif
enddo
ns = k

end subroutine copycellstosam
