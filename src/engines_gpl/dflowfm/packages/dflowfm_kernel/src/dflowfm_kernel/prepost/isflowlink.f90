SUBROUTINE ISflowlink(XP, YP, LL) ! IS THIS A flow NODE OR A flow LINK ?
 use m_netw
 use m_flowgeom
 use m_wearelt
 implicit none
 double precision :: XP, YP
 integer :: LL
 integer n, l, k1, k2
 double precision :: xa, ya

 LL = 0

 DO L = 1,lnx
    if (L > lnx1D) then
       k1 = lncn(1,l) ; k2 = lncn(2,L)  ! eigenlijk 3 en 4
       xa = 0.5*(xk(k1) + xk(k2)) ; ya = 0.5*(yk(k1) + yk(k2))
    else
       k1 = ln(1,L) ; k2 = ln(2,L)
       xa = 0.5*(xz(k1) + xz(k2)) ; ya = 0.5*(yz(k1) + yz(k2))
    endif

    IF (ABS(XA-XP) .LT. 3*RCIR .AND. ABS(YA-YP) .LT. 3*RCIR) THEN
        LL = L
        CALL DISLN(LL)
        RETURN
    ENDIF
 ENDDO

 RETURN
 END SUBROUTINE ISflowlink
