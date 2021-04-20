 SUBROUTINE DISLN(LL)   ! print link values
 use m_flowgeom
 use m_devices
 use network_data, only:kn
 use unstruc_display
 implicit none

 integer :: LL
 CHARACTER TEX*23
 DOUBLE PRECISION :: ZLIN

 IF (LL .LE. 0) THEN
    TEX = 'NO FLOW LINK FOUND    '
    CALL KTEXT(TEX,IWS-22,4,15)
 ELSE
    TEX = 'FLOW LINK NR:         '
    WRITE(TEX (14:),'(I10)') LL
    CALL KTEXT(TEX,IWS-22,4,15)
    TEX = 'VAL=                  '
    WRITE(TEX(6:), '(E18.11)') ZLIN(LL)
    CALL KTEXT(TEX,IWS-22,5,15)
    TEX = 'Nd1:         '
    WRITE(TEX (6:),'(I10)') LN(1,LL)
    CALL KTEXT(TEX,IWS-22,6,15)
    call gtext(tex, xz(ln(1,LL)), yz(ln(1,LL)), 221)
    TEX = 'Nd2:         '
    WRITE(TEX (6:),'(I10)') LN(2,LL)
    CALL KTEXT(TEX,IWS-22,7,15)
    call gtext(tex, xz(ln(2,LL)), yz(ln(2,LL)), 221)
 ENDIF

 RETURN
 END SUBROUTINE DISLN
