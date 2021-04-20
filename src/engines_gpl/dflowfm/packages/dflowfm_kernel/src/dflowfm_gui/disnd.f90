 SUBROUTINE DISND(NN)   ! print node values
 use m_devices
 use m_flowgeom
 implicit none
 integer :: nn

 CHARACTER TEX*23
 DOUBLE PRECISION :: ZNOD

 IF (NN .LE. 0) THEN
    TEX = 'NO FLOW NODE FOUND    '
    CALL KTEXT(TEX,IWS-22,4,15)
 ELSE
    TEX = 'FLOW NODE NR:         '
    WRITE(TEX (14:),'(I10)') NN
    CALL KTEXT(TEX,IWS-22,4,15)
    TEX = 'VAL=                  '
    WRITE(TEX(6:), '(E18.11)') ZNOD(NN)
    CALL KTEXT(TEX,IWS-22,5,15)
    TEX = 'XZ =                  '
    WRITE(TEX(6:), '(E18.11)') XZ(NN)
    CALL KTEXT(TEX,IWS-22,6,15)
    TEX = 'yZ =                  '
    WRITE(TEX(6:), '(E18.11)') YZ(NN)
    CALL KTEXT(TEX,IWS-22,7,15)
 ENDIF

 RETURN
 END SUBROUTINE DISND
