SUBROUTINE DISPFLOWNODEVALS(KP)
  use m_flowgeom
  use m_flow
  USE M_DEVICES

  implicit none
  DOUBLE PRECISION :: ZNOD
  integer :: KP

  integer :: l
  integer :: n
  CHARACTER TEX*23

  IF (KP .EQ. 0) RETURN
  CALL DRCIRC(XZ(KP),YZ(KP),BL(KP))

  TEX = 'NODE NR    :           '
  WRITE(TEX (14:),'(I10)') KP
  CALL KTEXT(TEX,IWS-22,4,15)

  TEX = 'X COORD    :           '
  WRITE(TEX (14:),'(E10.3)') Xz(KP)
  CALL KTEXT(TEX,IWS-22,13,15)

  TEX = 'Y COORD    :           '
  WRITE(TEX (14:),'(E10.3)') Yz(KP)
  CALL KTEXT(TEX,IWS-22,14,15)

  TEX = 'Z COORD    :           '
  WRITE(TEX (14:),'(E10.3)') bl(KP)
  CALL KTEXT(TEX,IWS-22,15,15)

  TEX = 'Z COORD    :           '
  WRITE(TEX (14:),'(e10.4)') znod(kp)
  CALL KTEXT(TEX,IWS-22,16,15)

  TEX = 'link       :           '
  DO N = 1,Nd(kp)%lnx
     L = ND(KP)%LN(N)
     WRITE(TEX ( 6:11),'(I6 )') N
     WRITE(TEX (14:23),'(I10)') L
     CALL KTEXT(TEX,IWS-22,16+N,15)
  ENDDO
  end SUBROUTINE DISPFLOWNODEVALS
