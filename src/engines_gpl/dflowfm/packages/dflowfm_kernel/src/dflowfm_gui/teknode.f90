  SUBROUTINE TEKNODE(KP,NCOL)
  use m_netw
  use unstruc_colors
  implicit none
  integer :: KP, NCOL
  integer :: k1
  integer :: k2
  integer :: l
  integer :: n

  CALL SETCOL(NCOL)
  DO N  = 1,NMK(KP)
     L  = NOD(KP)%LIN(N)
     K1 = KN(1,L)
     K2 = KN(2,L)
     if (k1 > 0 .and. k2 > 0) then
        CALL DMOVABS( XK(K1),YK(K1),ZK(K1) )
        CALL  DLNABS( XK(K2),YK(K2),ZK(K2) )
     endif
  ENDDO

  IF (NCOL > 0) THEN
     CALL SETCOL(NCOLNN)
     DO N  = 1,NMK(KP)
        L  = NOD(KP)%LIN(N)
        K1 = KN(1,L)
        K2 = KN(2,L)
        if (k1 > 0) then
           CALL DPTABS( XK(K1),YK(K1),ZK(K1) )
        endif
        if (k2 > 0) then
           CALL DPTABS( XK(K2),YK(K2),ZK(K2) )
        endif
     ENDDO
  ENDIF

  IF (KC(KP) .EQ. -1) CALL DCIRR(XK(KP),YK(KP),ZK(KP),NCOL)
  RETURN
  END SUBROUTINE TEKNODE
