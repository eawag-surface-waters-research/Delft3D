 SUBROUTINE TEKNODENUMS(MET,NCOL)
  USE M_MISSING
  use m_netw
  implicit none
   integer :: MET, NCOL

  integer :: k
  integer :: k1
  integer :: k2
  integer :: key
  integer :: l
  integer :: n
  integer :: ndraw

  COMMON /DRAWTHIS/  ndraw(50)

  LOGICAL INVNOD
  DOUBLE PRECISION X, Y, Z
  CALL SETCOL(NCOL)
  KMOD = MAX(1,NUMK/100)
  DO K  = 1,NUMK
     IF (.NOT. INVNOD(K)) CYCLE
     X = XK(K)
     Y = YK(K)
     Z = ZK(K)

     IF (MOD(K,KMOD) .EQ. 0) THEN
         CALL HALT2(KEY)
         IF (KEY .EQ. 1) then
            RETURN
         end if
     ENDIF

     IF (RNOD(K) .NE. dmiss) THEN
        IF (MET .EQ. 2 .OR. MET .GE. 6) THEN
           IF (NDRAW(8) .EQ. 2 .OR. NDRAW(8) .EQ. 3 .OR. NDRAW(8) .EQ. 5 ) THEN
              CALL DHITEXT(INT(RNOD(K)),X,Y,Z)
           ELSE IF (MET .EQ. 4) THEN
              DO N  = 1,NMK(K)
                 L  = NOD(K)%LIN(N)
                 K1 = KN(1,L)
                 K2 = KN(2,L)
                 X  = 0.5d0*(XK(K1) + 0.5d0*XK(K2))
                 Y  = 0.5d0*(YK(K1) + 0.5d0*YK(K2))
                 Z  = 0.5d0*(ZK(K1) + 0.5d0*ZK(K2))
                 CALL DHITEXT(L,X,Y,Z)
              ENDDO
           ELSE
              CALL dHTEXT(dble(RNOD(K)),X,Y,Z)
           ENDIF
        ENDIF
     ENDIF
  ENDDO

  RETURN
  END SUBROUTINE TEKNODENUMS
