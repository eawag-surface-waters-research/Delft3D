  SUBROUTINE TEKLINKNUMS(MET,NCOL)
  USE M_MISSING
  use m_netw
  implicit none
  integer :: MET, NCOL

  integer :: k1
  integer :: k2
  integer :: key
  integer :: l
  integer :: ndraw
  double precision :: vv
  logical :: invnod

  COMMON /DRAWTHIS/  ndraw(50)

  DOUBLE PRECISION XP,YP,ZP
  CALL SETCOL(NCOL)
  IF (MET .EQ. 2 .OR. MET .GE. 6 .and. MET .LE. 8) THEN
     LMOD = MAX(1,NUML/100)
     DO L  = 1,NUML
        IF (MOD(L,LMOD) .EQ. 0) THEN
            CALL HALT2(KEY)
            IF (KEY .EQ. 1) then
               RETURN
            end if
        ENDIF
        VV = RLIN(L)
        IF (VV .NE. dmiss) THEN
           K1 = KN(1,L)
           K2 = KN(2,L)
           IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
              IF (.NOT. INVNOD(K1) .and. .NOT. INVNOD(K2)) CYCLE
              XP = 0.5d0*(XK(K1) + XK(K2))
              YP = 0.5d0*(YK(K1) + YK(K2))
              ZP = 0.5d0*(ZK(K1) + ZK(K2))
              IF (NDRAW(7) .EQ. 2 .OR. NDRAW(7) .EQ. 3 .OR. (NDRAW(7) >= 10 .and. ndraw(7).ne.16 .and. ndraw(7).ne.17 .and. ndraw(7).ne.18)) THEN
                 CALL DHITEXT(INT(VV),XP,YP,ZP)
              ELSE
                 CALL DHTEXT(VV,XP,YP,ZP)
              ENDIF
           ENDIF
        ENDIF
     ENDDO
  ENDIF
  RETURN
  END SUBROUTINE TEKLINKNUMS
