  SUBROUTINE TEKLINK(L,NCOL)
  use m_netw
  use unstruc_colors
  implicit none
  integer :: L, NCOL
  integer :: jaSmallCir
  double precision :: dlength

  integer :: k1
  integer :: k2

  CALL SETLINKCOLOUR(L,NCOL)

  K1 = KN(1,L)
  K2 = KN(2,L)
  IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
     CALL MOVABS( XK(K1),YK(K1) )
     CALL  LNABS( XK(K2),YK(K2) )
     IF (NCOL > 0) THEN
         CALL SETCOL(NCOLNN)
         CALL PTABS(XK(K1),YK(K1) )
         CALL PTABS(XK(K2),YK(K2) )
     ENDIF
  ENDIF
  RETURN
  END SUBROUTINE TEKLINK
