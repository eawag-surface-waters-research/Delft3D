      SUBROUTINE FIELDOPT(NFLD)
      USE M_GRID
      implicit none
      integer :: nfld
      integer, PARAMETER :: MAXOP = 64
      integer :: nwhat2, maxexp, maxopt, i
      CHARACTER*40 OPTION(MAXOP),EXP(MAXOP),FIELDOP
      EXP(1)    = 'MENU 10                                 '
      EXP(2)    = 'GRID EDIT OPTIONS                       '
      MAXOPT    = 22
      DO 10 I = 1,MAXOPT
         OPTION(I) =  FIELDOP(I)
    10 CONTINUE
      NWHAT2  = NFLD
      CALL MENUV2(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      IF (NWHAT2 .GE. 1) THEN
         IF (NWHAT2 == 19) THEN
            CALL ORTHOGRID(1,1,MC,NC)
         else IF (NWHAT2 == 20) THEN
            call LOCALREFINE(Nwhat2, 1, 1, mc, nc, 1)
         else IF (NWHAT2 == 21) THEN
            call LOCALREFINE(Nwhat2, 1, 1, mc, nc, 2)
         ELSE
            NFLD = NWHAT2
         ENDIF
      ENDIF
      RETURN
      END subroutine fieldopt
