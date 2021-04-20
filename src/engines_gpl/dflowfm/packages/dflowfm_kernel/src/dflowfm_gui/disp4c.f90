      SUBROUTINE DISP4C(X,Y,N)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: istart
      integer :: key
      integer :: n
!     LAAT EEN TWEEDIMENSIONALE FUNCTIE ZIEN MET CIRKELS
      double precision :: X(N), Y(N)

      IF (N .LE. 0) RETURN
      ISTART = 0
      DO 10 I = 1,N
         IF (X(I) .NE. dmiss) THEN
            IF (ISTART .EQ. 1) THEN
               CALL LNABS(X(I),Y(I))
            ELSE
               CALL MOVABS(X(I),Y(I))
               ISTART = 1
            ENDIF
            CALL RCIRC(X(I),Y(I))
         ELSE
            ISTART = 0
         ENDIF
         IF (MOD(I,50) .EQ. 0) THEN
             CALL HALT2(KEY)
             IF (KEY .EQ. 1) RETURN
         ENDIF
   10 CONTINUE
      RETURN
      END
