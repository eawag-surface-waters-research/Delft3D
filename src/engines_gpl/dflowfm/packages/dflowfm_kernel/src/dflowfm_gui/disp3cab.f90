      SUBROUTINE DISP3CAB(X,Y,Z,NCL,N,RCIR,NCOL,A,B)
      USE M_MISSING
      implicit none
      double precision :: a
      double precision :: b
      integer :: i
      integer :: istart
      integer :: key
      integer :: n
      integer :: ncol
      double precision :: rcir
!     LAAT EEN TWEEDIMENSIONALE FUNCTIE ZIEN MET CIRKELS EN KLEUREN
      DOUBLE PRECISION X(N), Y(N), Z(N)
      INTEGER NCL(N)

      IF (N .LE. 0) RETURN
      CALL SETCOL(NCOL)
      ISTART = 0
      DO 10 I = 1,N
         IF (X(I) .NE. dmiss) THEN
            IF (ISTART .EQ. 1) THEN
               CALL DLNABS(A*X(I)+B,Y(I),Z(I))
            ELSE
               IF (NCL(I) .NE. 0) CALL SETCOL(NCL(I))
               CALL DMOVABS(A*X(I)+B,Y(I),Z(I))
               ISTART = 1
            ENDIF
            CALL CIR(RCIR)
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
