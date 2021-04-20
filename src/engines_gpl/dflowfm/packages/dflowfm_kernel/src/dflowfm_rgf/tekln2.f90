      SUBROUTINE TEKLN2(X, Y, mmax, nmax, M1, N1, M2, N2, NCOL)
!     TEKEN EEN LIJN IN GRID (MET CIRKELS ROND DE UITEINDEN)
      use m_missing
      implicit none
      integer :: mmax, nmax, m1, n1, m2, n2, ncol
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)

      integer :: istart, i, j, in, jn

      CALL SETCOL(NCOL)
      ISTART = 0
      IF (M1 .NE. 0) CALL CIRR(X(M1,N1),Y(M1,N1),NCOL)
      IF (M2 .NE. 0) CALL CIRR(X(M2,N2),Y(M2,N2),NCOL)
      IF (M1 .NE. 0 .AND. M2 .NE. 0) THEN
         IN = SIGN(1,M2-M1)
         JN = SIGN(1,N2-N1)
         DO 10 I = M1,M2,IN
            DO 10 J = N1,N2,JN
               IF (X(I,J) .NE. XYMIS) THEN
                  IF (ISTART .EQ. 0) THEN
                     CALL MOVABS(X(I,J),Y(I,J))
                     ISTART = 1
                  ELSE
                     CALL LNABS(X(I,J),Y(I,J))
                  ENDIF
               ELSE
                  ISTART = 0
               ENDIF
    10   CONTINUE
      ENDIF
      RETURN
      END subroutine tekln2
