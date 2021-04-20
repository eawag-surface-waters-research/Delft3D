      SUBROUTINE TEKB(X,Y,MMAX,NMAX,NCOL)
      implicit none
      integer :: mmax, nmax, ncol
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)

      integer :: mb, nb, mb2, nb2, npt, npt2, nputo, itype
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      integer :: i

      IF (ITYPE .EQ. 1) THEN
         CALL TEKLN2(X, Y, mmax, nmax, MB(1), NB(1), MB(2), NB(2), NCOL)
      ENDIF
      DO 10 I = 1,6
         IF (MB(I) .NE. 0) THEN
             CALL CIRR(X(MB(I),NB(I)), Y(MB(I),NB(I)),NCOL)
         ENDIF
    10 CONTINUE
      RETURN
      END subroutine tekb
