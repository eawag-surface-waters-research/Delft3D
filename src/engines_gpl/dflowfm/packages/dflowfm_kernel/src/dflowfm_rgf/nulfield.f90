      SUBROUTINE NULFIELD(X,Y, mmax, nmax)
      use m_missing
      implicit none
      integer :: mmax, nmax
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)
      integer :: MB,NB,MB2,NB2,NPT,NPT2,NPUTO,ITYPE
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      integer :: i, j

      DO 10 I = MB(3),MB(4)
         DO 10 J = NB(3),NB(4)
            X(I,J) = XYMIS
            Y(I,J) = 0d0
    10 CONTINUE
      RETURN
      END subroutine nulfield
