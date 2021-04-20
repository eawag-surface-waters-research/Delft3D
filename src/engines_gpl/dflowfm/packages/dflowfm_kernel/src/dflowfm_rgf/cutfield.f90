      SUBROUTINE CUTFIELD(X,Y,mmax, nmax, MC,NC)
      use m_missing
      implicit none
      integer :: mmax, nmax, mc, nc
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)
      integer :: MB,NB,MB2,NB2,NPT,NPT2,NPUTO,ITYPE
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      integer :: i, j

      DO 10 I = 1,MC
         DO 10 J = 1,NC
            IF (I .GE. MB(3) .AND. I .LE. MB(4) .AND. J .GE. NB(3) .AND. J .LE. NB(4) )THEN
!               mooi houwen zo
            ELSE
               X(I,J) = XYMIS
               Y(I,J) = 0d0
            ENDIF
    10 CONTINUE
      RETURN
      END subroutine cutfield
