      SUBROUTINE SMEERFUNCTIE(I,J,MP,NP,FR,IN,JN)
      implicit none
      integer :: i, j, mp, np, in, jn
      double precision :: fr

      integer :: mb, nb, mb2, nb2, npt, npt2, nputo, itype
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      double precision :: pi, phi, fri, frj
      PI = ACOS(-1d0)

      IF (I .EQ. MP) THEN
         PHI = 0
      ELSE IF (I .GT. MP .AND. I .LT. MB(4) ) THEN
         PHI = PI*dble(I - MP)/dble( MB(4) - MP )
      ELSE IF (I .LT. MP .AND. I .GT. MB(3)) THEN
         PHI = PI*dble(MP - I)/dble( MP - MB(3) )
      ELSE
         PHI = PI
      ENDIF
      FRI = (1 + COS(PHI) ) / 2

      IF (J .EQ. NP) THEN
         PHI = 0
      ELSE IF (J .GT. NP .AND. J .LT. NB(4) ) THEN
         PHI = PI*dble(J - NP)/dble( NB(4) - NP )
      ELSE IF (J .LT. NP .AND. J .GT. NB(3)) THEN
         PHI = PI*dble(NP - J)/dble( NP - NB(3) )
      ELSE
         PHI = PI
      ENDIF
      FRJ = (1 + COS(PHI) ) / 2

      IF (IN .EQ. 1 .AND. JN .EQ. 1) THEN
         FR = SQRT(FRI*FRJ)
      ELSE IF (JN .EQ. 1) THEN
         FR = FRJ
      ELSE IF (IN .EQ. 1) THEN
         FR = FRI
      ENDIF

      RETURN
      END subroutine smeerfunctie
