      SUBROUTINE NEWBLOCKPOINT(MP,NP,JA,IPT)
      implicit none
      integer :: mp, np, ja, ipt
      integer :: mb, nb, mb2, nb2, npt, npt2, nputo, itype
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE
!     NIEUW PUNT = 1, OUD PUNT = 0, NIEW PUNT MAAR REEDS VIER PUNTEN = -1
      integer :: i
      JA = 1
      DO 10 I=1,NPT
         IF (MP .EQ. MB(I) .AND. NP .EQ. NB(I)) THEN
           JA  = 0
           IPT = I
           RETURN
         ENDIF
    10 CONTINUE
      IPT = NPT + 1
      IF (NPT .EQ. 4) JA = -1
      RETURN
      END subroutine newblockpoint
