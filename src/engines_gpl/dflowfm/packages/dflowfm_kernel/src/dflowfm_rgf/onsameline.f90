      SUBROUTINE ONSAMELINE(IPT,MP,NP,JA)
      implicit none
      integer :: mp, np, ja, ipt
      integer :: mb, nb, mb2, nb2, npt, npt2, nputo, itype
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      integer :: md, nd
      JA = 1
      IF (ITYPE .EQ. 1) THEN
         IF (IPT .EQ. 1 .AND. MB(2) .NE. 0) THEN
            MD = MP - MB(2)
            ND = NP - NB(2)
            IF (MD .NE. 0 .AND. ND .NE. 0) JA = 0
         ELSE IF (IPT .EQ. 2) THEN
            MD = MP - MB(1)
            ND = NP - NB(1)
            IF (MD .NE. 0 .AND. ND .NE. 0) JA = 0
         ENDIF
      ENDIF
      RETURN
      END subroutine onsameline
