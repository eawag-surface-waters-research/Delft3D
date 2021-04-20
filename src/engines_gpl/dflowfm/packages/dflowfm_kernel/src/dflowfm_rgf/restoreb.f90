      SUBROUTINE RESTOREB(NPUT)
      implicit none
      integer ::  nput
      integer :: mb, nb, mb2, nb2, npt, npt2, nputo, itype
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      MB = MB2
      NB = NB2
      NPT  = NPT2
      NPUT = NPUTO
      RETURN
      END subroutine restoreb
