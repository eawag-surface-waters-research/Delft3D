      SUBROUTINE SAVEB(NPUT)
      implicit none
      integer ::  nput
      integer :: mb, nb, mb2, nb2, npt, npt2, nputo, itype
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      MB2 = MB
      NB2 = NB

      NPT2  = NPT
      NPUTO = NPUT
      RETURN
      END subroutine saveb
