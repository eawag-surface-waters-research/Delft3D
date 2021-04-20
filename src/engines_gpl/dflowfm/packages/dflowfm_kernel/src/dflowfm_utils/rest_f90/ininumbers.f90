      SUBROUTINE ININUMBERS()
      USE M_MISSING
      implicit none
      COMMON /NUMBERS/ PI, DG2RD, RD2DG, RA

      double precision :: pi, dg2rd, rd2dg, ra
      RA    = 6370000d0
!     RA    = dble(6378000.0)   DIT IN MEESTE ANDERE LITERATUUR
      PI    = acos(-1d0)
      DG2RD = PI/180d0
      RD2DG = 180d0/PI
      RETURN
      END
