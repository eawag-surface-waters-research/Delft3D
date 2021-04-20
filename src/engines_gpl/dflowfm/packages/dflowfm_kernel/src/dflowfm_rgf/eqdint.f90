      SUBROUTINE EQDINT(YH2,imax,TJ,Y2)
      implicit none
      integer :: imax
      double precision :: YH2(imax)
      double precision :: TJ, Y2
      integer :: j1, j2
      double precision :: T1, T2
      J1 = INT(TJ) + 1
      J2 = J1 + 1
      T1 = TJ - INT(TJ)
      T2 = 1 - T1
      Y2 = T2*YH2(J1) + T1*YH2(J2)
      RETURN
      END subroutine EQDINT
