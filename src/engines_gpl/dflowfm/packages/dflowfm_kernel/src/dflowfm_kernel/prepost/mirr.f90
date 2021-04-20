  SUBROUTINE MIRR(X,Y,Z,X2,Y2,Z2)
  USE M_LANDBOUNDARY
  implicit none
  DOUBLE PRECISION X,Y,Z,X2,Y2,Z2

  double precision :: ym
  YM = (YLAN(1) + YLAN(2)) / 2
  X2 = X
  Y2 = 2*YM - Y
  Z2 = Z
  RETURN
  END SUBROUTINE MIRR
