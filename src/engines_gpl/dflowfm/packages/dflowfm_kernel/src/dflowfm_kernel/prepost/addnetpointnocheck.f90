  SUBROUTINE addnetpointnocheck(XP,YP,ZP,K1)
  use m_netw
  implicit none
  double precision :: xp, yp, ZP
  integer          :: k1
  numk   = numk + 1
  k1     = numk
  xk(k1) = xp
  yk(k1) = yp
  ZK(K1) = ZP
  kc(k1) = 1
  RETURN
  END SUBROUTINE addnetpointnocheck
