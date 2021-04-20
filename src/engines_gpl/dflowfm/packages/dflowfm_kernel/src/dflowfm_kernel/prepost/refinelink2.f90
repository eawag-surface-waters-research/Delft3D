  SUBROUTINE REFINELINK2(L12,K12)
  use m_netw
  use gridoperations
  implicit none
  integer :: L12,K12

  integer :: k1
  integer :: k2
  integer :: lnu
  DOUBLE PRECISION :: XM, YM

  K1 = KN(1,L12) ; KC(K1) = 5
  K2 = KN(2,L12) ; KC(K2) = 5

  KN(1,L12) = 0 ; KN(2,L12) = 0

  XM = 0.5D0*(XK(K1) + XK(K2))
  YM = 0.5D0*(YK(K1) + YK(K2))

  CALL DSETNEWPOINT(XM,YM,K12)
  CALL NEWLINK(K1 ,K12,lnu) ! fast version without refinement
  CALL NEWLINK(K12,K2 ,lnu) ! fast version without refinement
  END SUBROUTINE REFINELINK2
