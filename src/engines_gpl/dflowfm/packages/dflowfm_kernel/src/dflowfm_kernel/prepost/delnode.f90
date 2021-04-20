  SUBROUTINE DELNODE(KP)
  use m_netw
  use m_missing
  implicit none
  integer :: KP

  double precision :: ag
  double precision :: cfl
  double precision :: e0
  double precision :: eps
  integer :: k1
  integer :: k2
  integer :: l1
  integer :: lnu
  integer :: nm1
  double precision :: pi
  double precision :: rho
  double precision :: rhow

  COMMON /CONSTANTS/ E0, RHO, RHOW, CFL, EPS, AG, PI

  DO NM1  = NMK(KP),1,-1
     L1   = NOD(KP)%LIN(NM1)
     K1   = KN(1,L1)
     K2   = KN(2,L1)
     CALL DELELEM(K1,K2,LNU)
  ENDDO
  NMK(KP) = 0
  KC(KP)  = 0
  XK(KP)  = dmiss
  YK(KP)  = dmiss
  ZK(KP)  = dmiss
  ! RM(KP)  = 0

  RETURN
  END SUBROUTINE DELNODE
