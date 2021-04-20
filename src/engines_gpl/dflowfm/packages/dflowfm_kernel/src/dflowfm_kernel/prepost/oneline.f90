  SUBROUTINE ONELINE(K,RD) ! TWEE LIJNTJES WORDEN 1
  use m_netw
  use gridoperations
  implicit none
  integer :: K
  double precision :: RD

  double precision :: a0
  double precision :: ag
  double precision :: cdflow
  double precision :: cfl
  double precision :: cfric
  double precision :: e0
  double precision :: eps
  double precision :: fbouy
  double precision :: fdyn
  integer :: ja
  integer :: janet
  integer :: k1
  integer :: k2
  integer :: l1
  integer :: l2
  integer :: lfa
  integer :: moments
  integer :: nm
  double precision :: pi
  double precision :: r0
  double precision :: rho
  double precision :: rhow

  COMMON /SETTINGS/ FDYN, FBOUY, CDFLOW, CFRIC, MOMENTS, JANET
  COMMON /CONSTANTS/ E0, RHO, RHOW, CFL, EPS, AG, PI
  JA = 0
  NM = NMK(K)
  IF (NM .EQ. 2) THEN
     L1 = NOD(K)%LIN(1)
     L2 = NOD(K)%LIN(2)
     ! IF (RL(L1) .LT. RD .OR. RL(L2) .LT. RD) THEN
        CALL OTHERNODE(K,L1,K1)
        CALL OTHERNODE(K,L2,K2)
        R0  = 0 !  RL(L1) + RL(L2)
        A0  = 0 !(EA(L1) + EA(L2)) / 2d0
        LFA = 1
        CALL DELNODE(K)
        CALL CONNECT(K1,K2,LFA,A0,R0)
     ! ENDIF
  ENDIF
  RETURN
  END SUBROUTINE ONELINE
