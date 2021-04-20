  SUBROUTINE NODEMASS()
  use m_netw
  USE M_AFMETING
  use m_missing, only: jins, dmiss
  use geometry_module, only: dpinpok
  implicit none

  double precision :: ag
  double precision :: cfl
  double precision :: e0
  double precision :: eps
  integer :: in1
  integer :: in2
  integer :: k
  integer :: k1
  integer :: k2
  integer :: l
  double precision :: pi
  double precision :: rho
  double precision :: rhow
  double precision :: rmas
  COMMON /CONSTANTS/ E0, RHO, RHOW, CFL, EPS, AG, PI

!  DO K = 1,NUMK
!     RM(K) = 0
!  ENDDO

  DO L = 1,NUML
     K1 = KN(1,L)
     K2 = KN(2,L)
     IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
        CALL DPINPOK( XK(K1), YK(K1), ZK(K1), NPL, XPL, YPL, IN1, jins, dmiss)
        CALL DPINPOK( XK(K2), YK(K2), ZK(K2), NPL, XPL, YPL, IN2, jins, dmiss)
!        IF (IN1 .EQ. 1 .AND. IN2 .EQ. 1) THEN
!           RMAS   = RHO*RL(L)*EA(L)*1D-6
!           RM(K1) = RM(K1) + RMAS/2
!           RM(K2) = RM(K2) + RMAS/2
!        ENDIF
     ENDIF
  ENDDO
  RETURN
  END SUBROUTINE NODEMASS
