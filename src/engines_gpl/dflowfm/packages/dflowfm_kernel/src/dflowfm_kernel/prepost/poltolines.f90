  SUBROUTINE POLTOLINES()

  use m_netw
  use m_afmeting
  use gridoperations

  implicit none
  double precision :: ael
  double precision :: ag
  double precision :: cfl
  double precision :: e0
  double precision :: eps
  integer :: k
  integer :: k1
  integer :: k2
  double precision :: pi
  double precision :: rho
  double precision :: rhow
  double precision :: rml
  double precision :: zp
  COMMON /CONSTANTS/ E0, RHO, RHOW, CFL, EPS, AG, PI
  DOUBLE PRECISION DLENGTH

  AEL    = PI*RDIAM*RDIAM/4                  ! RDIAM in mm
  DO K = 1,NPL-1
    CALL ISNODE( K1, XPL(K), YPL(K), ZP )
    IF (K1 .EQ. 0) THEN
       CALL GIVENEWNODENUM(K1)
       CALL SETPOINT(XPL(K),YPL(K),ZP,K1)
    ENDIF
    CALL ISNODE( K2, XPL(K+1), YPL(K+1), ZP )
    IF (K2 .EQ. 0) THEN
       CALL GIVENEWNODENUM(K2)
       CALL SETPOINT(XPL(K+1),YPL(K+1),ZP,K2)
    ENDIF
    RML = DLENGTH(K1,K2)
    CALL CONNECT(K1,K2,LFAC,AEL,RML)
  ENDDO
  RETURN
  END SUBROUTINE POLTOLINES
