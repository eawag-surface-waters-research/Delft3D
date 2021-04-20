  SUBROUTINE ADDELEM(K1,K2,JA)
  USE M_AFMETING
  implicit none
  integer :: K1,K2,JA

  double precision :: a0
  double precision :: ag
  double precision :: cdflow
  double precision :: cfl
  double precision :: cfric
  double precision :: e0
  double precision :: eps
  double precision :: fbouy
  double precision :: fdyn
  integer :: janet
  integer :: moments
  double precision :: pi
  double precision :: r0
  double precision :: rho
  double precision :: rhow

  DOUBLE PRECISION DLENGTH
  COMMON /SETTINGS/ FDYN, FBOUY, CDFLOW, CFRIC, MOMENTS, JANET
  COMMON /CONSTANTS/ E0, RHO, RHOW, CFL, EPS, AG, PI
  IF (JANET .EQ. 1) THEN
      A0 = PI*RDIAM*RDIAM/4
  ELSE
      A0 = 1E6*RWIDTH*RTHICK
  ENDIF
  R0 = DLENGTH(K1,K2)
  CALL CONNECT(K1,K2,1,A0,R0)
  RETURN
  END SUBROUTINE ADDELEM
