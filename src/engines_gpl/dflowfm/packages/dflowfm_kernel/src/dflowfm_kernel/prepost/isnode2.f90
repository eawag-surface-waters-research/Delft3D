  SUBROUTINE ISNODE2(KP, XP, YP, ZP)  ! X,Y,Z MOETEN ALLEN KLOPPEN
  use m_netw
  use m_wearelt
  implicit none
  integer :: KP
  double precision :: XP, YP, ZP

  double precision :: eps
  integer :: jav
  integer :: jview
  integer :: k
  double precision :: xyz

  COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
  KP  = 0
  EPS = 0.01d0*RCIR

  DO K = NUMK,1,-1
     IF (ABS(XK(K)-XP) .LT. EPS .AND. ABS(YK(K)-YP) .LT. EPS .AND. ABS(ZK(K)-ZP) .LT. EPS) THEN
         KP = K
         RETURN
     ENDIF
  ENDDO
  RETURN
  END SUBROUTINE ISNODE2
