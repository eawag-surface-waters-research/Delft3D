  SUBROUTINE NODTOALL()
  use m_netw
  use gridoperations
  implicit none

  integer :: ja
  integer :: k
  integer :: k1
  integer :: n1
  double precision :: XX,YY,ZZ
  N1 = NUMK
  XX = 0.5d0 ; YY = 0.5d0 ; ZZ = 0d0
  CALL GIVENEWNODENUM(K1)
  CALL SETPOINT(XX,YY,ZZ,K1)
  DO K = 1,N1
     CALL ADDELEM(K1,K,JA)
  ENDDO
  RETURN
  END SUBROUTINE NODTOALL
