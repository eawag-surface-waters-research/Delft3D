  SUBROUTINE GETMIDDLEKNOT(K1,K2,K12,A12,R12)
  use m_netw
  use gridoperations
  implicit none
  integer :: K1,K2,K12,K22
  double precision :: A12, R12

  integer :: l1
  integer :: l2
  integer :: n1
  integer :: n2

  DO N1  = 1,NMK(K1)
     L1  = NOD(K1)%LIN(N1)
     CALL OTHERNODE(K1,L1,K12)
     DO N2 = 1,NMK(K2)
        L2 = NOD(K2)%LIN(N2)
        CALL OTHERNODE(K2,L2,K22)
        IF (K12 .EQ. K22) THEN
           A12 = 0 ! ( EA(L1) + EA(L2) ) /2
           R12 = 0 ! ( RL(L1) + RL(L2) ) /2
           RETURN
        ENDIF
     ENDDO
  ENDDO
  K12 = 0
  RETURN
  END SUBROUTINE GETMIDDLEKNOT
