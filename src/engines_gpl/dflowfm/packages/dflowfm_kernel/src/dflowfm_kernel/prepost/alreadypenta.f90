  SUBROUTINE ALREADYPENTA(K1,K2,K3,K4,K5,JA)
  use m_netw
  implicit none
  integer :: K1,K2,K3,K4,K5,JA

  integer :: n1
  integer :: n2
  integer :: n3
  integer :: n4
  integer :: n5
  integer :: np
  JA = 0

  ! IF (K1 .EQ. 61 .OR. K2 .EQ. 61 .OR. K3 .EQ. 61 .OR. K4 .EQ. 61 .OR. K5 .EQ. 61) THEN
  !    JA = 1
  !    RETURN ! BUCKEYBALL
  ! ENDIF


  DO NP = NUMP, 1, -1
     IF (netcell(NP)%N .EQ. 5) THEN
        N1 = netcell(NP)%NOD(1)
        N2 = netcell(NP)%NOD(2)
        N3 = netcell(NP)%NOD(3)
        N4 = netcell(NP)%NOD(4)
        N5 = netcell(NP)%NOD(5)
        IF ((K1.EQ.N1 .OR. K1.EQ.N2  .OR. K1.EQ.N3 .OR. K1.EQ.N4 .OR. K1 .EQ. N5) .AND. &
            (K2.EQ.N1 .OR. K2.EQ.N2  .OR. K2.EQ.N3 .OR. K2.EQ.N4 .OR. K2 .EQ. N5) .AND. &
            (K3.EQ.N1 .OR. K3.EQ.N2  .OR. K3.EQ.N3 .OR. K3.EQ.N4 .OR. K3 .EQ. N5) .AND. &
            (K4.EQ.N1 .OR. K4.EQ.N2  .OR. K4.EQ.N3 .OR. K4.EQ.N4 .OR. K4 .EQ. N5) .AND. &
            (K5.EQ.N1 .OR. K5.EQ.N2  .OR. K5.EQ.N3 .OR. K5.EQ.N4 .OR. K5 .EQ. N5) ) THEN
            JA = np
            call qnerror('already 5', ' ',' ')
            RETURN
        ENDIF
     ENDIF
  ENDDO
  RETURN
  END SUBROUTINE ALREADYPENTA
