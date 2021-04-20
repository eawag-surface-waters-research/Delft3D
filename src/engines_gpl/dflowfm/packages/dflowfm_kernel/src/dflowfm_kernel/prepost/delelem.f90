    SUBROUTINE DELELEM(K1,K2,LNU)
    use m_netw
    implicit none
    integer :: K1,K2,LNU

    double precision :: ag
    double precision :: cfl
    double precision :: e0
    double precision :: eps
    integer :: l1
    integer :: l2
    integer :: nm1
    integer :: nm2
    double precision :: pi
    double precision :: rho
    double precision :: rhow
    double precision :: rmas

    COMMON /CONSTANTS/ E0, RHO, RHOW, CFL, EPS, AG, PI

    LNU = 0
    DO L1 = 1,NMK(K1)
       DO L2 = 1,NMK(K2)
          IF ( LNU .EQ. 0 .AND. NOD(K1)%LIN(L1) .EQ. NOD(K2)%LIN(L2) ) THEN
             LNU = NOD(K1)%LIN(L1)
             NOD(K1)%LIN(L1) = 0
             NOD(K2)%LIN(L2) = 0
          ENDIF
       ENDDO
    ENDDO
    IF (LNU .EQ. 0) THEN
!       KN(1,LNU) = 0
!       KN(2,LNU) = 0
       RETURN
    ENDIF

    DO L1 = 1,NMK(K1)
       IF (NOD(K1)%LIN(L1) .EQ. 0) THEN
          NMK(K1) = NMK(K1) - 1
          DO NM1  = L1,NMK(K1)
             NOD(K1)%LIN(NM1) = NOD(K1)%LIN(NM1+1)
          ENDDO
          EXIT
       ENDIF
    ENDDO
    IF (NMK(K1) == 0) KC(K1) = 0

    DO L2 = 1,NMK(K2)
       IF (NOD(K2)%LIN(L2) .EQ. 0) THEN
          NMK(K2) = NMK(K2) - 1
          DO NM2  = L2,NMK(K2)
             NOD(K2)%LIN(NM2) = NOD(K2)%LIN(NM2+1)
          ENDDO
          EXIT
       ENDIF
    ENDDO
    IF (NMK(K2) == 0) KC(K2) = 0

    KN(1,LNU) = 0
    KN(2,LNU) = 0

    ! RMAS      = RHO*RL(LNU)*EA(LNU)*1D-6
    ! RM(K1)    = RM(K1) - RMAS/2
    ! RM(K2)    = RM(K2) - RMAS/2

    ! EA(LNU)   = 0
    ! RL(LNU)   = 0

    RETURN
    END SUBROUTINE DELELEM
