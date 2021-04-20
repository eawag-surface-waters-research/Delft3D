  SUBROUTINE MERGEUNCONNECTEDNODES(K1,K2,JA)  ! KNOOP 1 WORDT OPGENOMEN IN KNOOP 2
  use m_netw
  use m_missing
  implicit none
  integer :: K1,K2,JA

  integer :: l
  integer :: n
  integer :: nm
  integer :: nm1
  integer :: nm2
  INTEGER :: NODLIN(200)

  JA  = 0
  NM1 = NMK(K1)
  NM2 = NMK(K2)

  N = 0
  DO NM = 1,NM2
     L  = NOD(K2)%LIN(NM)
     IF (KN(1,L) .NE. 0) THEN
        N = N + 1 ; NODLIN(N) = L
     ENDIF
  ENDDO

  DO NM = 1,NM1
     L = NOD(K1)%LIN(NM)
     IF (KN(1,L) .NE. 0) THEN
        N = N + 1 ; NODLIN(N) = L
        IF (KN(1,L) == K1) KN(1,L) = K2
        IF (KN(2,L) == K1) KN(2,L) = K2
     ENDIF
  ENDDO
  NMK(K2) = N


  if (allocated(nod(k2)%lin)) deallocate(NOD(K2)%LIN)
  ALLOCATE ( NOD(K2)%LIN(NMK(K2)) )
  NOD(K2)%LIN(1:NMK(K2)) = NODLIN(1:NMK(K2))

  if ( allocated(nod(k1)%lin) ) deallocate (NOD(K1)%lin) ! %LIN = 0  ! SPvdP: added check
  KC (K1)     = 0
  NMK(K1)     = 0
  XK (K1)     = dxymis
  ja = 1 ! nepcheck

  RETURN
  END SUBROUTINE MERGEUNCONNECTEDNODES
