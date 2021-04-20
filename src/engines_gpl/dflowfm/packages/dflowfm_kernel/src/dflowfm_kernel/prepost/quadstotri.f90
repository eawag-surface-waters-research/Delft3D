  SUBROUTINE quadsTOTRI()
  use m_netw
  use gridoperations
  implicit none
  double precision :: a
  integer :: k0
  integer :: k1
  integer :: k2
  integer :: k3
  integer :: k4
  integer :: l
  integer :: l12
  integer :: np
  integer :: numtri
  double precision :: r
  DOUBLE PRECISION DLENGTH

  CALL FINDcells(4)  ! quads

  L = NUMTRI
  DO NP = 1,NUMP
     K1 = netcell(NP)%NOD(1)
     K2 = netcell(NP)%NOD(2)
     K3 = netcell(NP)%NOD(3)
     K4 = netcell(NP)%NOD(4)

     CALL FINDEL(K1,K2,L12)
     A = 0 ! EA(L12)
     R = DLENGTH(K1,K2)
     CALL CONNECT(K1,K3,1,A,R)

     L  = L + 1
     K0 = 1 + (L-1)*3
     KTRI(K0)   = K1
     KTRI(K0+1) = K2
     KTRI(K0+2) = K3

     L  = L + 1
     K0 = 1 + (L-1)*3
     KTRI(K0)   = K4
     KTRI(K0+1) = K1
     KTRI(K0+2) = K3
  ENDDO
  NUMTRI = K0+2

  RETURN
  END SUBROUTINE quadsTOTRI
