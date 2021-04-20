  !> Finds the net link number between two net nodes.
  SUBROUTINE FINDEL(K1,K2,L1)
  use m_netw
  implicit none
  integer, intent(in   ) :: K1, K2 !< The two net node numbers between which a net link is searched for.
  integer, intent(  out) :: L1 !< The shared netlink between nodes k1 and k2, or 0 when not found.

  integer :: l2
  integer :: n1
  integer :: n2

  DO N1 = 1,NMK(K1)
     L1 = NOD(K1)%LIN(N1)
     DO N2 = 1,NMK(K2)
        L2 = NOD(K2)%LIN(N2)
        IF (L1 .EQ. L2) RETURN
     ENDDO
  ENDDO
  L1 = 0
  RETURN
  END SUBROUTINE FINDEL
