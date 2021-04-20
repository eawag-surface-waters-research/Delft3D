 subroutine othercell(k1,L,k2)
 USE M_FLOWGEOM
 implicit none
 integer :: k1,k2,L
 k2 = ln(1,L) + ln(2,L) - k1
 end subroutine othercell
