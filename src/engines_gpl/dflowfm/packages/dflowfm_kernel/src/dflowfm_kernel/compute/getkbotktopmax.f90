 subroutine getkbotktopmax(n,kb,kt,ktx)
! Variation on getkbotktop. Always returns the maximum possible layer range instead of the actual range.
 use m_flow
 use m_flowgeom
 implicit none
 integer :: n,kb,kt,ktx
 if (kmx == 0) then
    kb = n       ; kt = n       ; ktx = n
 else
    kb = kbot(n) ; kt = ktop(n) ; ktx = kb +  kmxn(n) - 1
 endif
 end subroutine getkbotktopmax
