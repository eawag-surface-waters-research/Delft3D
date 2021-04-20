 subroutine getkbotktop(n,kb,kt)
 use m_flow
 use m_flowgeom
 implicit none
 integer :: n,kb,kt
 if (kmx == 0) then
    kb = n       ; kt = n
 else
    kb = kbot(n) ; kt = ktop(n)
 endif
 end subroutine getkbotktop
