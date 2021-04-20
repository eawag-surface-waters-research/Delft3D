subroutine getverticallyaveraged(sal,mx)
use m_flow
use m_flowgeom
Implicit none
double precision :: sal(mx)
integer          :: n, k, kb, kt, mx

do n = 1,ndx
   call getkbotktop(n,kb,kt)
   sal(n) = 0d0
   if (vol1(n) > 0) then
      do k = kb,kt
         sal(n) = sal(n) + sal(k)*vol1(k)
      enddo
      sal(n) = sal(n)/vol1(n)
   endif
enddo
end subroutine getverticallyaveraged
