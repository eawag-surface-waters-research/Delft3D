 subroutine zerowaterlevel()                         ! restart without water
 use m_flow
 use m_flowgeom
 implicit none
 s0 = 0d0
 s0 = max(bl,s0)
 s1 = s0
 u0 = 0d0
 u1 = 0d0
 end subroutine zerowaterlevel
