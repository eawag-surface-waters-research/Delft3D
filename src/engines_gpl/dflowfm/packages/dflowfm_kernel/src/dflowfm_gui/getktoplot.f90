subroutine getktoplot(kk,k)
 use m_flowgeom
 use m_flow
 implicit none
 integer, intent(in) :: kk
 integer, intent(out) :: k
 if (kplotfrombedorsurface == 1) then
     k = kbot(kk) - 1 + min( kplot, kmxn(kk) )
     k = min(k, ktop(kk) )
 else
     k = kbot(kk) + kmxn(kk) - kplot
     k = max(k, kbot(kk) )
 endif
 end subroutine getktoplot
