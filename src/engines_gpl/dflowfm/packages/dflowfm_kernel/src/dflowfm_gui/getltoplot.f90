subroutine getLtoplot(kk,k)
 use m_flowgeom
 use m_flow
 implicit none
 integer, intent(in) :: kk
 integer, intent(out) :: k
 if (kplotfrombedorsurface == 1) then
     k = Lbot(kk) - 1 + min( kplot, kmxL(kk) )
     k = min(k, Ltop(kk) )
 else
     k = Lbot(kk) + kmxL(kk) - kplot
     k = max(k, Lbot(kk) )
 endif
 end subroutine getLtoplot
