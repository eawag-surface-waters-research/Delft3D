 subroutine dropk(xp,yp,idir)
 use m_polygon
 use m_flowgeom
 use m_flow
 implicit none
 double precision, intent(in) :: xp, yp
 integer,          intent(in) :: idir

 ! locals
 integer          :: L, LL, Lb, Lt

 call isFLOWLINK(xp, yp, LL)
 if (LL > 0) then
    call getLbotLtop(LL,Lb,Lt)
    do L = Lb-1, Lt
       turkin0(L) = turkin0(L) + 1d0
       turkin1(L) = turkin0(L)
    enddo
 endif
 return
 end subroutine dropk
