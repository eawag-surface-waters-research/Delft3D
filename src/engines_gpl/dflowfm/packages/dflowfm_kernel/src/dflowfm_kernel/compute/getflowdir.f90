 subroutine getflowdir(L,iu)
 use m_flow
 use m_flowgeom
 implicit none
 integer :: L, iu, k1,k2

 if (u1(L) > 0d0) then
     iu =  1
 else if (u1(L) < 0d0) then
     iu = -1
 else
    k1 = ln(1,L) ; k2 = ln(2,L)
    if (s1(k1) > s1(k2) ) then
        iu = 1
    else
        iu = -1
    endif
 endif
 end subroutine getflowdir
