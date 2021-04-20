 subroutine addclosed_2D_walls()
 use m_flowgeom
 use m_flow
 use m_missing

 implicit none

 integer          :: n, k1
 double precision :: bl1, aa1, hh1

 do n   = 1, mxwalls
    k1  = walls(1,n)
    bl1 = walls(13,n)
    aa1 = walls(17,n)
    hh1 = s1(k1) - bl1
    a1(k1)  = a1(k1) + aa1
    if (hh1 > 0d0) then
       vol1(k1) = vol1(k1) + aa1*hh1
    endif
 enddo

 end subroutine addclosed_2D_walls
