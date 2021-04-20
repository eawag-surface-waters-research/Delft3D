 subroutine inkade(sx2,sy2,i,j)
 use m_ship
 implicit none
 integer          :: i, j
 double precision :: sx2, sy2
 i = 0 ; j = 0
 if (sx2 > xmxs) i =  1
 if (sx2 < xmns) i = -1
 if (sy2 > ymxs) j =  1
 if (sy2 < ymns) j = -1
 if (i .ne. 0 .or. j .ne. 0) then
    i = i + 1 ; i = i-1
 endif

 end subroutine inkade
