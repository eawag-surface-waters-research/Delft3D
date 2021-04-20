      subroutine getarcinfolevel( x, y, zzz)
 use m_arcinfo
 implicit none
 double precision :: x,y,zzz
 ! locals
 integer          :: m,n

 if (mca .eq. 0) return
 m = 2 + (x - x0) / dxa
 n = 2 + (y - y0) / dxa
 zzz = d(m,n)
 end subroutine getarcinfolevel
