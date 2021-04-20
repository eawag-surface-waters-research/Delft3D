 subroutine triarea3D(xx,yy,zz,triarea)                      ! input: two vectors starting from zero
 implicit none
 double precision :: xx(2), yy(2), zz(2), triarea
 double precision :: d(3)
 d(1) =      yy(1)*zz(2) - yy(2)*zz(1)
 d(2) =-1d0*(xx(1)*zz(2) - xx(2)*zz(1))
 d(3) =      xx(1)*yy(2) - xx(2)*yy(1)

 triarea = 0.5d0*sqrt( d(1)*d(1) + d(2)*d(2) + d(3)*d(3) )

 end subroutine triarea3D
