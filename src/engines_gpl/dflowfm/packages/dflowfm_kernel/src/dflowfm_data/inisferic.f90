 !> initialize sferical coordinate frame
 subroutine inisferic()
 use m_sferic
 use m_physcoef
 implicit none
 double precision :: sidereal
 pi       = acos(-1d0)
 twopi    = 2d0*pi
 dg2rd    = pi/180d0
 rd2dg    = 180d0/pi
 sidereal = 23d0*3600d0 + 56d0*60d0 + 4.1d0
 omega    = twopi/sidereal
 fcorio   = 2d0*omega*sin(anglat*dg2rd)
 dy2dg    = rd2dg/ra
 end subroutine inisferic
