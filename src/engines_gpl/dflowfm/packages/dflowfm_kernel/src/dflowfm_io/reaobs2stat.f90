 subroutine reaobs2stat(mobs, mout)   ! convert d3d obs file to model independent
 use m_grid
 implicit none
 integer :: mobs, mout
 double precision      :: xce, yce
 character (len = 132) :: rec
 character (len = 20 ) :: name
 integer               :: m,n

 10 read(mobs,'(a)', end = 999) rec

 read(rec( 1:),'(a)') name
 read(rec(21:),*    ) m,n

 xce      = 0.25d0*( xc(m-1,n) + xc(m-1,n-1) + xc(m,n) + xc(m,n-1) )
 yce      = 0.25d0*( yc(m-1,n) + yc(m-1,n-1) + yc(m,n) + yc(m,n-1) )

 write(mout,*) xce, yce, name

 goto 10

 999 call doclose (mobs)
     call doclose (mout)

 end subroutine reaobs2stat
