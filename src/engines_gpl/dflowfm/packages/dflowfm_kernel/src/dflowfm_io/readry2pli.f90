 subroutine readry2pli(mthd, mout)   ! convert barrier v file to model independent, barv content =  m,n,sill depth
 use m_grid
 implicit none
 integer :: mthd, mout
 double precision      :: xce, yce, z=9999d0
 character (len = 132) :: rec
 character (len = 1 )  :: uv
 integer               :: m,n,m2,n2, mn, mx, nn, nx, i

 10 read(mthd,'(a)', end = 999) rec

 read(rec,*) m,n

 write(mout,'(a)') 'Line'
 write(mout,'(a)') ' 5 3'

 write(mout,*) xc(m  ,n-1), yc(m  ,n-1), z
 write(mout,*) xc(m  ,n  ), yc(m  ,n  ), z
 write(mout,*) xc(m-1,n  ), yc(m-1,n  ), z
 write(mout,*) xc(m-1,n-1), yc(m-1,n-1), z
 write(mout,*) xc(m  ,n-1), yc(m  ,n-1), z


 goto 10

 999 call doclose (mthd)
     call doclose (mout)

 end subroutine readry2pli
