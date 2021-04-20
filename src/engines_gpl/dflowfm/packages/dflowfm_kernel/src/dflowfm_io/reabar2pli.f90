 subroutine reabar2pli(mthd, mout)   ! convert barrier v file to model independent, barv content =  m,n,sill depth
 use m_grid
 implicit none
 integer :: mthd, mout
 double precision      :: xce, yce,dep
 character (len = 132) :: rec
 character (len = 1 )  :: uv
 integer               :: m,n,m2,n2, mn, mx, nn, nx, i

 10 read(mthd,'(a)', end = 999) rec

 read(rec,*) m,n,dep

 write(mout,'(a)') 'Line'
 write(mout,'(a)') ' 2 2'

 if ( index(rec,'u') > 0 .or. index(rec,'U') > 0 ) then

     write(mout,*) xc(m,n-1), yc(m,n-1) ,-dep
     write(mout,*) xc(m,n)  , yc(m,n)   ,-dep

 else

     write(mout,*) xc(m-1,n), yc(m-1,n) ,-dep
     write(mout,*) xc(m,n)  , yc(m,n)   ,-dep

 endif

 goto 10

 999 call doclose (mthd)
     call doclose (mout)

 end subroutine reabar2pli
