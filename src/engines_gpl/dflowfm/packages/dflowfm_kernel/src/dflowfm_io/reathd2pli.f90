 subroutine reathd2pli(mthd, mout)   ! convert d3d obs file to model independent
 use m_grid
 implicit none
 integer :: mthd, mout
 double precision      :: xce, yce
 character (len = 132) :: rec
 character (len = 1 )  :: uv
 integer               :: m,n,m2,n2, mn, mx, nn, nx, i

 10 read(mthd,'(a)', end = 999) rec

 read(rec,*) m,n,m2,n2,uv

 write(mout,'(a)') 'Line'
 write(mout,'(a)') ' 2 2'

 if ( index(rec,'u') > 0 .or. index(rec,'U') > 0 ) then

     nn = min(n,n2) ; nx = max(n,n2)
     write(mout,*) xc(m,nn-1)  , yc(m,nn-1)
     do i = nn, nx
        write(mout,*) xc(m,i), yc(m,i)
     enddo

 else

     mn = min(m,m2) ; mx = max(m,m2)
     write(mout,*) xc(mn-1,n)  , yc(mn-1,n)
     do i = mn, mx
        write(mout,*) xc(i,n), yc(i,n)
     enddo

 endif

 goto 10

 999 call doclose (mthd)
     call doclose (mout)

 end subroutine reathd2pli
