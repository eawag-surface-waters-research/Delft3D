 subroutine hkcircle(x,y,r) ! plotdevice routine interacter is niet goed, zie file fout.bmp
 implicit none
 double precision :: x, y, r
 double precision :: twopi , phi
 integer :: k
 twopi = 2*acos(-1d0)
 call movabs(x+r,y)
 do k = 1,360
    phi = twopi*dble(k)/360.
    call lnabs( x+r*cos(phi), y+r*sin(phi) )
 enddo
 end subroutine hkcircle
