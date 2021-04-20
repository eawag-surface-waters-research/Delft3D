 subroutine lineinterp3(xx, yy, zz, vv, ktx, x,y,z,v,n)
 implicit none
 integer          :: ktx, n, k, ip
 double precision :: xx(0:ktx), yy(0:ktx), zz(0:ktx), vv(0:ktx)
 double precision :: x(0:n)   , y(0:n)   , z(0:n)   , v(0:n)
 double precision :: a, b


 ip = 0
 do k = 0, ktx
    do while ( xx(k) > x(ip+1) .and. ip < n - 1 )
       ip = ip + 1
    enddo
    if ( xx(k) <= x(ip) ) then
       yy(k)  =   y(ip)
       zz(k)  =   z(ip)
       vv(k)  =   v(ip)
    else if ( xx(k) > x(ip) .and. xx(k) <= x(ip+1) ) then
       a     = ( xx(k) - x(ip) ) / max( 1d-4 , x(ip+1) - x(ip) ) ; b = 1d0 - a
       yy(k) = b*y(ip) + a*y(ip+1)
       zz(k) = b*z(ip) + a*z(ip+1)
       vv(k) = b*v(ip) + a*v(ip+1)
    else
       yy(k) = y(ip+1)
       zz(k) = z(ip+1)
       vv(k) = v(ip+1)
    endif
 enddo

 end subroutine lineinterp3
