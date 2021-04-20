 subroutine lineinterp(xx, yy, ktx, x,y,n)
 implicit none
 integer          :: ktx, n, k, ip
 double precision :: xx(ktx), yy(ktx), x(n), y(n)
 double precision :: a, b


 ip = 1
 do k = 1, ktx
    do while ( xx(k) > x(ip+1) .and. ip < n-1 )
       ip = ip + 1
    enddo
    if ( xx(k) <= x(ip) ) then
       yy(k)  =  y(ip)
    else if ( xx(k) > x(ip) .and. xx(k) <= x(ip+1) ) then
       a     = ( xx(k) - x(ip) ) / max( 1d-4 , x(ip+1) - x(ip) ) ; b = 1d0 - a
       yy(k) = b*y(ip) + a*y(ip+1)
    else
       yy(k) = y(ip+1)
    endif
 enddo

 end subroutine lineinterp
