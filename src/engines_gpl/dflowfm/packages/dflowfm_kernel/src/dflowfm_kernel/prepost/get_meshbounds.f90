!> get mesh bounding box coordinates (useful for spherical, periodic coordinates)
!>   2D part of the mesh only
subroutine get_meshbounds(xboundmin, xboundmax)
   use network_data
   implicit none

   double precision, intent(out) :: xboundmin, xboundmax  !< mesh bounding box x-coordinates

   double precision              :: x1, x2

   integer                       :: L, k1, k2

   xboundmin =  huge(1d0)
   xboundmax = -huge(1d0)
   do L=1,numL
      if ( kn(3,L).eq.2 ) then
         k1 = kn(1,L)
         k2 = kn(2,L)
         if ( k1.gt.0 .and. k2.gt.0 ) then   ! safety
            x1 = xk(kn(1,L))
            x2 = xk(kn(2,L))
            xboundmin = min(min(x1,x2), xboundmin)
            xboundmax = max(max(x1,x2), xboundmax)
         end if
      end if
   end do

   return
end subroutine get_meshbounds
