!> rearrange netnodes for spherical, periodic coordinates
!>    net nodes at the left are preferred
subroutine rearrange_worldmesh(xboundmin, xboundmax)
   use m_sferic
   use network_data
   implicit none

   double precision, intent(in) :: xboundmin, xboundmax  !< mesh bounding box x-coordinates

   integer                      :: k

   if ( jsferic.eq.1 .and. xboundmax-xboundmin.gt.180d0) then
      do k=1,numk
         if ( xk(k)-360d0.ge.xboundmin ) then
            xk(k) = xk(k)-360d0
         end if

         if ( xk(k).lt.xboundmin ) then
            xk(k) = xk(k)+360d0
         end if
      end do
   end if

   return
end subroutine rearrange_worldmesh
