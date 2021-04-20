!> check and see if the cell is convex (1) or not (0)
integer function isconvexcell(k)

   use m_netw
   use geometry_module, only: dcosphi
   use m_sferic, only: jsferic, jasfer3D
   use m_missing, only : dxymis
   use gridoperations

   implicit none

   integer, intent(in) :: k   !< cell number

   integer                     :: i, j, ip1, ip2, N
   integer                     :: k1, k2, k3
!   integer                     :: L

   double precision            :: cosphi

   double precision, parameter :: TOL=0d-2


   isconvexcell = 1

   N = netcell(k)%N

   do i=1,N
      ip1 = i+1; if ( ip1.gt.N ) ip1 = ip1-N
      ip2 = i+2; if ( ip2.gt.N ) ip2 = ip2-N
      k1 = netcell(k)%nod(i)
      k2 = netcell(k)%nod(ip1)
      k3 = netcell(k)%nod(ip2)

      cosphi = dcosphi(xk(k1),yk(k1),xk(k2),yk(k2),xk(k2),yk(k2),xk(k3),yk(k3), jsferic, jasfer3D, dxymis)

!      if ( abs(cosphi).lt.TOL .or. abs(1d0-abs(cosphi)).lt.TOL ) cycle
      if ( abs(1d0-abs(cosphi)).lt.TOL ) cycle

!     check counterclockwise
      if ( rechtsaf(k1,k2,k3) ) then
         isconvexcell = 0
         exit
      end if
   end do

   return
end function isconvexcell
