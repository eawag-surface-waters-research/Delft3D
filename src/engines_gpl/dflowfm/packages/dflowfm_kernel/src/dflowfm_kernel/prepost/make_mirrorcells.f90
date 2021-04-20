!> make the mirror cells for open boundaries
subroutine make_mirrorcells(Nx, xe, ye, xyen, kce, ke, ierror)
   use network_data, only: numL, kn, lne, nmk, xk, yk
   implicit none

   integer,                           intent(in)    :: Nx      !< number of links
   double precision, dimension(Nx),   intent(out)   :: xe, ye  !< inner cell center coordinates
   double precision, dimension(2,Nx), intent(out)   :: xyen    !< mirror cell center coordinates
   integer,          dimension(Nx),   intent(inout) :: kce     !< flag
   integer,          dimension(Nx),   intent(out)   :: ke      !< inner cell number

   integer,                           intent(out)   :: ierror   !< error (1) or not (0)

   logical, external                                :: is_1d_boundary_candidate

   double precision, dimension(4)                   :: xx, yy  ! (half) mirror cell contour

   double precision                                 :: xci, yci, xcb, ycb, xce2, yce2

   integer                                          :: ind, k1, k2, k3, k4, L

   ierror = 1

   do L  = 1,numL                                      ! candidate points and distance tolerance of closed (u) points
      k3 = kn(1,L)  ; k4 = kn(2,L)

!      if ( abs(xk(k3)+11.5d0)+abs(xk(k4)+11.5d0) .lt. 1d-8 ) then
!         continue
!      end if

      if (kn(3,L) == 2 .and. &     ! 2D links
          (lne(1,L) == 0 .and. lne(2,L) /= 0 .or. &     ! boundary links
           lne(1,L) /= 0 .and. lne(2,L) == 0)) then
         ind = lne(1,L)+lne(2,L)                      ! i.e., the nonzero cell nr.

         call mirrorcell( ind, xk(k3), yk(k3), xk(k4), yk(k4), xci, yci, xcb, ycb, xce2, yce2, xx, yy)  ! voetje uitsteken tussen xz intern (xci) en xz rand (xcb)
         xe(L)     = xci
         ye(L)     = yci
         xyen(1,L) = xce2
         xyen(2,L) = yce2
         kce(L)    = 1
         ke(L)     = ind
      else if (kn(3,L) == 1 .or. kn(3,L) == 6) then                      ! 1D links
         k1 = k3 ; k2 = k4
         if (is_1d_boundary_candidate(L,1)) then
            xe(L)     = xk(k1)
            ye(L)     = yk(k1)
            xyen(1,L) = 2d0*xk(k1) - xk(k2)
            xyen(2,L) = 2d0*yk(k1) - yk(k2)
            kce(L)    = 1
            ke(L)     = -lne(1,L)
         else if (is_1d_boundary_candidate(L,2)) then
            xe(L)     = xk(k2)
            ye(L)     = yk(k2)
            xyen(1,L) = 2d0*xk(k2) - xk(k1)
            xyen(2,L) = 2d0*yk(k2) - yk(k1)
            kce(L)    = 1
            ke(L)     = -lne(2,L)
         endif
      endif
   enddo

   ierror = 0
1234 continue

   return
end subroutine make_mirrorcells
