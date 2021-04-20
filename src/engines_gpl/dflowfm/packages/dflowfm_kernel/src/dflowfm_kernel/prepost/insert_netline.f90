!> Insert a netline by splitting a string of connected quadrilateral cells
!! in one direction.
!!
!! The direction and start cell is determined by specifying a single 'cross'
!! link that will be split.
recursive subroutine insert_netline(xp, yp, L_)
   use m_netw
   use gridoperations

   implicit none

   double precision, intent(in) :: xp, yp !< link coordinates (used only if L_.eq.0)
   integer, intent(in)          :: L_     !< link number (set to 0 first time)

   double precision             :: zp
   double precision, parameter  :: dcostol = 0.25d0

   integer, dimension(2)        :: Lnext   ! next links in recursion
   integer                      :: Nnext   ! number of next links
      integer                      :: i, ic, ja, kk, kknext, L, N, N2Dcells
   integer                      :: ierror

   ierror = 1

!  initialization: find link
   if ( L_.eq.0 ) then
      if ( netstat /= NETSTAT_OK ) then
         call findcells(100)
      end if

      L = 0
      call islink(L, xp, yp, zp)

      if ( L.eq.0 ) goto 1234

      call teknet(0,ja) ! whipe out previous net
      call readyy('Inserting meshline', 0d0)
   else
      L = L_
   end if

   Nnext = 0
   Lnext = 0

      if ( kn(3,L).eq.2 ) then
         N2Dcells = lnn(L)
      else  ! 1D
         N2Dcells = 0
      end if

      do i=1,N2Dcells
      ic = lne(i,L)
      N = netcell(ic)%N
      if ( N.ne.4 ) cycle
      kk=1; do while ( netcell(ic)%lin(kk).ne.L .and. kk.lt.N ); kk=kk+1; end do
      if ( netcell(ic)%lin(kk).ne.L ) cycle
      kknext = kk+2; if ( kknext.gt.N ) kknext = kknext-N
      Nnext = Nnext+1
      Lnext(Nnext) = netcell(ic)%lin(kknext)
   end do

   call splitlink(0d0, 0d0, L, dcostol, 1, ierror)
   if ( ierror.ne.0 ) goto 1234
!   ja = 1
!   call confrm(' ', ja)

   do i=1,Nnext
!     proceed with links that are inside the selecting polygon
!      if ( kc(kn(1,Lnext(i))).gt.0 .and. kc(kn(2,Lnext(i))).gt.0 ) then
      if ( lc(Lnext(i)).gt.0 .and. kn(1,Lnext(i)).gt.0  .and. kn(2,Lnext(i)).gt.0 ) then  ! Lnext(i) may have been disabled/deleted in the recursion
         call insert_netline(0d0, 0d0, Lnext(i))
      else
         continue
      end if
   end do

   ierror = 0

!  error handling
1234 continue

   if ( L_.eq.0 ) then
      call readyy(' ',-1d0)
      call teknet(1,ja) ! plot new net
   end if

   return
end subroutine insert_netline
