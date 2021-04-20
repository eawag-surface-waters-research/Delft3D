!> Write tecplot output in already opened file
!>   note: file is closed when tim==tstop
   subroutine tecplot_out(mtecfil, tim, Lwriheader)

   use m_flowgeom
   use m_flow
   use m_flowtimes
   use m_netw
   implicit none

   integer                      :: mtecfil
   double precision, intent(in) :: tim
   logical,          intent(in) :: Lwriheader

   integer               :: i, j, k, icell
   integer, dimension(4) :: inode
   double precision, dimension(:), allocatable :: s1k, a1k, hk
   character(len=256)    :: zone

! Write header
   if ( Lwriheader ) then
      write(mtecfil, '(a)') 'Title = "output"'
      write(mtecfil, '(a)') 'Variables = x y z e h u v'
   end if

   ! Write cell centred data

!   write (mtecfil, *) 'Zone f=point, t="', trim(zone),'"', &
!        &      ', i=', ndx
!   do i = 1, ndx
!      write (mtecfil, '(3f18.7)') xz(i), yz(i), s1(i) !, u1(inod)
!   enddo

   ! Write net node data

   ! Interpolate s1 to net nodes

   allocate(s1k(1:numk))
   allocate(a1k(1:numk))
   allocate(hk (1:numk))
   s1k = 0.0d0
   a1k = 0.0d0
   hk  = 0.0d0

   ! Loop over flow nodes (flow cells/elements) and over all its net nodes (corners) = expensive!
   ! Weighted interpolation based on cell area

   do i = 1, ndxi
      do j = 1, netcell(i)%N
         k = netcell(i)%nod(j)
         s1k(k) = s1k(k) + a1(i)*s1(i)
         a1k(k) = a1k(k) + a1(i)
      enddo
   enddo

   ! Scale the interpolated water levels

   s1k = s1k / a1k

   ! Compute total depth in net nodes

   do i = 1, numk
      hk(i) = max(0.0d0, s1k(i) - zk(i))
   enddo

   ! Compute velocities to make sure the velocities in net nodes (corner points) have been computed

   call setvelocityfield()

   !
   ! Write zone information
   !
   write (zone  , '(f16.4)'      ) tim
   write (mtecfil, '(3a)'         ) 'Zone T ="', trim(zone),'"' !, numk
   write (mtecfil, '(a,i0,a,i0,a)') 'N=', numk, ', E=', ndxi,', F=FEPOINT ET=QUADRILATERAL'

   do i = 1, numk
      write (mtecfil, '(7f18.7)') xk(i), yk(i), zk(i), s1k(i), hk(i), ucnx(i), ucny(i) !, u1k(i)
   enddo

   ! Write connectivity table to file

   do icell = 1, ndxi
      do j=1,3
         inode(j) = netcell(icell)%nod(j)
      enddo
      if (netcell(icell)%N == 3) then
         inode(4) = netcell(icell)%nod(1)
      else
         inode(4) = netcell(icell)%nod(4)
      endif
      write (mtecfil, '(4(i0,3x))') inode(1), inode(2), inode(3), inode(4)
   enddo

end subroutine tecplot_out
