 ! delete curviliniar grid outside polygon(s)
 subroutine del_grid_outside_pol()
   use m_grid
   use m_polygon
   use m_tpoly
   use m_missing
   implicit none

   type(tpoly), dimension(:),   allocatable :: pols

   integer,		 dimension(:,:), allocatable :: kn  ! grid node-based mask
   integer,		 dimension(:,:), allocatable :: kc  ! grid cell-based mask

   integer                                  :: numpols, inpol
   integer                                  :: i, j
   integer                                  :: ipol

   integer                                  :: ierror ! error (1) or not (0)

   ierror = 0
   if ( NPL.lt.2 .or. MC.lt.1 .or. NC.lt.1 ) goto 1234 ! nothing to do

   ierror = 1

!  allocate
   allocate(kn(MC,NC))
   kn = 0
   allocate(kc(MC-1,NC-1))
   kc = 0

!  convert global polygon to array of tpoly-type polygons
   call pol_to_tpoly(numpols, pols, keepExisting=.false.)

!	loop over polygons
   do ipol=1,numpols
!     mask grid points that are inside a polygon
      inpol = 0  ! do not initialize (already in pol_to_tpoly)
      do j=1,NC
         do i=1,MC
            call dbpinpol_tpoly(pols(ipol), xc(i,j),yc(i,j),inpol)
            if ( inpol.eq.1 ) then
               kn(i,j) = 1
            end if
         end do
      end do
   end do

!  mark grid cells inside oudside polygons when at least one of its nodes is inside
   do j=1,NC-1
      do i=1,MC-1
         if ( kn(i,j).eq.1 .or. kn(i+1,j).eq.1 .or. kn(i,j+1).eq.1 .or. kn(i+1,j+1).eq.1 ) then
            kc(i,j) = 1
         end if
      end do
   end do

!  mark nodes that are member of a cell inside the polygon(s)
   kn = 0
   do j=1,NC-1
      do i=1,MC-1
         if ( kc(i,j).eq.1 ) then
            kn(i,j)     = 1
            kn(i+1,j)   = 1
            kn(i,j+1)   = 1
            kn(i+1,j+1) = 1
         end if
      end do
   end do

!  remove grid cells outside polygon
   do j=1,NC
      do i=1,MC
         if ( kn(i,j).eq.0 ) then
            xc(i,j)     = DMISS
            yc(i,j)     = DMISS
         end if
      end do
   end do

   ierror = 0
1234 continue

   call dealloc_tpoly(pols)
   if ( allocated(kn) ) deallocate(kn)
   if ( allocated(kc) ) deallocate(kc)

   return
 end subroutine del_grid_outside_pol
