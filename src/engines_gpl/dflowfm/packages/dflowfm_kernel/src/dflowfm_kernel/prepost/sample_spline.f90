!> sample a spline
subroutine sample_spline(num, xs, ys, numref, Nr, xr, yr, ierror)
   use m_splines
   use m_alloc
   implicit none

   integer,                                     intent(in)    :: num       !< number of spline control points
   double precision, dimension(num),            intent(in)    :: xs, ys    !< spline control points coordinates
   integer,                                     intent(in)    :: numref    !< number of additional points between spline control points
   integer,                                     intent(inout) :: Nr        !< array size (in), number of sample points (out)
   double precision, dimension(Nr),             intent(out)   :: xr, yr    !< sample point coordinates
   integer,                                     intent(out)   :: ierror    !< no error (0), memory error (2) or other error (1)

   double precision, dimension(:), allocatable                :: xh2, yh2

   double precision                                           :: tn

   integer                                                    :: i, j, Nr_in

   ierror = 1

   Nr_in = Nr

   if ( num.lt.1 ) goto 1234

!  compute the number of samples
   Nr = num + (num-1)*numref

!  check array size
   if ( Nr_in.lt.Nr ) then
      ierror = 2
      goto 1234
   end if

!  allocate
   allocate(xh2(num))
   allocate(yh2(num))

   call spline(xs,num,xh2)
   call spline(ys,num,yh2)

   Nr = 0
   do i = 1,num-1
      do j = 1,numref+1
         Nr = Nr+1
         tn = (i - 1) + dble(j-1) / dble(numref+1)
         call splint(xs,xh2,num,tn,xr(Nr))
         call splint(ys,yh2,num,tn,yr(Nr))
      end do
   end do

!  add last point
   Nr = Nr+1
   tn = dble(num-1)
   call splint(xs,xh2,num,tn,xr(Nr))
   call splint(ys,yh2,num,tn,yr(Nr))

   ierror = 0
   1234 continue

!  deallocate
   if ( allocated(xh2) ) deallocate(xh2)
   if ( allocated(yh2) ) deallocate(yh2)

   return
end subroutine sample_spline
