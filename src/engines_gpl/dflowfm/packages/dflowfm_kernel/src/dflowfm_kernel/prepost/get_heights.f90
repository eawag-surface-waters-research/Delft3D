!> get the grid heights from the cross spline information
subroutine get_heights()
   use m_splines
   use m_spline2curvi
   use m_missing
   use m_alloc

   implicit none

   integer                                     :: is, j, js, k, kk, ks, kks, ncs, num, numj, NsubL, NsubR
   integer                                     :: kL, kR   ! left and right neighboring splines at the cross spline w.r.t. the center spline

   integer                                     :: numnew

   double precision, dimension(Nsubmax)        :: hL, hR

   double precision, dimension(:), allocatable :: xlist, ylist

   logical                                     :: Lorient

   double precision, external                  :: splinelength_int

!  allocate
   allocate(xlist(1), ylist(1))

   do is=1,mcs
!     determine the number of control points in the spline
      call nump(is,num)
      if ( num.le.2 ) cycle   ! center splines only

      do j=1,splineprops(is)%ncs
         js = splineprops(is)%ics(j)

         call nump(js,numj)

         ncs = splineprops(js)%ncs

!        for this cross spline, find the left and right neighboring splines w.r.t. the center spline
         kL = 0
         kR = 0
         do k=1,ncs
            ks = splineprops(js)%ics(k)
            if ( ks.eq.is ) then
!               if ( k.gt.1 )   kL = splineprops(js)%ics(k-1)
!               if ( k.lt.ncs ) kR = splineprops(js)%ics(k+1)

               do kk=k-1,1,-1
                  kks = splineprops(js)%ics(kk)
                  if ( splineprops(kks)%id.eq.-ks ) then
                     kL = kks
                     exit
                  end if
               end do

               do kk=k+1,ncs
                  kks = splineprops(js)%ics(kk)
                  if ( splineprops(kks)%id.eq.-ks ) then
                     kR = splineprops(js)%ics(kk)
                     exit
                  end if
               end do

               exit
            end if
         end do

         Lorient = splineprops(is)%Lorient(j)  ! orientation of the cross spline

!        reallocate if necessary
         if ( numj.gt.ubound(xlist,1) ) then
            numnew = int(1.2d0*dble(numj))+1
            call realloc(xlist, numnew)
            call realloc(ylist, numnew)
         end if

         xlist(1:numj) = xsp(js,1:numj)
         ylist(1:numj) = ysp(js,1:numj)

         call comp_subheights(is, Lorient, numj, xlist, ylist, &
                              splineprops(js)%ncs, splineprops(js)%ics, splineprops(js)%t, splineprops(js)%cosphi,   &
                              splineprops(is)%NsubL(j), splineprops(is)%NsubR(j), splineprops(is)%hL(:,j), splineprops(is)%hR(:,j))
      end do
   end do

!  deallocate
   deallocate(xlist, ylist)

   return
end subroutine get_heights
