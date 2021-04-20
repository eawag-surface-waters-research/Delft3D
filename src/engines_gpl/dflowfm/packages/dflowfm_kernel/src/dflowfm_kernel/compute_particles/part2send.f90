!> copy particle data to send array worksnd
subroutine part2send(jsnd, isnd)
   use m_particles
   use m_partparallel
   use m_partitioninfo, only: ndomains
   use m_alloc
   implicit none

   integer, dimension(0:ndomains),        intent(in)  :: jsnd   !< subdomain start pointers in data arrays
   integer, dimension(jsend(ndomains)-1), intent(in)  :: isnd   !< particle numbers

   integer                                            :: i, j, kother
   integer                                            :: numsend, numnew

   numsend = jsnd(ndomains)-1

   if ( numsend.gt.0 ) then
!     realloc
      if ( NDIM.gt.ubound(worksnd,1) .or. numsend.gt.ubound(worksnd,2) ) then
         numnew = 1+int(1.2d0*dble(numsend))
         call realloc(worksnd, (/ NDIM,numnew /), keepExisting=.false.)
      end if

!     fill data arrays
      do j=1,numsend
         i=isnd(j)

         worksnd(INDX_XPART,j) = xpart(i)
         worksnd(INDX_YPART,j) = ypart(i)
         worksnd(INDX_DTREM,j) = dtremaining(i)
         worksnd(INDX_IGLOB,j) = dble(iglob(i))
         kother = icellother(kpart(i))
         if ( kother.gt.0 ) then ! will be send to other subdomain
            worksnd(INDX_KPART,j) = dble(kother)
         else  ! will not be send to other subdomain, used for backup in own subdomain
            worksnd(INDX_KPART,j) = dble(kpart(i))
         end if
         if ( INDX_ZPART.ne.0 ) then
            worksnd(6,j) = zpart(i)
         end if
      end do
   end if

   return
end subroutine part2send
