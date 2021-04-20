!> add particles from received data
subroutine recv2part(numrecv,work)
   use m_particles
   use m_partparallel
   use m_partitioninfo, only: ndomains
   use unstruc_messages
   use m_missing
   implicit none

   integer,                                   intent(in)  :: numrecv  !< number of received particles
   double precision, dimension(NDIM,numrecv), intent(in)  :: work     !< received data

   integer,          dimension(:),            allocatable :: iperm

   integer                                                :: i, ipoint, j
   integer                                                :: Nreplace

   integer                                                :: ierror

   if ( numrecv.gt.0 ) then
!     get number of existing particles that may be replaced
      Nreplace = 0
      do i=1,Npart
         if ( kpart(i).eq.0 ) then
            Nreplace = Nreplace+1
         end if
      end do

      call realloc_particles(Npart+max(numrecv-Nreplace,0), .true., ierror)

      ipoint = 1
      do j=1,numrecv
         if ( ipoint.le.Npart ) then
            do while ( kpart(ipoint).ne.0 )
               ipoint = ipoint+1
            end do
         end if

         xpart(ipoint) = work(INDX_XPART,j)
         ypart(ipoint) = work(INDX_YPART,j)
         dtremaining(ipoint) = work(INDX_DTREM,j)
         iglob(ipoint) = int(work(INDX_IGLOB,j))
         kpart(ipoint) = int(work(INDX_KPART,j))
         if ( INDX_ZPART.ne.0 ) then
            zpart(ipoint) = work(INDX_ZPART,j)
         end if

         Npart = max(Npart,ipoint)
      end do
   end if

   return
end subroutine recv2part
