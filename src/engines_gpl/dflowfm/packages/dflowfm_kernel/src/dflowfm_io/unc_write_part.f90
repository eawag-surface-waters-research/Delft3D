!> write particles to netcdf file
subroutine unc_write_part(ifile,itime,id_parttime,id_partx,id_party,id_partz)
   use m_particles
   use netcdf
   use m_sferic
   use m_flow, only: kmx
   use geometry_module, only: cart3Dtospher
   use m_missing
   use unstruc_messages
   implicit none

   integer,                        intent(in)  :: ifile  !< output file identifier
   integer,                        intent(in)  :: itime
   integer,                        intent(in)  :: id_parttime, id_partx, id_party, id_partz

   double precision, dimension(:), allocatable :: xx, yy

   integer,          dimension(:), allocatable :: iperm

   double precision                            :: dis2

   integer                                     :: i, i0, ii, iglb
   integer                                     :: ierr, ierror

   double precision,                 parameter :: dtol = 1d-8

   integer, save :: icount=0

   ierror = 1

   icount = icount+1

   if ( icount.ge.24 ) then
      continue
   end if

!  allocate
   allocate(iperm(NpartOut))
   iperm = 0
   allocate(xx(NpartOut))
   xx = dmiss
   allocate(yy(NpartOut))
   yy = dmiss

!  order particles
   ierr = 0
   do i=1,Npart
      if ( kpart(i).eq.0 ) cycle

      iglb = iglob(i)
!     check if this global number already occured
      i0 = iperm(iglb)
      if ( i0.ne.0 ) then
!        check coordinates
         dis2 = (xpart(i)-xpart(i0))**2 + (ypart(i)-ypart(i0))**2
         if ( jsferic.ne.0 ) then
            dis2 = dis2 + (zpart(i)-zpart(i0))**2
         end if
         if ( dis2.gt.dtol ) then
            write(6,"('particle error: ', I0)") iglb
            ierr = 1
         end if
      end if
      iperm(iglb) = i
   end do
   if ( ierr.ne.0 ) goto 1234

   if ( jsferic.eq.1 ) then
      do ii=1,Npart
         i = iperm(ii)
         if ( i.gt.0 ) then
            call Cart3Dtospher(xpart(i),ypart(i),zpart(i),xx(ii),yy(ii),0d0)
         end if
      end do
   else
      do ii=1,NpartOut
         i = iperm(ii)
         if ( i.gt.0 ) then
            xx(ii) = xpart(i)
            yy(ii) = ypart(i)
         end if
      end do
   end if

   ierr = nf90_put_var(ifile, id_parttime, timepart, (/ itime /))
   if ( ierr.ne.0 ) goto 1234
   ierr = nf90_put_var(ifile, id_partx, xx, start=(/ 1,itime /), count=(/ NpartOut,1 /) )
   if ( ierr.ne.0 ) goto 1234
   ierr = nf90_put_var(ifile, id_party, yy, start=(/ 1,itime /), count=(/ NpartOut,1 /) )
   if ( ierr.ne.0 ) goto 1234

   if ( kmx.gt.0 ) then
!     particle vertical coordinate
   end if

   if ( japart.eq.1 ) call restore_particles()

   ierror = 0
1234 continue

!  deallocate
   if ( allocated(iperm) ) deallocate(iperm)
   if ( allocated(xx) ) deallocate(xx)
   if ( allocated(yy) ) deallocate(yy)

!  error handling
   if ( ierror.ne.0 ) then
      call mess(LEVEL_ERROR, 'particles output error')
   end if

   return
end subroutine unc_write_part
