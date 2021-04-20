!> get number of subtimesteps and delta subtimesteps
subroutine get_ndeltasteps()
   use m_flowgeom, only: Ndxi, Lnxi, Lnx, ln
   use m_flowtimes, only: dts
   use m_transport
   use timers

   implicit none

   double precision                      :: dt, dtmin
   double precision                      :: logtwo

   integer                               :: kk, LL

   double precision, external            :: get_dt

   integer(4) ithndl /0/
   if (timon) call timstrt ( "get_ndeltasteps", ithndl )

   numnonglobal = 0

!  get smallest and largest time steps
   dtmin = dtmin_transp

   if ( dtmin.ge.dts ) then
      nsubsteps = 1
      ndeltasteps = 1
   else
      logtwo = log(2d0)
      nsubsteps = max(1,2**int(log(dts/dtmin)/logtwo+0.9999d0))
      dtmin = dts/nsubsteps

!     get number of substeps
      do kk=1,Ndxi
         dt = dtmax(kk)
         if ( dt.lt.dts ) then
            ndeltasteps(kk) = min(2**int(log(dt/dtmin)/logtwo),nsubsteps)
            numnonglobal = numnonglobal+1
         else
            ndeltasteps(kk) = nsubsteps
         end if
      end do

!     fictitious boundary cells
      do LL=Lnxi+1,Lnx
         ndeltasteps(ln(1,LL)) = ndeltasteps(ln(2,LL))
      end do

!      if ( nsubsteps.gt.1 ) then
!         write(6,*) dtmin
!      end if

   end if

   if (timon) call timstop( ithndl )
   return
end subroutine get_ndeltasteps
