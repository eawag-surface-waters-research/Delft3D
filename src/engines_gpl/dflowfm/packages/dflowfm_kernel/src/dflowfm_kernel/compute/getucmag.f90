!> Computes the velocity magnitude in cell centers, typically used for output only.
!! All arrays via input arguments, not via use m_flow.
subroutine getucmag(N, ucxi, ucyi, ucmago)
   use m_flowgeom, only: ndx
   use m_flow, only: kmx

   implicit none
   integer,          intent(in   ) :: N         !< Length of cell arrays (probably ndkx)
   double precision, intent(in   ) :: ucxi(N)   !< Input array containing cell centered x-velocities.
   double precision, intent(in   ) :: ucyi(N)   !< Input array containing cell centered y-velocities.
   double precision, intent(  out) :: ucmago(N) !< Output array containing cell centered velocity magnitudes.

   integer          :: kk,k,kb,kt

   !call realloc(ucmag, ndkx, keepExisting = .false.)
   ! NOTE: workx/y contain the velocity vectors, possibly corrected into Eulerian velocities (see getucxucyeuler).
   if ( kmx.gt.0 ) then
      do kk=1,ndx
         call getkbotktop(kk,kb,kt)
         do k = kb,kt
            ucmago(k) = sqrt(ucxi(k)**2 + ucyi(k)**2) ! TODO: this does not include vertical/w-component now.
         end do
      end do
   else
      do kk = 1,ndx
            ucmago(kk) = sqrt(ucxi(kk)**2 + ucyi(kk)**2)
      enddo
   end if

end subroutine getucmag
