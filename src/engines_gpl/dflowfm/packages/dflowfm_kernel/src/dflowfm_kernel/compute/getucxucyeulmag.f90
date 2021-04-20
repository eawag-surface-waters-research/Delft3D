!> Computes/gets cell centered horizontal x/y velocities, either Eulerian or Lagrangian, and when requested also magnitude.
!! Centralized routine for multiple uses in output files.
subroutine getucxucyeulmag(N, ucxeulg, ucyeulg, ucmago, jaeulervel, jaucmag)
   use m_flowgeom
   use m_flow, only: ndkx, ucx, ucy
   use m_flowparameters, only: jawave
   use m_waves, only: ustokes            ! available for all wave models

   implicit none

   integer,          intent(in   ) :: N          !< Length of cell arrays (probably ndkx)
   double precision, intent(  out) :: ucxeulg(N) !< Target array in which to store x-velocities.
   double precision, intent(  out) :: ucyeulg(N) !< Target array in which to store y-velocities.
   double precision, intent(  out) :: ucmago(N)  !< Target array in which to store velocity magnitudes. May be undefined when jaucmag==0.
   integer,          intent(in   ) :: jaeulervel !< Whether or not (1/0) to compute Eulerian velocities (i.e., substract Stokes drift)
   integer,          intent(in   ) :: jaucmag    !< Whether or not (1/0) to compute velocity magnitudes.

   ! Copy ucx/ucy to ucxeulg/ucyeulg
   ! They will optionally be transformed into Eulerian velocities
   ucxeulg(1:ndkx) = ucx(1:ndkx) ; ucyeulg(1:ndkx) = ucy(1:ndkx)

   ! Transform uxy/ucy into Eulerian velocities
   if (jaeulervel==1 .and. jawave>0) then
      call getucxucyeuler(N, ucxeulg, ucyeulg)
   endif

   ! Compute magnitude for vel.vectors (either Lagr. or Eul.)
   if (jaucmag == 1) then
      call getucmag(N, ucxeulg, ucyeulg, ucmago)
   end if

end subroutine getucxucyeulmag
