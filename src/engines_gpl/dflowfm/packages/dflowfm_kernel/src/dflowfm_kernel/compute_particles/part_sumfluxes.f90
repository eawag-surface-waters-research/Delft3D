subroutine part_sumfluxes(q1,Dts)
   use m_particles
   use m_partmesh
   use m_flowgeom, only: Lnx
   implicit none

   double precision, dimension(Lnx), intent(in) :: q1  !< fluxes
   double precision,                 intent(in) :: Dts !< time interval

   integer :: L

   do L=1,Lnx
      qpart(L) = qpart(L) + q1(L)*dts
   end do

   return
end subroutine part_sumfluxes
