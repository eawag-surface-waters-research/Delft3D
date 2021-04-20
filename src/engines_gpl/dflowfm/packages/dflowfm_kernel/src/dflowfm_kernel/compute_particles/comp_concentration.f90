!> compute concentrations of particles (parts per unit volume) in flownodes
subroutine comp_concentration(s, nconst, iconst, c)
   use m_particles
   use m_partmesh
   use m_flowgeom, only : Ndx, ba, bl
   use m_flowparameters, only: epshs
   use m_flow, only: Ndkx
   implicit none

   double precision, dimension(Ndx),        intent(in)  :: s      !< water level
   integer,                                 intent(in)  :: nconst !< number of constituents
   integer,                                 intent(in)  :: iconst !< particle tracer constituent number
   double precision, dimension(Nconst,Ndx), intent(out) :: c      !< constituents

   integer :: i, k

   do i=1,Ndx
      c(iconst,i) = 0d0
   end do

!  count number of particles per cell
   do i=1,Npart
      k = kpart(i)
      if ( k.eq.0 ) cycle

      k = iabs(cell2nod(k))

      c(iconst,k) = c(iconst,k) + 1
   end do

!  compute concentration (parts per unit volume)
   do k=1,Ndx
      if ( s(k)-bl(k) .gt. epshs ) then
         c(iconst,k) = c(iconst,k) / (ba(k)*(s(k)-bl(k)))
      else
         c(iconst,k) = 0d0
      end if
   end do

   return
end subroutine comp_concentration
