!> allocate auxiliary fluxes
subroutine alloc_auxfluxes()
   use m_particles
   use m_flowgeom, only: Ndx, Lnx
   use m_alloc
   implicit none

   if ( timestep.gt.0d0 ) then
      call realloc(sbegin, Ndx, fill=0d0, keepExisting=.false.)
      call realloc(qpart, Lnx, fill=0d0, keepExisting=.false.)
   end if

   if ( threeDtype.eq.1 ) then
      call realloc(qfreesurf, Lnx, fill=0d0, keepExisting=.false.)
   end if

   return
end subroutine alloc_auxfluxes
