!> (re)allocate flux coefficients
subroutine realloc_partfluxes()
   use m_partmesh
   use m_partfluxes
   use m_alloc
   use m_missing
   implicit none

   integer :: N

   call realloc(jflux2link, numedges+1, keepExisting=.false., fill=1)
   N = jflux2link(numedges+1)-1
   call realloc(iflux2link, N,  keepExisting=.false., fill=0)
   call realloc(Aflux2link, N,  keepExisting=.false., fill=0d0)

   return
end subroutine realloc_partfluxes
