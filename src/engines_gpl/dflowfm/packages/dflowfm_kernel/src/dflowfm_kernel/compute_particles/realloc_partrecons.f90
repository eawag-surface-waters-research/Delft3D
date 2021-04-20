!> (re)allocate flux coefficients et cetera
subroutine realloc_partrecons()
   use m_partmesh
   use m_partrecons
   use m_alloc
   use m_missing
   use m_sferic, only: jsferic
   implicit none

   call realloc(qe, numedges, keepExisting=.false., fill=DMISS)

   call realloc(u0x, numcells, keepExisting=.false., fill=DMISS)
   call realloc(u0y, numcells, keepExisting=.false., fill=DMISS)
   if ( jsferic.eq.1 ) then
      call realloc(u0z, numcells, keepExisting=.false., fill=DMISS)
   end if
   call realloc(alpha, numcells, keepExisting=.false., fill=DMISS)

   call realloc(ireconst, numcells+1, keepExisting=.false., fill=0)
   return
end subroutine realloc_partrecons
