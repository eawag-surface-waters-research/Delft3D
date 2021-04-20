!> (re)allocate partmesh data
subroutine realloc_partmesh()
   use m_partmesh
   use m_alloc
   use m_missing
   use m_sferic, only: jsferic
   implicit none

   integer :: N

   call realloc(edge2node, (/2, numedges/), fill=0, keepExisting=.false.)
   call realloc(edge2cell, (/2, numedges/), fill=0, keepExisting=.false.)
   call realloc(xnode, numnodes, fill=0d0, keepExisting=.false.)
   call realloc(ynode, numnodes, fill=0d0, keepExisting=.false.)
   if ( jsferic.eq.1 ) then
      call realloc(znode, numnodes, fill=0d0, keepExisting=.false.)
   end if

   call realloc(xzwcell, numcells, fill=DMISS, keepExisting=.false.)
   call realloc(yzwcell, numcells, fill=DMISS, keepExisting=.false.)
   if ( jsferic.eq.1 ) then
      call realloc(zzwcell, numcells, fill=DMISS, keepExisting=.false.)
   end if
   call realloc(areacell, numcells, fill=DMISS, keepExisting=.false.)

   if ( jsferic.eq.0 ) then
      call realloc(dnx, (/1, numedges/), fill=DMISS, keepExisting=.false.)
      call realloc(dny, (/1, numedges/), fill=DMISS, keepExisting=.false.)
   else
      call realloc(dnx, (/2, numedges/), fill=DMISS, keepExisting=.false.)
      call realloc(dny, (/2, numedges/), fill=DMISS, keepExisting=.false.)
      call realloc(dnz, (/2, numedges/), fill=DMISS, keepExisting=.false.)
      call realloc(dnn, (/3, numcells/), fill=DMISS, keepExisting=.false.)
   end if
   call realloc(w, numedges, fill=DMISS, keepExisting=.false.)

   call realloc(edge2link, numedges, fill=0, keepExisting=.false.)
!   call realloc(nod2cell, numcells, fill=0, keepExisting=.false.)
   call realloc(cell2nod, numcells, fill=0, keepExisting=.false.)

   call realloc(jcell2edge, numcells+1, fill=1, keepExisting=.false.)
   N = jcell2edge(numcells+1)-1
   call realloc(icell2edge, N, fill=0, keepExisting=.false.)

   return
end subroutine realloc_partmesh
