subroutine alloc_partparallel()
   use m_partmesh
   use m_partparallel
   use m_partitioninfo, only: ndomains
   use m_alloc
   use m_sferic
   implicit none

   integer :: num

   call realloc(icellother, numcells)
   call realloc(jsend,ndomains,lindex=0)
   call realloc(isend,numcells)       ! first bound

   num = 10

   call realloc(worksnd, (/ NDIM, num /)) ! first bound
   call realloc(workrecv, (/ NDIM, num /)) ! first bound

   call realloc(jpoint, ndomains, lindex=0)

   call realloc(irequest,3*ndomains)

   call realloc(jrecv,ndomains,lindex=0)

   call realloc(numsendarr, ndomains-1, lindex=0)
   call realloc(numrecvarr, ndomains-1, lindex=0)

   return
end subroutine alloc_partparallel
