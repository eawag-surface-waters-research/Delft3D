subroutine dealloc_partparallel
   use m_partparallel
   implicit none

   if ( allocated(icellother) ) deallocate(icellother)
   if ( allocated(jsend) ) deallocate(jsend)
   if ( allocated(isend) ) deallocate(isend)

   if ( allocated(worksnd) ) deallocate(worksnd)
   if ( allocated(workrecv) ) deallocate(workrecv)
   if ( allocated(jpoint) ) deallocate(jpoint)

   if ( allocated(irequest) ) deallocate(irequest)

   if ( allocated(jsend) ) deallocate(jrecv)

   if ( allocated(numsendarr) ) deallocate(numsendarr)
   if ( allocated(numrecvarr) ) deallocate(numrecvarr)

   return
end subroutine dealloc_partparallel
