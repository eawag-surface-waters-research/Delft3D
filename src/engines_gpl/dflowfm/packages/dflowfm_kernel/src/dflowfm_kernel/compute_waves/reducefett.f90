subroutine reducefett(n)
use m_waves
use m_flowgeom
use m_partitioninfo
implicit none
integer :: n, k, ierror
do k = 1,ndx
   fett(1,k)  = fetch(n,k)
   fett(2,k)  = fetdp(n,k)
enddo
call update_ghosts(ITYPE_SaLL, 2, ndx, fett, ierror)
do k = 1,ndx
   fetch(n,k) = fett(1,k)
   fetdp(n,k) = fett(2,k)
enddo
end subroutine
