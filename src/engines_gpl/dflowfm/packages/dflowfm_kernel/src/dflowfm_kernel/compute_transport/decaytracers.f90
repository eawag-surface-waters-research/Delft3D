subroutine decaytracers()
use m_transport
use m_flowgeom
use m_flow
use m_flowtimes
use timers

implicit none

double precision :: decaytime
integer :: i, k

integer(4) ithndl /0/
if (timon) call timstrt ( "decaytracers", ithndl )

do i=ITRA1,ITRAN
   decaytime = decaytimetracers(i - itra1 + 1)
   if (decaytime > 0d0) then
      do k = 1,ndkx
          constituents (i,k) = constituents(i,k) / (1d0 + dts/decaytime)
      enddo
   endif
enddo

if (timon) call timstop( ithndl )
end subroutine decaytracers
