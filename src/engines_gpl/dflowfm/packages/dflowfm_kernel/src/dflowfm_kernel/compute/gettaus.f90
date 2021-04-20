subroutine gettaus(typout)

use m_flowgeom
use m_flow
use m_alloc

implicit none
integer, intent (in)       ::  typout   !< type of setting, 1: set czs and taus, 2: just set czs:

double precision           ::  taucurc  !< local variable for taucurrent
double precision           ::  czc      !< local variable for chezy
integer                    ::  ierr     !< Error code
integer                    ::  n        !< Counter

if (.not. allocated(czs) ) then
    call realloc(czs,  ndxi,  keepExisting = .false., fill = 0d0, stat = ierr)
else if (size(czs) < ndxi) then
    call realloc(czs,  ndxi,  keepExisting = .false., fill = 0d0, stat = ierr)
endif
if (typout == 1) then
    if (.not. allocated(taus) ) then
        call realloc(taus,  ndxi,  keepExisting = .false., fill = 0d0, stat = ierr)
    else if (size(taus) < ndxi) then
        call realloc(taus,  ndxi,  keepExisting = .false., fill = 0d0, stat = ierr)
    endif
endif

do n = 1,ndxi
   call gettau(n,taucurc,czc)
   czs(n) = czc
   if (typout == 1) then
       taus(n) = taucurc
   endif
enddo
end subroutine gettaus
