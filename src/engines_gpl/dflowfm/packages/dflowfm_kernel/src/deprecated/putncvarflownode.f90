subroutine putncvarflownode( ncid, idq, fnod, kx, nx, itim, jint)
use netcdf
use unstruc_netcdf
use m_flow, only: work1
implicit none

double precision, intent(in)     :: fnod(*)
integer,          intent(in)     :: ncid  ! file unit
integer,          intent(in)     :: idq   ! quantity id
integer,          intent(in)     :: kx, nx, itim, jint


integer                          :: ierr, kb, kt, k, kk

if (kx > 0) then
   do kk=1,nx
      call getkbotktop(kk,kb,kt)
      if (jint == 1) kb = kb - 1
      do k = kb,kt
         work1(k-kb+1,kk) = fnod(k)
      enddo
   enddo
   ierr = nf90_put_var(ncid, idq, work1(1:kx,1:nx), start=(/ 1, 1, itim /), count=(/ kx, nx, 1 /))
else
   ierr = nf90_put_var(ncid, idq, work1(1:kx,1:nx), start=(/ 1, 1, itim /), count=(/ kx, nx, 1 /))
endif
end subroutine putncvarflownode
