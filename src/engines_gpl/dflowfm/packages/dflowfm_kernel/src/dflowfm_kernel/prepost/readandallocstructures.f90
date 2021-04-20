subroutine readandallocstructures( )
use m_strucs
use m_alloc
implicit none
integer :: i, ierr

call realloc(strhis, mxstrhis, nstru)
call aerr('strhis(mxstrhis,nstru)' ,ierr , mxstrhis*nstru )

call realloc(strhis2, mxstrhis, nstru)
call aerr('strhis2(mxstrhis,nstru)' ,ierr , mxstrhis*nstru )

mxgeneral   = 0d0
mxuniversal = 0d0

do i = 1,nstru
   if (itypstr(i) == ST_GENERAL_ST) then
      mxgeneral   = mxgeneral + 1
      ntypstr(i)  = mxgeneral
   else if (itypstr(i) == ST_UNI_WEIR) then
      mxuniversal = mxuniversal + 1
      ntypstr(i)  = mxuniversal
   endif
enddo

if (mxgeneral > 0) then
   if (allocated (generalstruc) ) deallocate (generalstruc)
   allocate (generalstruc(mxgeneral), stat=ierr)
endif

if (mxuniversal > 0) then
   if (allocated (universalstruc) ) deallocate (universalstruc)
   allocate (universalstruc(mxuniversal), stat=ierr)
endif

end subroutine readandallocstructures
