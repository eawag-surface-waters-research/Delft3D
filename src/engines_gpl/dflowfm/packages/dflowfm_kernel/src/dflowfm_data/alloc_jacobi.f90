 subroutine alloc_jacobi(ndx,lnx)
 use m_jacobi
 use m_alloc
 integer :: ndx, lnx, ierr

 if (ndx == ndxjac .and. lnx == lnxjac) return

 if (allocated(bbi) ) then
    deallocate(bbi,db,rr)
 endif

 allocate ( bbi  (ndx) , stat = ierr)
 call aerr('bbi  (ndx)', ierr,   ndx) ; bbi = 0
 allocate ( db   (ndx) , stat = ierr)
 call aerr('db   (ndx)', ierr,   ndx) ; db  = 0
 allocate ( rr   (ndx) , stat=ierr )
 call aerr('rr   (ndx)', ierr, ndx )  ; rr  = 0

end subroutine alloc_jacobi
