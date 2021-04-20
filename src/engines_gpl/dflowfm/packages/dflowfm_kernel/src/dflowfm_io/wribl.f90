 subroutine wribl(mout)                                  ! write bottom level
 USE M_FLOWGEOM
 implicit none

 integer :: mout, k

 write(mout,'(A,I12)') 'NR of internal FLOWCELLS = ', ndxi

 do k  = 1,ndxi
    write(mout,* )  xz(k), yz(k), bl(k)
 enddo
 call doclose(mout)

 end subroutine wribl
