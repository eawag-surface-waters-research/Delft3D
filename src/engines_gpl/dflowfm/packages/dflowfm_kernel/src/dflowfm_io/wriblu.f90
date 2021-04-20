 subroutine wriblu(mout)                                ! write bottom level u points
 USE M_FLOWGEOM
 implicit none

 integer :: mout, L

 write(mout,'(A,I12)') 'NR of FLOWlinks = ', lnx

 do L  = 1,lnx
    write(mout,* )  xu(L), yu(L), blu(L)
 enddo
 call doclose(mout)

 end subroutine wriblu
