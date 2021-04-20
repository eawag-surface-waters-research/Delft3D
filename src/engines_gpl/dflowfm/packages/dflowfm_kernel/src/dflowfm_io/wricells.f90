 subroutine wricells(mout)                       ! write flow cell surrounding netnodes
 use m_netw
 use m_flowgeom

 implicit none
 integer :: mout, n, j

 write(mout,'(A,I12)') 'NR of NETNODES           = ', numk    ! nump = ndx
 write(mout,'(A,I12)') 'NR of NETLINKS           = ', numL    ! nump = ndx
 write(mout,'(A,I12)') 'NR of internal FLOWCELLS = ', nump    ! nump = ndx
 do n = 1,nump
    write(mout,'(10I10)') netcell(n)%n, (netcell(n)%NOD(j), j=1,netcell(n)%n )
 enddo

 end subroutine wricells
