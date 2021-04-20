 subroutine ispointinsidecells( xz, yz, nn )         ! check if certain point is inside other cells

 use m_netw
 use geometry_module, only: pinpok
 use m_missing, only: jins, dmiss

 implicit none
 double precision :: xz, yz, x(10), y(10)
 integer          :: nn

 ! locals
 integer          :: m,n,k

 do n = 1,nump
    do m = 1,netcell(n)%n
       k = netcell(n)%NOD(m)
       x(m) = xk(k)
       y(m) = yk(k)
    enddo
    call pinpok(xz,yz,netcell(n)%n,x,y,nn, jins, dmiss)
    if (nn .ne. 0) then
       nn = n ; return
    endif
 enddo

 end subroutine ispointinsidecells
