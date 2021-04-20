 subroutine sortnetlinks()
 use m_netw
 use sorting_algorithms, only: indexxi
 implicit none
 integer, allocatable :: n1(:), in(:), ni(:)
 integer              :: L1, k, kk, L

 if (numL == 0) return

 allocate ( n1(numL), in(numL), ni(numL) )

 L1   = numl1D+1
 do L = L1, numL
    n1(L) = lne(1,L)
 enddo

 call indexxi(numL-L1+1, n1(L1:), IN(L1:) )

 do L = 1,numl1D
    in(L) = L
 enddo

 do L = L1, numL
   in(L) = in(L) + numL1D
 enddo

 do L = 1,numL
    ni(in(L)) = L
 enddo

 do L = L1, numL
    n1(L)    = lne(1,L)
 enddo
 do L = L1, numL
    lne(1,L) = n1(in(L))
 enddo

 do L = L1, numL
    n1(L)   = lne(2,L)
 enddo
 do L = L1, numL
    lne(2,L) = n1(in(L))
 enddo

  do L = L1, numL
    n1(L)    = kn(1,L)
 enddo
 do L = L1, numL
    kn(1,L) = n1(in(L))
 enddo

 do L = L1, numL
    n1(L)   = kn(2,L)
 enddo
 do L = L1, numL
    kn(2,L) = n1(in(L))
 enddo

 do L = L1, numL
    n1(L)   = kn(3,L)
 enddo
 do L = L1, numL
    kn(3,L) = n1(in(L))
 enddo

 do L = L1, numL
    n1(L)  = lnn(L)
 enddo
 do L = L1, numL
    lnn(L) = n1(in(L))
 enddo

 do k = 1,numk
    do kk = 1,nmk(k)
       NOD(K)%LIN(kk) = ni(NOD(K)%LIN(kk))
    enddo
 enddo

 do k = 1,nump
    do kk = 1,netcell(k)%n
       netcell(K)%LIN(kk) = ni(netcell(K)%LIN(kk))
    enddo
 enddo

 deallocate(n1, in, ni)

 end  subroutine sortnetlinks
