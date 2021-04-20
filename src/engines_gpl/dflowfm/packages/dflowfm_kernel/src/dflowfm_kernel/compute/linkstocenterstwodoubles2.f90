 subroutine linkstocenterstwodoubles2(vnod,vlin,vlin2)      ! both vlin and vlin2 to vnod(1,* and vnod(2,*
 use m_flow
 use m_netw
 use m_flowgeom

 implicit none

 double precision       :: vlin(lnkx), vlin2(lnkx)
 double precision       :: vnod(2,ndkx)
 integer                :: L, k1, k2, LL, Lb, Lt, kk, kb, kt, k

 vnod = 0d0
 if (kmx == 0) then
    do L   = 1,lnx
       k1  = ln  (1,L) ; k2 = ln  (2,L)
       vnod(1,k1) = vnod(1,k1) + vlin (L)*wcL(1,L)
       vnod(1,k2) = vnod(1,k2) + vlin (L)*wcL(2,L)
       vnod(2,k1) = vnod(2,k1) + vlin2(L)*wcL(1,L)
       vnod(2,k2) = vnod(2,k2) + vlin2(L)*wcL(2,L)
    enddo
 else
    do LL  = 1,lnx
       call getLbotLtop(LL,Lb,Lt)
       do L = Lb,Lt
          k1  = ln  (1,L) ; k2 = ln  (2,L)
          vnod(1,k1) = vnod(1,k1) + vlin (L)*wcL(1,LL)
          vnod(1,k2) = vnod(1,k2) + vlin (L)*wcL(2,LL)
          vnod(2,k1) = vnod(2,k1) + vlin2(L)*wcL(1,LL)
          vnod(2,k2) = vnod(2,k2) + vlin2(L)*wcL(2,LL)
       enddo
    enddo

    !$OMP PARALLEL DO                                          &
    !$OMP PRIVATE(kk,kb,kt,k)
    do kk = 1,ndx
       call getkbotktop(kk,kb,kt)
       do k = kt+1, kb+kmxn(kk)-1
          vnod(1,k) = vnod(1,kt)
          vnod(2,k) = vnod(2,kt)
       enddo
    enddo
    !$OMP END PARALLEL DO
 endif
 end subroutine linkstocenterstwodoubles2
