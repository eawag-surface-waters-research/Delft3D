 subroutine linkstocenters(vnod,vlin)                    ! set flow node value based on flow link values scalar

 use m_flow
 use m_netw
 use m_flowgeom

 implicit none

 double precision       :: vlin(lnkx)
 real                   :: vnod(ndkx)
 integer                :: L, k1, k2, LL, Lb, Lt, kk, kb, kt, k

 vnod = 0d0

 if (kmx == 0) then
    do L   = 1,lnx
       k1  = ln  (1,L) ; k2 = ln  (2,L)
       vnod(k1) = vnod(k1) + vlin(L)*wcL(1,L)
       vnod(k2) = vnod(k2) + vlin(L)*wcL(2,L)
    enddo
 else
    do LL  = 1,lnx
       call getLbotLtop(LL,Lb,Lt)
       do L = Lb,Lt
          k1  = ln  (1,L) ; k2 = ln  (2,L)
          vnod(k1) = vnod(k1) + vlin(L)*wcL(1,LL)
          vnod(k2) = vnod(k2) + vlin(L)*wcL(2,LL)
       enddo
    enddo

    !$OMP PARALLEL DO                                          &
    !$OMP PRIVATE(kk,kb,kt,k)
    do kk = 1,ndx
       call getkbotktop(kk,kb,kt)
       do k = kt+1, kb+kmxn(kk)-1
          vnod(k) = vnod(kt)
       enddo
    enddo
    !$OMP END PARALLEL DO
 endif
 end subroutine linkstocenters
