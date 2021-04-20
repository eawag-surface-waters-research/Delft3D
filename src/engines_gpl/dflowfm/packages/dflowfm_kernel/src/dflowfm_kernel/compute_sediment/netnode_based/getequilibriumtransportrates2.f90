 subroutine getequilibriumtransportrates2(L, kb1, kb2, seq, wse, mx, hsk, jamin) ! get equilibrium transportrateconc seq based on bans associated with bndlink L
 use m_netw
 use m_flowgeom
 use m_sediment
 implicit none
 integer           :: L, kb1, kb2, mx, jamin                              ! Linknr, left and right ban nr, mxgr,
 double precision  :: seq(mx)  , seq2(mx)                                 ! seq(kg/m3)
 double precision  :: wse(mx)                                             ! effective fall velocity (m/s)
 double precision  :: hsk                                                 ! waterdepth, flowcell or ban
 integer           :: k1, k2, kk, n, j

 if (kb1 == 0) then        ! if bans unknown, first find them
    k1 = lncn(1,L) ; k2 = lncn(2,L)
    do kk = 1,mxban
       n = nban(1,kk)      ! net node
       if (kb1 == 0) then
          if (n == k1) then
             kb1 = kk
          endif
       endif
       if (kb2 == 0) then
          if (n == k2) then
             kb2 = kk
          endif
       endif
       if (kb1 .ne. 0 .and. kb2 .ne. 0) then
           exit
       endif
    enddo
 endif

 if (kb1 == 0) then
     kb1 = 0
 endif
 if (kb2 == 0) then
     kb2 = 0
 endif


 call getequilibriumtransportrates(kb1, seq , wse, mx, hsk)

 call getequilibriumtransportrates(kb2, seq2, wse, mx, hsk)

 if (jamin == 1) then
    do j = 1,mxgr
       seq(j) = min( seq(j), seq2(j) )
    enddo
 else
    do j = 1,mxgr
       seq(j) = max( seq(j), seq2(j) )
    enddo
 endif

 end subroutine getequilibriumtransportrates2
