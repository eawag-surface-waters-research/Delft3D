double precision function upwsalslope(L,k,ds2)          ! k is upwind cell for link L, find slope upwind of k
 use m_flowgeom                                          ! limit upwind slopes for all inflowing links
 use m_flow
 implicit none
 integer          :: L, k
 double precision :: ds2


 integer                    :: kk,ku,LL,LLL,jap
 double precision           :: ds1
 double precision, external :: dcminmod

 upwsalslope = -1d9
 if (ds2 < 0) upwsalslope = 1d9

 jap = -1
 if (ln(1,L) == k) jap = 1

 do kk = 1,nd(k)%lnx
    LLL= nd(k)%ln(kk)
    LL = iabs(LLL)
    if (LL .ne. L .and. q1(LL)*LLL > 0) then
       ku = ln(1,LL)
       if (ku == k) ku = ln(2,LL)

       ds1 = (sa0(k) - sa0(ku))*jap

       if (ds2 > 0) then
          upwsalslope = dcminmod(ds1,ds2)
       else if (ds2 < 0) then
          upwsalslope = dcminmod(ds1,ds2)
       endif
    endif
 enddo
 end function upwsalslope
