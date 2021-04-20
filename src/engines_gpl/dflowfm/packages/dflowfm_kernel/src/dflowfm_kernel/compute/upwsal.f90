 double precision function upwsal(L,k12)                        ! upwind salinity
 use m_flowgeom
 use m_flow
 implicit none
 integer :: L, k12

 double precision :: cl, sl, rl, ql, qls
 integer :: k, kk, LL, LLL, ku

 cl = csu(L)  ; sl = snu(L)
 if (k12 == 2) then
     cl = -cl ; sl = -sl
 endif

 k    = ln(k12,L)

 ql   = 0
 qls  = 0
 do kk  = 1,nd(k)%lnx
    LL  = nd(k)%ln(kk)
    LLL = iabs(LL)
    ku  = ln(1,LLL)
    if (ku == k) ku = ln(2,LLL)

    rl = cl*csu(LLL) + sl*snu(LLL)
    if      (LL > 0 .and. q1(LLL) > 0) then
         if (rl > 0) then
            ql  = ql  + rl*q1(LLL)
            qls = qls + rl*q1(LLL)*sa0(ku)
         endif
    else if (LL < 0 .and. q1(LLL) < 0) then
         if (rl < 0) then
            ql  = ql  + rl*q1(LLL)
            qls = qls + rl*q1(LLL)*sa0(ku)
         endif
    endif
 enddo

 if (ql > 0) then
    upwsal = qls/ql
 else
    upwsal = sa0(k)
 endif

 end function upwsal
