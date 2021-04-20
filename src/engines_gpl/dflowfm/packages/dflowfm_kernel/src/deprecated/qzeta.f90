 double precision function qzeta(n12,L)              ! average specific q in zeta point comparable to fls
 use m_flow                                          ! qdo = 0.5*(qleft+qright)
 use m_flowgeom
 implicit none

 integer :: L                                        ! for link L,
 integer :: n12                                      ! find normal velocity components of the other links

 ! locals
 integer :: LL, LLL, LLLL                            ! for links LL,
 integer :: k12, kup                                 ! relevant node, 1 or 2, L/R

 qzeta = 0d0

 k12   = ln(n12,L)
 do LL   = 1, nd(k12)%lnx                            ! loop over all attached links
    LLL  = nd(k12)%ln(LL)
    LLLL = iabs(LLL)
    qzeta = qzeta + q1(LLLL)
 enddo
 end function Qzeta
