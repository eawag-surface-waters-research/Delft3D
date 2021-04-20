 double precision function Qsum(k)                   ! sum of Q out of k (m3/s)
 use m_flow
 use m_flowgeom
 implicit none

 integer :: k                                        ! for node k,

 ! locals
 integer :: LL, LLL, LLLL                            ! for links LL,

 Qsum = 0d0

 do LL   = 1, nd(k)%lnx                              ! loop over all attached links
    LLL  = nd(k)%ln(LL)
    LLLL = iabs(LLL)

    if ( q1(LLLL) == 0d0 ) then                      ! skip, this is link L itself, net result = 0

    else if (LLL > 0) then                           ! incoming link
       Qsum = Qsum - q1(LLLL)
    else
       Qsum = Qsum + q1(LLLL)
    endif

 enddo

 end function Qsum
