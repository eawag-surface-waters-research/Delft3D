 double precision function QucPerpure1D(n12,L)       ! sum of (Q*uc cell centre upwind normal) at side n12 of link L
 use m_flow                                          ! advect the cell center velocities (dimension: m4/s2)
 use m_flowgeom                                      ! leaving the cell = +
 implicit none

 integer :: L                                        ! for link L,
 integer :: n12                                      ! find normal velocity components of the other links

 ! locals
 integer :: LL, LLL, LLLL                            ! for links LL,
 integer :: k12      , kup                                ! relevant node, 1 or 2, L/R
 double precision ::  ucin, ucinx, uciny, ucupin, cs, sn

 integer :: nn12

 double precision, external:: lin2nodx, lin2nody, nod2linx, nod2liny

 QucPerpure1D = 0d0
 cs           = csu(L)
 sn           = snu(L)

 k12  = ln(n12,L)
 do LL   = 1, nd(k12)%lnx                            ! loop over all attached links
    LLL  = nd(k12)%ln(LL)
    LLLL = iabs(LLL)

    if ( qa(LLLL) == 0d0) then                       ! include own link

    else

       if (nd(k12)%lnx == 2 .and. u1Du(LLLL) .ne. 0d0) then
          ucin = ucxu(LLLL)*cs + ucyu(LLLL)*sn
          ucin = sign(abs(u1Du(LLLL)), ucin) - u1(L)
       else
          ucin = ucxu(LLLL)*cs + ucyu(LLLL)*sn  - u1(L)
       endif

       if (LLL > 0) then                             ! incoming link
          QucPerpure1D = QucPerpure1D - qa(LLLL)*ucin
       else
          QucPerpure1D = QucPerpure1D + qa(LLLL)*ucin
       endif

    endif

 enddo

 end function QucPerpure1D
