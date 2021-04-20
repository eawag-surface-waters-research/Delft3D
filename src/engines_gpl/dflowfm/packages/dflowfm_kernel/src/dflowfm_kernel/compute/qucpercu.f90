 double precision function QucPercu(n12,L)             ! sum of (Q*uc cell centre upwind normal) at side n12 of link L
 use m_flow                                          ! advect the cell center velocities (dimension: m4/s2)
 use m_flowgeom                                      ! leaving the cell = +
 implicit none

 integer :: L                                        ! for link L,
 integer :: n12                                      ! find normal velocity components of the other links

 ! locals
 integer :: LL, LLL, LLLL                            ! for links LL,
 integer :: k12      , kup                           ! relevant node, 1 or 2, L/R
 double precision :: cs, sn, ucin, ucinx, uciny

 integer :: nn12

 double precision, external:: lin2nodx, lin2nody, nod2linx, nod2liny

 QucPercu = 0d0
 cs       = csu(L)
 sn       = snu(L)

 k12  = ln(n12,L)
 do LL   = 1, nd(k12)%lnx                            ! loop over all attached links
    LLL  = nd(k12)%ln(LL)
    LLLL = iabs(LLL)

    if ( qa(LLLL) == 0d0) then                       ! include own link

    else

!       ucin = ( ucxu(LLLL) - ucx(k12) )*cs + (ucyu(LLLL) - ucy(k12) )*sn
       nn12 = 1; if ( LLL.gt.0 ) nn12 = 2
       ucinx = lin2nodx(LLLL,nn12,ucxu(LLLL),ucyu(LLLL)) - ucx(k12)
       uciny = lin2nody(LLLL,nn12,ucxu(LLLL),ucyu(LLLL)) - ucy(k12)
       ucin  = nod2linx(L,n12,ucinx,uciny)*cs + nod2liny(L,n12,ucinx,uciny)*sn


       if (LLL > 0) then                             ! incoming link
          QucPercu = QucPercu - q1(LLLL)*ucin
       else
          QucPercu = QucPercu + q1(LLLL)*ucin
       endif

    endif

 enddo

 end function QucPercu
