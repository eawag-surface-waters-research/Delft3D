 !double precision function QunPeri(n12,L)            ! sum of (Q*un face node upwind normal) at side n12 of link L
 !use m_flow                                          ! advect the corner velocities (dimension: m4/s2)
 !use m_flowgeom                                      ! leaving the cell = +
 !implicit none
 !
 !integer :: L                                        ! for link L,
 !integer :: n12                                      ! find normal velocity components of the other links
 !
 !! locals
 !integer :: LL, LLL, LLLL                            ! for links LL,
 !integer :: k12                                      ! relevant node, 1 or 2, L/R
 !double precision cs, sn, ucin, unxu, unyu, aa
 !
 !QunPeri = 0d0
 !cs      = csu(L)
 !sn      = snu(L)
 !
 !k12  = ln(n12,L)
 !
 !do LL   = 1, nd(k12)%lnx                            ! loop over all attached links
 !   LLL  = nd(k12)%ln(LL)
 !   LLLL = iabs(LLL)
 !
 !   if ( qa(LLLL) == 0d0) then                       ! include own link
 !
 !   else if ( LLL*qa(LLLL) > 0d0) then               ! only incoming
 !
 !      unxu = 0.5d0*( ucnx(lncn(1,LLLL)) + ucnx(lncn(2,LLLL)) )
 !      unyu = 0.5d0*( ucny(lncn(1,LLLL)) + ucny(lncn(2,LLLL)) )
 !
 !
 !      ucin = unxu*cs + unyu*sn  - u1(L)
 !      if (LLL > 0) then                             ! incoming link
 !          aa = acl(LLLL)
 !          QunPeri = QunPeri - qa(LLLL)*ucin
 !      else
 !          aa = 1d0-acl(LLLL)
 !          QunPeri = QunPeri + qa(LLLL)*ucin
 !      endif
 !
 !   endif
 !
 !enddo
 !
 !end function QunPeri
 subroutine QucPeripiaczek(n12,L,ai,ae,iad)          ! sum of (Q*uc cell IN centre upwind normal) at side n12 of link L
 use m_flow                                          ! advect the cell center velocities (dimension: m4/s2)
 use m_flowgeom
 use m_flowtimes                                     ! leaving the cell = +
 use m_sferic
 implicit none

 integer :: n12,L,iad                                ! for link L,
 double precision ai, ae

 ! locals
 integer :: LL, LLL, LLLL                            ! for links LL,
 integer :: k12, kup, ja                             ! relevant node, 1 or 2, L/R
 double precision :: cs, sn, ucin, cfl, tet, ucinx, uciny

 integer :: nn12

 double precision, external:: lin2nodx, lin2nody, nod2linx, nod2liny

 ai = 0d0 ; ae = 0d0
 cs      = csu(L)
 sn      = snu(L)

 k12     = ln(n12,L)
 do LL   = 1, nd(k12)%lnx                            ! loop over all attached links
    LLL  = nd(k12)%ln(LL)
    LLLL = iabs(LLL)

    if ( qa(LLLL) .ne. 0d0) then                     !

       ja = 0
       if (iad == 3) then
          ja = 1                                     ! all in odd schemes
       else if ( LLL*qa(LLLL) > 0d0 ) then
          ja = 1                                     ! incoming only otherwise
       endif

       if (ja == 1) then
          if (jasfer3D == 0) then
             ucin = ucxu(LLLL)*cs + ucyu(LLLL)*sn
          else
             nn12 = 1; if ( LLL.gt.0 ) nn12 = 2
             ucinx = lin2nodx(LLLL,nn12,ucxu(LLLL),ucyu(LLLL))
             uciny = lin2nody(LLLL,nn12,ucxu(LLLL),ucyu(LLLL))
             ucin  = nod2linx(L,n12,ucinx,uciny)*cs + nod2liny(L,n12,ucinx,uciny)*sn
          endif
          if (LLL > 0) then                          ! incoming link
             ae = ae - qa(LLLL)*ucin
             ai = ai + qa(LLLL)
          else
             ae = ae + qa(LLLL)*ucin
             ai = ai - qa(LLLL)
          endif

       endif

    endif

 enddo

 end subroutine Qucperipiaczek
