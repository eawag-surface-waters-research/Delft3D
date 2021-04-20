 subroutine tekrailines(ncol,jaall,ITYP)
 use m_flowgeom
 USE M_FLOW
 use m_flowtimes
 use m_sferic
 use m_missing
 use unstruc_display
 implicit none
 integer          :: nx, ncol, jaall, ITYP
 integer          :: r, L, k1,k2
 double precision :: zz1, zz2, xz1, xz2
 integer          :: ja

 call setcol(ncol)
 do L = 1,lnx
    if (mod(L,200) == 0) then
       call halt2(ja)
       if (ja == 1) exit
    endif


    k1 = ln (1,L)
    k2 = ln (2,L)

    if (jaall == 1 .and. wetplot > 0d0) then
       if (hu(L) < wetplot) then !  hs(k1) < wetplot .or. hs(k2) < wetplot) then
           cycle
       endif
    endif

    zz1 = dmiss ; zz2 = dmiss
    if (ityp == 1) then
       zz1 = s1(k1)
       zz2 = s1(k2)
    else if (ityp == 2) then
       zz1 = bl(k1)
       zz2 = bl(k2)
    else if (ityp == 3) then
       zz1 = sa1(k1)
       zz2 = sa1(k2)
    else if (ityp == 4) then
       zz1 = pgrw(k1)
       zz2 = pgrw(k2)
    else if (ityp == 5) then
       zz1 = sgrw1(k1)
       zz2 = sgrw1(k2)
    else if (ityp == 6) then
       if (L <= lnx1D) then
          zz1 = dmiss ; zz2 = dmiss
          if (prof1D(3,L) < 0) then
             if ( s1m(k1) > bl(k1) + prof1D(2,L) .or. s1m(k2) > bl(k2) + prof1D(2,L)) then
                zz1 = s1m(k1)
                zz2 = s1m(k2)
             endif
          endif
       endif
    else if (ityp == 7) then
       if (L <= lnx1D) then
          if (prof1D(1,L) > 0) then
             zz1 = bl(k1) + prof1D(2,L)
             zz2 = bl(k2) + prof1D(2,L)
          else
             zz1 = bl(k1)
             zz2 = bl(k2)
          endif
       else
          zz1 = bl(k1)
          zz2 = bl(k2)
       endif
    endif

    if (zz1 == dmiss .or. zz2 == dmiss) cycle

    if (yfac > 0) then
       zz1 = zz1 + (yz(k1) - ymn)*yfac
       zz2 = zz2 + (yz(k2) - ymn)*yfac
    endif

    if (jsferic == 1) then ! jglobe
       if (abs(xz(k1) - xz(k2)) > 10d0) cycle
    endif

    xz1 = xz(k1)
    xz2 = xz(k2)

    if (abs(zz1) < 1d-6) zz1 = 0d0  ! heh heh, eindelijk. -> #@!
    if (abs(zz2) < 1d-6) zz2 = 0d0

    call movabs(xz1, zz1 )
    call  lnabs(xz2, zz2 )

 enddo

 end subroutine tekrailines
