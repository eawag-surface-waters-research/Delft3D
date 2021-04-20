subroutine addlink1D(L,japerim)                        ! and add area's and volumes of 1D links
 use m_flowgeom
 use m_flow
 use m_missing
 use m_flowparameters
 use unstruc_channel_flow
 use precision_basics

 implicit none

 integer           :: japerim, L, ja, calcConv

 integer           :: k1, k2, K, LL
 double precision  :: ar1, wid1, cf1, ar2, wid2, cf2, dx1, dx2, widu, diam, perim
 double precision  :: hpr

 dx1 = 0.5d0*dx(L)
 dx2 = dx1
 k1 = ln(1,L)
 k2 = ln(2,L)

 if (dxDoubleAt1DEndNodes) then
    if (kcu(L) == 1) then
       if ( nd(k1)%lnx == 1 ) then
          dx1 = 2d0*dx1
       endif
       if ( nd(k2)%lnx == 1 ) then
          dx2 = 2d0*dx2
       endif
    endif
 endif

 if (japerim == 0) then

    calcConv = 0

    if (kcs(k1) == 1) then ! TODO: consider *also* adding storage area to the 2D side k1, if kcu(L)==5, maybe not for kcu(L)==7
       hpr = s1(k1)-bob0(1,L)
       if (hpr >= 0d0) then
          call getprof_1D(L, hpr, ar1, wid1, japerim, calcConv, perim)
          vol1(k1) = vol1(k1) + dx1*ar1
          if (hpr < epshu) then
             ! make sure A1 gets a value, by computing the profile data, using a water depth of epshu.
             call getprof_1D(L, epshu, ar1, wid1, japerim, calcConv, perim)
          endif
          a1(k1) =   a1(k1) + dx1*wid1
       endif
    endif

    if (kcs(k2) == 1) then ! TODO: consider *also* adding storage area to the 2D side k2, if kcu(L)==5, maybe not for kcu(L)==7
       hpr = s1(k2)-bob0(2,L)
       if (hpr >= 0d0) then
          call getprof_1D(L, hpr, ar2, wid2, japerim, calcConv, perim)
          vol1(k2) = vol1(k2) + dx2*ar2
          if (hpr < epshu) then
             ! make sure A1 gets a value, by computing the profile data, using a water depth of epshu.
             call getprof_1D(L, epshu, ar2, wid2, japerim, calcConv, perim)
          endif
          a1(k2) =   a1(k2) + dx2*wid2
       endif
    endif

    if (nonlin >= 2) then

       LL = L
       if (L > lnxi) then                                   ! for 1D boundary links, refer to attached link
          LL = LBND1D(L)
       endif

       if (network%loaded) then
          hpr = max(0d0,s1m(k1)-bob0(1,L))                   ! this statement is called most nr of times through waterlevel iteration
          if (hpr > 0d0) then
             call getprof_1D_min(L, hpr, ar1, wid1)
             a1m(k1)  = a1m(k1)  + dx1*wid1
             vol1(k1) = vol1(k1) - dx1*ar1
          endif

          hpr = max(0d0,s1m(k2)-bob0(2,L))                   ! this statement is called most nr of times through waterlevel iteration
          if (hpr > 0d0) then
             call getprof_1D_min(L, hpr, ar2, wid2)
             a1m(k2)  = a1m(k2)  + dx2*wid2
             vol1(k2) = vol1(k2) - dx2*ar2
          endif

       elseif (prof1D(3,LL) < 0 ) then                          ! closed
          if (kcs(k1) == 1) then
             hpr = max(0d0,s1m(k1)-bob0(1,L))                   ! this statement is called most nr of times through waterlevel iteration
             if (hpr > 0.5d0*prof1D(2,LL) ) then
                call getprof_1D_min(L, hpr, ar1, wid1)
                a1m(k1)  = a1m(k1)  + dx1*wid1
                vol1(k1) = vol1(k1) - dx1*ar1
             endif
          endif
          if (kcs(k2) == 1) then
             hpr = max(0d0,s1m(k2)-bob0(2,L))                   ! this statement is called most nr of times through waterlevel iteration
             if (hpr > 0.5d0*prof1D(2,LL) ) then
                call getprof_1D_min(L, hpr, ar2, wid2)
                a1m(k2)  = a1m(k2)  + dx2*wid2
                vol1(k2) = vol1(k2) - dx2*ar2
             endif
          endif
       endif

    endif

 else

    calcConv = 1

    ! getprof1D sets cfu
    if (hu(L) > 0) then
       call getprof_1D(L, hu(L), au(L), widu, japerim, calcConv, perim)  ! memory closeness of profiles causes this statement here instead of in setau
    endif

    ! calculate VOL1_F to be used for 1d-advection

    calcConv = 0
    if(network%loaded) then
       ! Only in case of a 1d-network, vol1 and vol1_f can be different
      if (kcs(k1) == 1) then
         ! flow volume
         hpr = s1(k1)-bob0(1,L)
         if (hpr > 0d0) then
            if (comparereal(hu(L), hpr)== 0) then
               vol1_f(k1) = vol1_f(k1) + dx1*au(L)
            else
               call getprof_1D(L, hpr, ar1, wid1, 1, calcConv, perim)
               vol1_f(k1) = vol1_f(k1) + dx1*ar1
            endif
         endif
      endif

      if (kcs(k2) == 1) then
         hpr = s1(k2)-bob0(2,L)
         if (hpr > 0d0) then
            ! flow volume
            if (comparereal(hu(L), hpr)== 0) then
               vol1_f(k2) = vol1_f(k2) + dx2*au(L)
            else
               call getprof_1D(L, hpr, ar2, wid2, 1, calcConv, perim)
               vol1_f(k2) = vol1_f(k2) + dx2*ar2
            endif
         endif
      endif
    else
       vol1_f(k1) = vol1(k1)
       vol1_f(k2) = vol1(k2)
    endif
 endif

 end subroutine addlink1D
