!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2023.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

! 
! 

subroutine sethu(jazws0)                            ! Set upwind waterdepth hu
 use m_flowgeom                                     ! Todo: higher order + limiter, see transport
 use m_flow
 use m_flowtimes
 use m_sediment
 use m_fixedweirs
 use m_sobekdfm
 use m_sferic
 use m_missing
 use m_netw, only: xk, yk, zk
 use unstruc_model, only:md_restartfile
 use geometry_module, only: dbdistance

 implicit none

 ! locals
 integer           :: L, k1, k2, ku, kd, isg, LL, k, Ld, iup, nq, kk, ifrctyp, jazws0
 integer           :: n, kb, kb0, kt, itpbn, ng, jawet, Lb, nfw
 double precision  :: zb, hh, dtgh
 double precision  :: sup, bup, sk1, sk2, hs1, hs2, epsh, qdak
 double precision  :: hsav, hul, utp, hup
 double precision  :: huk1, huk2, hsku, sigm
 double precision  :: onet = 1d0/3d0
 double precision  :: twot = 2d0/3d0
 double precision  :: tgi  = 1d0/(2d0*9.81d0)
 double precision  :: ds, ds1, ds2, ds0, xx, hdl, bupmin, he, zcdamn, hcrest, uin, blmx, fdx
 double precision  :: h0, dzb, cz, sixth = 1d0/6d0, frcn, z00, sqcf, uuL, vhei, eup, ucxku, ucyku, vben, uLL, agwdxi

 double precision  :: Qweir_super, Qweir_sub, wu_orig
 double precision  :: weirrelax = 0.75d0
 double precision  :: Qrat, re, Edown, ucxkd, ucykd
 double precision  :: dp, d2, aa, hucrest, hunoweir, qweirsimple, ufac, efac

 double precision  :: avolk, hkruin, wsbov, wsben, d1, ewben, eweir, qvolk, qunit, hov, vov, vbov, hvolk, dte0, dtefri, qov, tol
 double precision  :: sl1, sl2, sl3, sku, hskub, hub, sigmd, hskd, hskx
 character (len=4) :: toest

 integer           :: k3, k4, itel, kuu, ku2, kku, ip , Lnu, kbd, ktd, kbd0, LLbc, kkd

 double precision  :: zw0u

 double precision, external :: dslim,  nod2linx, nod2liny

 ! SPvdP: s0 at the old time level already satisfies the boundary conditions at the old time level (see s1nod)
 !  Nevertheless, s0 at the boundary (at the old time-level) will now be filled with boundary conditions at the new time level
 if(jazws0==0 .or. len_trim(md_restartfile)==0) then
    ! if(jazws0==1 .and. len_trim(md_restartfile)>0) then s0 and s1 are read from restart file, in this case, no need to call the following subroutine
    call sets01zbnd(0, 0)                               ! set s0 on z-boundaries
 endif

 if (uniformhu > 0d0) then
    hu = uniformhu ; return
 endif

 !
 ! SPvdP: water-levels at the velocity boundaries do already satisfy the Neumann condition (see s1nod)
 !
 !do n  = 1, nbndu                                    ! velocity boundaries
 !   kb     = kbndu(1,n)
 !   k2     = kbndu(2,n)
 !   s0(kb) = s0(k2)
 !enddo

 ! adjust bobs for controllable dams
 call adjust_bobs_for_dams_and_structs()

 avolk = twot*sqrt(twot*ag)
 nfw   = 0

 do L = 1,lnx

!   for cut-cells
    if (wu(L).eq.0d0 ) then
       hu(L) = 0d0
       au(L) = 0d0
       cycle
    end if

    k1 = ln(1,L) ; k2 = ln(2,L)

    if (jazws0 == 0) then
       uuL = u1(L)
    else
       uuL = u0(L)
    endif

    if ( uuL > 0) then           ! was q1 (halfway the timestep), jazws0 assigns if start of loop or loop itself is considered
       iup = 1  ; ku = k1 ; kd = k2 ; isg =  1
    else if (uuL < 0) then
       iup = 2  ; ku = k2 ; kd = k1 ; isg = -1
    else if ( s0(k1) > s0(k2) ) then
       iup = 1  ; ku = k1 ; kd = k2 ; isg =  1
    else
       iup = 2  ; ku = k2 ; kd = k1 ; isg = -1
    endif

    sup = s0(ku)

    if (limtyphu > 0 ) then
        if (limtyphu == 21) then      ! central
           sup = 0.5d0*( s0(k1) + s0(k2) )
        else if (limtyphu == 22) then ! perot alfa
           sup = acl(L)*s0(k1) + (1d0-acl(L))*s0(k2)
        else if (limtyphu == 23) then ! regular linear interpolation
           sup = acl(L)*s0(k2) + (1d0-acl(L))*s0(k1)
        else                          ! usual limiters except 6
           if (uuL > 0) then
              ip = 0
           else
              ip = 3
           endif
           ds2 = s0(kd) - s0(ku)

           kku = klnup(1+ip,L)
           kuu = abs(kku) ; sku = dmiss
           if (kku < 0) then
              sku = s0(kuu)
           else if (kuu > 0) then
              ku2 = iabs(klnup(2+ip,L))
              if ( ku2 > 0) then
                 sl1 = slnup(1+ip,L) ; sl2  = slnup(2+ip,L)
                 sku = s0(kuu)*sl1 + s0(ku2)*sl2
              endif
           endif
           if (sku .ne. dmiss) then
              sl3 = slnup(3+ip,L)
              ds1 = (s0(ku)  - sku)*sl3
              sup = sup + dslim(ds1, ds2, limtyphu)
           endif
        endif
    endif

    !DIR$ INLINE
    call getblu_from_bob(L, iup, bup)
    if (jafullgridoutput == 1) then
       blup(L) = bup
    end if

    huL = sup-bup

    if (huL  > epshu) then

       if (ncdamsg > 0 .or. ifixedweirscheme > 0) then                           ! sethu

          if (iadv(L) == 21 .or. iadv(L) >= 23 .and. iadv(L) <= 25) then         ! weir velocity point


             if (iadv(L) >= 23 .and. iadv(L) <= 25) then                         ! undisturbed velocity as if no weir present, WAQUA like
                hunoweir  = sup - blu(L) ! bob(1,L)                              ! 23 = Rajaratnam, 24 = Tabellenboek, 25 = Villemonte
                hunoweir  = max(hunoweir, huL)
                ucxku     = ucx(ku) ; ucyku = ucy(ku)
             else
                call getucxucynoweirs(ku, ucxku, ucyku, ifixedweirscheme )
             endif

             if (iadv(L) >= 23 .and. iadv(L) <= 25) then                         ! 23 = Rajaratnam, 24 = Tabellenboek, 25 = Villemonte
                 ! uin = ucxku*csfxw(nfw) + ucyku*snfxw(nfw)
                 uin = abs(u1(L))
             else
                 uin = ucxku*csu(L) + ucyku*snu(L)                               ! semi subgrid
             endif
             vhei = 0.5d0*uin*uin / ag
             eup  = hul + vhei

             if (iadv(L) == 21 .or. iadv(L) == 23) then

                hcrest= s0(kd) - bup

                if ( hcrest < hul ) then

                    huL = hcrest

                    if (hul < twot*eup) then ! supercritical

                       hul = twot*eup

                       hup = hcrest - hul

                       if (hup < 0) then
                          adve(L) = adve(L) - isg*hup*ag*dxi(L)
                       endif

                    endif


                endif ! hcrest< hul

             endif

             if (iadv(L) == -21) then !+-21

                if (jasfer3D == 1) then
                   uin = nod2linx(L,iup,ucxku,ucyku)*csu(L) + nod2liny(L,iup,ucxku,ucyku)*snu(L)
                endif
                fdx     = 0.5d0*dxi(L)*isg
                advi(L) = advi(L) + fdx*u0(L)
                adve(L) = adve(L) - fdx*uin*uin

             else if (iadv(L) == 23) then       ! simple Rajaratnam

                 ufac    = hunoweir / huL  ! compensates for undisturbed field velocity
                 efac    = 1d0 - (1d0/ufac**2)
                 advi(L) = advi(L) + 0.5d0*dxi(L)*abs(u1(L))*ufac*ufac*efac
                 huL     = hunoweir

             else if (iadv(L) == 24 .or. iadv(L) == 25) then  !  Tabellenboek or Villemonte from WAQUA

                 nfw    =  nfxwL(L)

                 wsbov  =  sup
                 wsben  =  s0(kd)
                 hkruin = -bup

                 ! determine sill height downstream of weir
                 !
                 if (uin .ge. 0.0 ) then
                     d1 = shrxw(nfw)
                 else
                     d1 = shlxw(nfw)
                 endif

                 ! vbov   =  abs(u1(L))
                 ! vbov   =  sqrt(ucxku*ucxku + ucyku*ucyku)
                 vbov   =  abs(uin)
                 vhei   =  0.5d0*vbov*vbov / ag
                 eweir  =  max (0.000001d0, wsbov + hkruin) + vhei
                 qvolk  =  avolk*eweir**1.5d0
                 qunit  =  vbov*hunoweir

                 ! Compute energy height downstream (EWBEN)
                 vben   = qunit / max (0.000001d0,wsben - bl(kd))
                 vhei   =  0.5d0*vben*vben / ag
                 ewben  =  max (0.000001d0, wsben + hkruin) + vhei
                 ! limit downstream energy height EWBEN by upstream enegy height EWEIR
                 ewben = min(ewben, eweir)

                 ! Qunit  = abs(q1(L)) / wu(L)

                 hov    =  wsbov + hkruin
                 vov    =  qunit/hov
                 if (vov < 0.5d0 ) then
                    itel  = 0
                    hvolk = twot*eweir
                    tol   = 0.001d0 *max(0.0001d0, qunit)
                    qov   = 0d0
                    do while (itel < 100 .and. (abs(qunit - qov)) > tol )
                       itel = itel + 1
                       vov  = qunit / hov
                       hov  = max(hvolk, eweir - (vov**2)/(2d0*ag) )
                       qov  = vov*hov
                    enddo
                 endif
                 dte0   = weirdte(nfw)
                 dtefri = 0.0d0
                 call enloss(ag, d1, eweir, hkruin, hov,                   &
                             qunit, qvolk, toest, vov,                     &
                             ewben, wsbov, wsben, weirdte(nfw),            &
                             dtefri,iadv(L), crestlxw(nfw),                &
                             taludlxw(nfw), taludrxw(nfw), vegxw(nfw), testfixedweirs )
                 weirdte(nfw) = (1d0 - waquaweirthetaw)*weirdte(nfw) + waquaweirthetaw*dte0
                 !
                 ! attention total waterdepth instead of water above crest
                 if ( toest == 'volk' ) then
                     vbov = qvolk/max(hunoweir, 1d-6 )
                 endif
                 if (vbov > 1d-8) then
                     agwdxi  = ag*weirdte(nfw)*dxi(L)
                     if (kmx == 0) then
                        advi(L) = advi(L) + agwdxi/vbov        ! 1/s
                     else
                        do LL = Lbot(L), Ltop(L)
                           uLL      = max(1d-4, abs(u1(LL)))
                           advi(LL) = advi(LL) + agwdxi/uLL
                        enddo
                     endif
                     map_fixed_weir_energy_loss(L) = weirdte(nfw)
                 else
                     map_fixed_weir_energy_loss(L) = 0
                 endif
                                  
                 !if (huweirregular > 0d0) then 
                 !   if (huL < huweirregular) then  
                 !   else if (hul < 2d0*huweirregular) then 
                 !      huL = huweirregular + hunoweir*(hul - huweirregular) /  huweirregular  
                 !   else 
                 !      huL = hunoweir
                 !   endif
                 !else 
                    huL = hunoweir
                 !endif
 
             else


             endif

          endif ! kadepunt

       endif

       hu(L) = huL

    else
       hu(L) = 0d0
       au(L) = 0d0
    endif

    if (kmx > 0) then
       Lb       = Lbot(L) 
       if (hu(L) > 0d0) then

          kt      = ktop(ku)
          kb      = min ( ln0( iup,Lb ) , kt )  ! dickv, was ln
          kb0     = kb - 1                      ! kbot(ku) - 1
          Ltop(L) = Lb + kt - kb

          au(L)    = 0d0
          hu(Lb-1) = 0d0

          if ( Lb == Ltop(L) ) then                ! one layer

             LL     = Lb
             hu(LL) = hu(L)
             au(LL) = wu(L)*(hu(LL) - hu(LL-1))   ! this is only for now here, later move to addlink etc
             au(L)  = au(L) + au(LL)              ! add to integrated 2Dh layer

          else

             hsku   = zws(kt) - zws(kb0)
             if (Ltop(L) > Lb + kmxL(L) - 1) then
                call qnerror('Ltop too large',' ',' ')
             endif

            
             ! UNST-5182: The code below has only been implemented for keepzlayeringatbed == 2
             ! To be implemented for keepzlayeringatbed == 0 and 1 as well, because layer distribution is independent of value of keepzlayeringatbed?
             !
             if (layertype == 2 .and. keepzlayeringatbed == 2 ) then  ! split in a central and sigma oriented part to avoid flipflop

                ktd  = ktop(kd)
                kbd  = min ( ln0(3-iup,Lb ) , ktd )
                kbd0 = kbd - 1

                hub  = 0d0

                if (ktd == kbd) then                     

                                                         ! downwind side one layer => default upwind sigma
                    LLbc = Lb - 1                        ! => default upwind sigma 


                else if (kt - kb == ktd - kbd .and. kt - kbot(ku) + 1 <= numtopsig .and. ktd - kbot(kd) + 1 <= numtopsig) then 

                                                         ! same number of layers on both sides within numtopsig
                    LLbc = Lb - 1                        ! => default upwind sigma 

                else 
                  
                    if (ihuz == 1) then                  ! central from bed til second or first above local bob

                       do LL = Lb+1, Ltop(L)             ! search upwind cell for first layer above local bob
                          hub = zws(kb+LL-Lb) - bup
                          if (hub > 0) then
                              LLbc = LL
                              exit
                          endif
                       enddo 

                    else if (ihuz == 2) then             ! all central  

                       LLbc = Ltop(L)
                       hub  = hu(L)                     

                    else if (ihuz == 3) then             ! central from bed till highest layer with equal levels 

                       LLbc  = Ltop(L)
                       do LL = Ltop(L)-1, Lb+1, -1       ! search for highest layer with equal zws
                          if ( zws(kb+LL-Lb) > bup .and. abs( zws(kb+LL-Lb) - zws(kbd+LL-Lb) ) < 1d-10) then 
                              LLbc = LL
                              exit
                          endif
                       enddo
                       hub = zws(kb+LLbc-Lb) - bup
  
                    else if (ihuz == 4) then             ! central from bed till one below highest downwind layer, much like 3 

                       LLbc = Ltop(L)
                       do LL = Ltop(L)-1, Lb+1, -1       ! search for second layer from top on downwind side
                          if (zws(kb+LL-Lb) > bup .and. ln(3-iup,LL ) == ktd-1) then 
                             LLbc = LL
                             exit
                          endif
                       enddo
                      
                       hub = zws(kb+LLbc-Lb) - bup
     
                    endif

                    do LL  = Lb, LLbc                    ! central in lower part
                       if (ihuzcsig == 4) then 
                          sigm = dble(LL-Lb+1) / dble(LLbc-Lb+1)                                          ! fifty/fifty, .33 or so
                       else 
                          sigm = ( zws(kb+LL-Lb)  - zws(kb0)  ) / ( zws(kb+LLbc-Lb)  - zws(kb0)  )        ! sigmaup
                          if (zws(kbd+LL-Lb) - zws(kbd0) > 0d0 .and. zws(kbd+LLbc-Lb) - zws(kbd0) > 0d0) then
                              sigmd  = ( zws(kbd+LL-Lb) - zws(kbd0) ) / ( zws(kbd+LLbc-Lb) - zws(kbd0) )  ! sigmadown
                              if (ihuzcsig == 1) then 
                                  sigm = 0.5d0*(sigm + sigmd)
                              else if (ihuzcsig == 2) then 
                                  sigm = max(sigm, sigmd) 
                              else if (ihuzcsig == 3) then 
                                  sigm = min(sigm, sigmd) 
                              endif
                          endif
                       endif
                       hu(LL) = sigm*hub
                       au(LL) = wu(L)*(hu(LL) - hu(LL-1)) ! this is only for now here, later move to addlink etc
                       au(L)  = au(L) + au(LL)            ! add to integrated 2Dh layer
                    enddo

                endif

                hub = hu(L) - hub
                do LL = LLbc+1, Ltop(L)              ! upwind in upper part
                   sigm   = ( zws(kb+LL-Lb) - zws(kb+LLbc-Lb) ) / ( zws(kt) - zws(kb+LLbc-Lb) )
                   hu(LL) = hu(LLbc) + sigm*hub
                   au(LL) = wu(L)*(hu(LL) - hu(LL-1)) ! this is only for now here, later move to addlink etc
                   au(L)  = au(L) + au(LL)            ! add to integrated 2Dh layer
                enddo

             else                                     ! default: upwind sigma oriented distribution of hu(L)

                if (keepzlay1bedvol == 0) then        ! default, upwind based  

                   do LL = Lb, Ltop(L)
                      sigm   = (zws(kb+LL-Lb)-zws(kb0)) / hsku
                      hu(LL) = sigm*hu(L)
                      au(LL) = wu(L)*(hu(LL)-hu(LL-1))   ! this is only for now here, later move to addlink etc
                      au(L)  = au(L) + au(LL)            ! add to integrated 2Dh layer
                   enddo

                else if (kmxn(ku) == kmxn(kd) .and. kmxn(ku) <= numtopsig) then ! in sigma identical
                                                                                ! but only checked here 
                   do LL = Lb, Ltop(L)                                        
                      sigm   = (zws(kb+LL-Lb)-zws(kb0)) / hsku                   
                      hu(LL) = sigm*hu(L)
                      au(LL) = wu(L)*(hu(LL)-hu(LL-1))   ! this is only for now here, later move to addlink etc
                      au(L)  = au(L) + au(LL)            ! add to integrated 2Dh layer
                   enddo
 
                else                                     ! different nr of layers 

                   ktd  = ktop(kd)
                   kbd  = min ( ln0(3-iup,Lb ) , ktd )
                   hskd = zws(ktd) - max(zws(kbd-1), bl(kd))
                   if (hskd > 0d0) then                  ! downwind wet => maxtop-maxbot
                      zw0u = max(bl(ku), bl(kd)) 
                      hskx = max(zws(kt),zws(ktd)) - zw0u
                      do LL  = Lb, Ltop(L)
                         kkd   =  min(ktd, kbd+LL-Lb)
                         sigm  = (max( zws(kb+LL-Lb),zws(kkd) ) - zw0u) / hskx
                         hu(LL) = sigm*hu(L)
                         au(LL) = wu(L)*(hu(LL)-hu(LL-1)) ! this is only for now here, later move to addlink etc
                         au(L)  = au(L) + au(LL)          ! add to integrated 2Dh layer
                      enddo
                   else                                   ! downwind dry => upwind only
                      zw0u = max(zws(kb-1), bl(ku) )
                      hsku = zws(kt ) - zw0u 
                      do LL  = Lb, Ltop(L)
                         sigm  = ( zws(kb+LL-Lb) - zw0u ) / hsku 
                         hu(LL) = sigm*hu(L)
                         au(LL) = wu(L)*(hu(LL)-hu(LL-1)) ! this is only for now here, later move to addlink etc
                         au(L)  = au(L) + au(LL)          ! add to integrated 2Dh layer
                      enddo
                   endif 
                endif

             endif

          endif

          !do LL = Lb, Ltop(L)
          !   if (hu(LL) - hu(LL-1)  <= 0d0 ) then 
          !      sigm = hu(LL)
          !   endif
          !enddo

       else
          Ltop(L) = 1 ! lb - 1 ! 1 ! flag dry
       endif

    endif

 enddo


 do L = 1,lnx
    k1 = ln(1,L) ; k2 = ln(2,L)
    hsav  = max(epshs, acl(L)*hs(k1) + (1d0-acl(L))*hs(k2) )
    huvli(L) = 1d0 / hsav
 enddo

 if (lincontin == 1) then
    do L = 1,lnx
       hu(L) = -0.5d0*( bob(1,L) + bob(2,L) )
    enddo
 endif


 if (nbnd1d2d > 0) then       ! 1d2d boundary check for closed boundaries
    call sethu_1d2d()
 endif

 if (javeg > 0) then
    call setveg()
 endif

 ! Create an index array containing the wet flow links.
 call fill_onlyWetLinks()

end subroutine sethu
