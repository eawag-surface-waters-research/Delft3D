!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2021.                                
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

! $Id$
! $HeadURL$

subroutine setumod(jazws0)                          ! set cell center Perot velocities at nodes
                                                     ! set Perot based friction velocities umod at u point
                                                     ! set tangential velocities at u point
                                                     ! set velocity gradient at u point
                                                     ! set corner based Perot velocities

 use m_flow
 use timers
 use m_flowgeom
 use m_flowtimes
 use m_sferic
 use m_wind
 use m_ship
 use m_missing
 use m_xbeach_data, only : DR, roller, swave
 use unstruc_model, only : md_restartfile
 implicit none

 integer,intent(in):: jazws0
 ! locals
 integer           :: L, LL, k, k1, k2, k12, k3, k4, kb, n, n1, n2, nn, ks, ierr
 double precision  :: ux, uy                         ! centre or node velocity x- and y components
 double precision  :: hsi, humx                      ! inverse centre depth, max depth u points
 double precision  :: qwd,qwd1,qwd2                  !
 double precision  :: qucx, qucy
 double precision  :: duxdn, duydn, duxdt, duydt     ! normal and tangential global ux,uy gradients
 double precision  :: vicl, c11, c12, c22, wudx, bai2, sxx, syy, snn

 double precision  :: sxw, syw, sf, ac1, ac2, csl, snl, wuw, ustar, suxw, suyw, uin, suxL, suyL
 double precision  :: cs, sn, dxi2, dyi2, sucheck
 double precision  :: chezy2, hhu, rt, hmin
 double precision  :: uu,vv,uucx,uucy, ff, ds, hup, fcor, vcor
 double precision  :: dundn, dutdn, dundt, dutdt, shearvar, delty, vksag6, Cz
 double precision  :: umodLL, volu, hul, dzz, adx, hdx, huv, qL, wcxu, wcyu
 double precision, allocatable:: u1_tmp(:)

 integer           :: nw, L1, L2, kbk, k2k, Ld, Lu, kt, Lb, Lt, Lb1, Lt1, Lb2, Lt2, kb1, kb2, ntmp

 double precision  :: depumin  ! external
 double precision  :: horvic   ! external
 double precision  :: horvic3  ! external

 double precision  :: DRL, nuhroller

 double precision  :: dxiAu, vicc

 integer :: ini = 0

 integer :: ndraw
 COMMON /DRAWTHIS/ ndraw(50)

 double precision, external :: nod2linx, nod2liny, lin2nodx, lin2nody, cor2linx, cor2liny
 double precision, external :: nod2wallx, nod2wally, wall2linx, wall2liny

 call timstrt('Umod', handle_umod)
 if(jazws0==1 .and. len_trim(md_restartfile)>0) then
   ! This is the moment after the restart file is read and before the first output of the inital info.
   ! At this moment, u0 is used to compute the cell-center velocities. And hs has been computed in flow_initimestep, using s0.
    ntmp = size(u1)
    allocate(u1_tmp(ntmp))
    u1_tmp = u1
    u1     = u0
    hs     = s0 - bl
    call setucxucyucxuucyu() !reconstruct cell-center velocities
    u1     = u1_tmp
    deallocate(u1_tmp)
 else
    call setucxucyucxuucyu()
 endif
 dti = 1d0/dts

 ! set friction velocities umod, tangential velocities v and velocity gradients and windstresses

 !$OMP PARALLEL DO                           &
 !$OMP PRIVATE(L,LL,Lb,Lt,k1,k2,cs,sn,hmin,fcor,vcor)
 do LL   = lnx1D+1,lnx
    hmin = min( hs(ln(1,LL)),hs(ln(2,LL)) )

    call getLbotLtop(LL,Lb,Lt)
    cs = csu(LL)  ; sn = snu(LL) ; v(LL) = 0d0
    do L = Lb,Lt
       k1 = ln(1,L) ; k2 = ln(2,L)

       if ( jasfer3D == 1 ) then
          v(L) =      acL(LL) *(-sn*nod2linx(LL,1,ucx(k1),ucy(k1)) + cs*nod2liny(LL,1,ucx(k1),ucy(k1))) +  &
                 (1d0-acL(LL))*(-sn*nod2linx(LL,2,ucx(k2),ucy(k2)) + cs*nod2liny(LL,2,ucx(k2),ucy(k2)))
       else
          v(L) =      acl(LL) *(-sn*ucx(k1) + cs*ucy(k1) ) + &
                 (1d0-acl(LL))*(-sn*ucx(k2) + cs*ucy(k2) )
       endif
       if (kmx > 0) then
          v(LL) = v(LL) + v(L)*Au(L) ! hk: activate when needed
       endif

       if (icorio > 0) then
          ! set u tangential
          if (icorio == 4) then
                 vcor = v(L)
          else
             if ( jasfer3D == 1 ) then
                 vcor =      acL(LL) *(-sn*nod2linx(LL,1,ucxq(k1),ucyq(k1)) + cs*nod2liny(LL,1,ucxq(k1),ucyq(k1))) +  &
                        (1d0-acL(LL))*(-sn*nod2linx(LL,2,ucxq(k2),ucyq(k2)) + cs*nod2liny(LL,2,ucxq(k2),ucyq(k2)))
             else
                 vcor =      acl(LL) *(-sn*ucxq(k1) + cs*ucyq(k1) ) + &     ! continuity weighted best sofar plus depth limiting
                        (1d0-acl(LL))*(-sn*ucxq(k2) + cs*ucyq(k2) )
             endif
          endif

          if (jsferic == 1) then
             fcor = fcori(LL)
          else
             fcor = fcorio
          endif
          if (fcor .ne. 0d0) then
             if (trshcorio > 0) then
                if ( hmin < trshcorio) then
                   fcor = fcor*hmin/trshcorio
                endif
             endif
             adve(L) = adve(L) - fcor*vcor
          endif
       endif

    enddo
    if (kmx > 0) then
       if ( Au(LL) .gt. 0d0 ) then ! hk: activate if needed
           v(LL) = v(LL) / Au(LL)
       endif
    endif

 enddo

 !$OMP END PARALLEL DO

 !updvertp

ihorvic = 0
if (vicouv > 0 .or. javiusp == 1 .or. Smagorinsky > 0 .or. Elder > 0 .or. kmx > 0) then
   ihorvic = 1
endif
if (ihorvic > 0 .or. jaconveyance2D>=3 .or. ndraw(29) == 37) then
   call setcornervelocities()                        ! must be called after ucx, ucy have been set
endif
if (vicouv < 0d0) then
   ihorvic = 0
endif

if (ihorvic > 0 .or. NDRAW(29) == 37) then
  dvxc = 0 ; dvyc = 0; suu = 0
  if (kmx == 0) then

     if (istresstyp == 2 .or. istresstyp == 3) then     ! first set stressvector in cell centers

       vksag6 = vonkar*sag/6d0
       do L = lnx1D+1,lnx
          if (hu(L) > 0) then                           ! link will flow

             cs = csu(L)  ; sn = snu(L)
             k1 = ln(1,L) ; k2 = ln(2,L)

             vicL = 0d0
             if (Elder > 0d0) then                     !  add Elder
                 call getcz(hu(L), frcu(L), ifrcutp(L), Cz, L)
                 vicL = vicL + Elder * (vksag6/Cz) * ( hu(L) ) * sqrt( u1(L)*u1(L) + v(L)*v(L) )  ! vonkar*sag/(6*Cz) = 0.009
             endif

             k3 = lncn(1,L) ; k4 = lncn(2,L)

             if ( jasfer3D == 1 ) then
                duxdn = ( nod2linx(L,2,ucx(k2),ucy(k2)) - nod2linx(L,1,ucx(k1),ucy(k1)) )*dxi(L)
                duydn = ( nod2liny(L,2,ucx(k2),ucy(k2)) - nod2liny(L,1,ucx(k1),ucy(k1)) )*dxi(L)
                duxdt = ( cor2linx(L,2,ucnx(k4),ucny(k4)) - cor2linx(L,1,ucnx(k3),ucny(k3)) ) * wui(L)
                duydt = ( cor2liny(L,2,ucnx(k4),ucny(k4)) - cor2liny(L,1,ucnx(k3),ucny(k3)) ) * wui(L)
             else
                duxdn   =  ( ucx(k2) -  ucx(k1)) * dxi(L)
                duydn   =  ( ucy(k2) -  ucy(k1)) * dxi(L)
                duxdt   =  (ucnx(k4) - ucnx(k3)) * wui(L)
                duydt   =  (ucny(k4) - ucny(k3)) * wui(L)
             endif

             if (Smagorinsky > 0 .or. NDRAW(29) == 37) then               ! add Smagorinsky
                dundn    =  cs*duxdn + sn*duydn
                dutdn    = -sn*duxdn + cs*duydn
                dundt    =  cs*duxdt + sn*duydt
                dutdt    = -sn*duxdt + cs*duydt
                if ( NDRAW(29) == 37 ) then   ! plot curl
                   plotlin(L) = (dutdn - dundt)
                endif
                if (Smagorinsky > 0) then
                   shearvar = 2d0*(dundn*dundn + dutdt*dutdt + dundt*dutdn) + dundt*dundt + dutdn*dutdn

                   vicL     = vicL + Smagorinsky*Smagorinsky*sqrt(shearvar)/( dxi(L)*wui(L) )
                endif

             endif

             if (nshiptxy > 0) then
                 if (vicuship /= 0d0) then
                    vicL = vicL + vicushp(L)
                 endif
             endif

             ! JRE: add roller induced viscosity
             if ((jawave .eq. 4) .and. (swave .eq. 1) .and. (roller .eq. 1)) then
                DRL = acL(L) * DR(k1) + (1-acL(L)) * DR(k2)
                nuhroller = hu(L) * (DRL / rhomean) ** (1d0/3d0)
                vicL = max(nuhroller, vicL)
             end if

!             if (viuchk < 0.5d0) then
!                vicL = min(vicL, viuchk*dti /( dxi(L)*dxi(L) + wui(L)*wui(L) ) )
!             endif

!            viuchk: safe would be min(vol1(k1)/nd(k1)%N, vol1(k2)/nd(k2)%N) * dti / (dxi(L)*Au(L)),
!                    hence 0.2d0*min(vol1(k1),vol1(k2))... is safe up to pentagons

             if (javiusp == 1) then       ! user specified part
                 vicc = viusp(L)
             else
                 vicc = vicouv
             endif
             vicL = vicL + vicc

             if (ja_timestep_auto_visc == 0) then
                dxiAu = dxi(L)*hu(L)*wu(L)
                if ( dxiAu.gt.0d0 ) then
                   vicL = min(vicL, 0.2d0*dti*min( vol1(k1) , vol1(k2) )  / dxiAu )  ! see Tech Ref.: Limitation of Viscosity Coefficient
                endif
             endif

             vicLu(L) = vicL                       ! horizontal eddy viscosity applied in mom eq.
             viu(L) = max(0d0, vicL - vicc)        ! modeled turbulent part

             c11    = cs*cs ; c12=cs*sn ; c22=sn*sn
             suxL   = duxdn + c11*duxdn + c12*(duydn - duxdt) - c22*duydt
             suyL   = duydn + c11*duxdt + c12*(duxdn + duydt) + c22*duydn

             suxL   = suxL*vicL/wui(L)
             suyL   = suyL*vicL/wui(L)
             if (istresstyp == 3) then
                hmin  = min(hs(k1), hs(k2))
                suxL  = hmin*suxL
                suyL  = hmin*suyL
             endif

             if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
                dvxc(k1) = dvxc(k1) + lin2nodx(L,1,suxL,suyL)
                dvyc(k1) = dvyc(k1) + lin2nody(L,1,suxL,suyL)
                dvxc(k2) = dvxc(k2) - lin2nodx(L,2,suxL,suyL)
                dvyc(k2) = dvyc(k2) - lin2nody(L,2,suxL,suyL)
             else
                dvxc(k1) = dvxc(k1) + suxL
                dvyc(k1) = dvyc(k1) + suyL
                dvxc(k2) = dvxc(k2) - suxL
                dvyc(k2) = dvyc(k2) - suyL
             endif

          endif

       enddo

    else

        !$OMP PARALLEL DO                                  &
        !$OMP PRIVATE(L,k1,k2)
        do L = lnx1D+1,lnx
          if (hu(L) > 0) then                           ! link will flow
             k1 = ln(1,L) ; k2 = ln(2,L)
             if (istresstyp == 4 .or. istresstyp == 5) then             ! set stresscomponent in links right away
                suu(L) = acl(L)*horvic(1,L) + (1d0-acl(L))*horvic(2,L)
             else if (istresstyp == 6) then
                suu(L) = acl(L)*horvic3(1,L) + (1d0-acl(L))*horvic3(2,L)
             endif
          endif
        enddo
        !$OMP END PARALLEL DO

    endif

  else if (kmx > 0) then

     if (istresstyp == 2 .or. istresstyp == 3) then     ! first set stressvector in cell centers

       do LL = lnx1D+1,lnx

          if (abs(kcu(LL)) .ne. 2) cycle
          call getLbotLtop(LL,Lb,Lt)
          cs   = csu(LL)  ; sn = snu(LL)

          if (javiusp == 1) then    ! user specified part
              vicc = viusp(LL)
           else
              vicc = vicouv
          endif

          do L = Lb, Lt

             vicL = 0d0

             k1 = ln  (1,L) ; k2 = ln  (2,L)
             k3 = lncn(1,L) ; k4 = lncn(2,L)
             if ( jasfer3D == 1 ) then
                duxdn = ( nod2linx(LL,2,ucx(k2),ucy(k2)) - nod2linx(LL,1,ucx(k1),ucy(k1)) )*dxi(LL)
                duydn = ( nod2liny(LL,2,ucx(k2),ucy(k2)) - nod2liny(LL,1,ucx(k1),ucy(k1)) )*dxi(LL)
                duxdt = ( cor2linx(LL,2,ucnx(k4),ucny(k4)) - cor2linx(LL,1,ucnx(k3),ucny(k3)) ) * wui(LL)
                duydt = ( cor2liny(LL,2,ucnx(k4),ucny(k4)) - cor2liny(LL,1,ucnx(k3),ucny(k3)) ) * wui(LL)
             else
                duxdn =  ( ucx(k2) -  ucx(k1)) * dxi(LL)
                duydn =  ( ucy(k2) -  ucy(k1)) * dxi(LL)
                duxdt =  (ucnx(k4) - ucnx(k3)) * wui(LL)
                duydt =  (ucny(k4) - ucny(k3)) * wui(LL)
             endif


             if (Smagorinsky > 0 .or. NDRAW(29) == 37) then               ! add Smagorinsky
                dundn    =  cs*duxdn + sn*duydn
                dutdn    = -sn*duxdn + cs*duydn
                dundt    =  cs*duxdt + sn*duydt
                dutdt    = -sn*duxdt + cs*duydt
                if ( NDRAW(29) == 37 .and. L-Lb+1 == kplot ) then         ! plot curl
                   plotlin(LL) = (dutdn - dundt)
                endif
                if (Smagorinsky > 0) then
                   shearvar = 2d0*(dundn*dundn + dutdt*dutdt + dundt*dutdn) + dundt*dundt + dutdn*dutdn
                   vicL     = vicL + Smagorinsky*Smagorinsky*sqrt(shearvar)/( dxi(LL)*wui(LL) )
                endif
             endif

             vicL = vicL + vicc

             if (javiuplus3D > 0) then
                vicL = vicL + vicwwu(L)
             endif

             if (nshiptxy > 0) then
                 if (vicuship /= 0d0) then
                    vicL = vicL + vicushp(LL)
                 endif
             endif

             if (ja_timestep_auto_visc == 0) then
                dxiAu = dxi(LL)*Au(L)
                if ( dxiAu.gt.0d0 ) then
                   vicL = min(vicL, 0.2d0*dti*min( vol1(k1) , vol1(k2) )  / dxiAu )
                endif
             endif

             vicLu(L) = vicL                       ! horizontal eddy viscosity applied in mom eq.
             viu(L) = max(0d0, vicL - vicc)        ! modeled turbulent part

             c11    = cs*cs ; c12=cs*sn ; c22=sn*sn
             suxL   = duxdn + c11*duxdn + c12*(duydn - duxdt) - c22*duydt
             suyL   = duydn + c11*duxdt + c12*(duxdn + duydt) + c22*duydn

             suxL   = suxL*vicL/wui(LL)
             suyL   = suyL*vicL/wui(LL)
             if (istresstyp == 3) then
                hmin  = min( zws(k1)-zws(k1-1), zws(k2)-zws(k2-1)  )
                suxL  = hmin*suxL
                suyL  = hmin*suyL
             endif

             if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
                dvxc(k1) = dvxc(k1) + lin2nodx(LL,1,suxL,suyL)
                dvyc(k1) = dvyc(k1) + lin2nody(LL,1,suxL,suyL)
                dvxc(k2) = dvxc(k2) - lin2nodx(LL,2,suxL,suyL)
                dvyc(k2) = dvyc(k2) - lin2nody(LL,2,suxL,suyL)
             else
                dvxc(k1) = dvxc(k1) + suxL
                dvyc(k1) = dvyc(k1) + suyL
                dvxc(k2) = dvxc(k2) - suxL
                dvyc(k2) = dvyc(k2) - suyL
             endif

          enddo

       enddo

     endif
   endif

 endif

 if (ihorvic > 0) then

     if (istresstyp == 2 .or. istresstyp == 3 ) then

        if (kmx == 0) then
          !$OMP PARALLEL DO                       &
          !$OMP PRIVATE(L,k1,k2,huv)

         do L = lnx1D+1,lnx
             if (hu(L) > 0) then                           ! link will flow
                 k1 = ln(1,L) ; k2 = ln(2,L)
                 huv    = 0.5d0*( hs(k1) + hs(k2) )         ! *huvli(L)
                 if (huv > epshu ) then

                    if ( jasfer3D == 1 ) then
                       suu(L) =      acl(L)  * bai(k1)*( csu(L)*nod2linx(L,1,dvxc(k1),dvyc(k1)) + snu(L)*nod2liny(L,1,dvxc(k1),dvyc(k1)) ) +    &
                                (1d0-acl(L)) * bai(k2)*( csu(L)*nod2linx(L,2,dvxc(k2),dvyc(k2)) + snu(L)*nod2liny(L,2,dvxc(k2),dvyc(k2)) )
                    else
                       suu(L) = acl(L)     *bai(k1)*( csu(L)*dvxc(k1) + snu(L)*dvyc(k1)  ) +  &
                               (1d0-acl(L))*bai(k2)*( csu(L)*dvxc(k2) + snu(L)*dvyc(k2)  )
                    endif

                    if (istresstyp == 3) then
                        suu(L) = suu(L) / huv
                    endif
                 endif
             endif
          enddo
          !$OMP END PARALLEL DO
        else

          !$OMP PARALLEL DO                       &
          !$OMP PRIVATE(LL,kb1,kb2,Lb,Lt,L,k1,k2,huv)

           do LL = lnx1D+1,lnx
              if (hu(LL) > 0d0) then
                 kb1 = ln(1,LL) ; kb2 = ln(2,LL)
                 call getLbotLtop(LL,Lb,Lt)
                 do L = Lb,Lt
                    k1 = ln(1,L) ; k2 = ln(2,L)
                    huv    = 0.5d0 * ( (zws(k1)-zws(k1-1)) + (zws(k2)-zws(k2-1) ) )
                    if (huv > epshu) then
                       if ( jasfer3D == 1 ) then
                          suu(L) =      acl(LL)  * bai(kb1)*( csu(LL)*nod2linx(LL,1,dvxc(k1),dvyc(k1)) + snu(LL)*nod2liny(LL,1,dvxc(k1),dvyc(k1)) ) +    &
                                   (1d0-acl(LL)) * bai(kb2)*( csu(LL)*nod2linx(LL,2,dvxc(k2),dvyc(k2)) + snu(LL)*nod2liny(LL,2,dvxc(k2),dvyc(k2)) )
                       else
                          suu(L) = acl(LL)     *bai(kb1)*( csu(LL)*dvxc(k1) + snu(LL)*dvyc(k1)  ) +  &
                                  (1d0-acl(LL))*bai(kb2)*( csu(LL)*dvxc(k2) + snu(LL)*dvyc(k2)  )
                       endif

                       if (istresstyp == 3) then
                          suu(L) = suu(L)/huv
                       endif
                    endif
                 enddo
             endif
          enddo

           !$OMP END PARALLEL DO

        endif


    endif

    do nw  = 1,mxwalls
       k1  = walls(1,nw)                              ! waterlevel point on the inside
       k3  = walls(2,nw)                              ! first corner
       k4  = walls(3,nw)                              ! second corner
       L1  = walls(4,nw)                              ! link attached to first corner
       L2  = walls(5,nw)                              ! link attached to second corner
       sf  = walls(6,nw)                              ! ustarfactor, ustar=sf*us
       cs  = walls(7,nw)                              ! sux = -cs*ustar
       sn  = walls(8,nw)                              ! suy = -sn*ustar
       wuw = walls(9,nw)                              ! width of wall

       if (irov == 1) then                            ! partial slip
          if (kmx == 0) then
             if ( jasfer3D.eq.1 ) then
                ustar = (cs*nod2wallx(nw,ucx(k1),ucy(k1)) + sn*nod2wally(nw,ucx(k1),ucy(k1)))*sf
             else
                ustar = (cs*ucx(k1) + sn*ucy(k1))*sf
             endif
             walls(16,nw) = ustar
             suxw  = -cs*ustar*abs(ustar)*wuw*bai(k1)
             suyw  = -sn*ustar*abs(ustar)*wuw*bai(k1)
             if (L1 .ne. 0) then
                csl     = csu(L1) ; snl = snu(L1) ; ac1 = walls(10,nw)
                if ( jasfer3D.eq.1 ) then
                   suu(L1) = suu(L1) + (csl*wall2linx(nw,1,suxw,suyw) + snl*wall2liny(nw,1,suxw,suyw))*ac1
                else
                   suu(L1) = suu(L1) + (csl*suxw + snl*suyw)*ac1
                endif
             endif
             if (L2 .ne. 0) then
                csl     = csu(L2) ; snl = snu(L2) ; ac2 = walls(11,nw)
                if ( jasfer3D.eq.1 ) then
                   suu(L2) = suu(L2) + (csl*wall2linx(nw,2,suxw,suyw) + snl*wall2liny(nw,2,suxw,suyw))*ac2
                else
                   suu(L2) = suu(L2) + (csl*suxw + snl*suyw)*ac2
                endif
             endif
          else
             call getkbotktop(k1,kb,kt)
             if ( L1.ne.0 ) call getLbotLtop(L1,Lb1,Lt1)
             if ( L2.ne.0 ) call getLbotLtop(L2,Lb2,Lt2)
             do k = kb, kt
                if ( jasfer3D.eq.1 ) then
                   ustar = (cs*nod2wallx(nw,ucx(k),ucy(k)) + sn*nod2wally(nw,ucx(k),ucy(k)))*sf
                else
                   ustar = (cs*ucx(k) + sn*ucy(k))*sf
                endif
                walls(16,nw) = ustar
                suxw  = -cs*ustar*abs(ustar)*wuw*bai(k1)
                suyw  = -sn*ustar*abs(ustar)*wuw*bai(k1)
                if (L1 .ne. 0) then
                   csl     = csu(L1) ; snl = snu(L1) ; ac1 = walls(10,nw)
                   if ( jasfer3D.eq.1 ) then
                      suu(Lb1+k-kb) = suu(Lb1+k-kb) + (csl*wall2linx(nw,1,suxw,suyw) + snl*wall2liny(nw,1,suxw,suyw))*ac1
                   else
                      suu(Lb1+k-kb) = suu(Lb1+k-kb) + (csl*suxw + snl*suyw)*ac1
                   endif
                endif
                if (L2 .ne. 0) then
                   csl     = csu(L2) ; snl = snu(L2) ; ac2 = walls(11,nw)
                   if ( jasfer3D.eq.1 ) then
                      suu(Lb2+k-kb) = suu(Lb2+k-kb) + (csl*wall2linx(nw,2,suxw,suyw) + snl*wall2liny(nw,2,suxw,suyw))*ac2
                   else
                      suu(Lb2+k-kb) = suu(Lb2+k-kb) + (csl*suxw + snl*suyw)*ac2
                   endif
                endif
             enddo
          endif
       else if (irov == 2) then                      ! no slip
          if ( jasfer3D.eq.1 ) then
             ustar = (cs*nod2wallx(nw,ucx(k1),ucy(k1)) + sn*nod2wally(nw,ucx(k1),ucy(k1)))
          else
             ustar = (cs*ucx(k1) + sn*ucy(k1))          ! component parallel to wall
          endif

          walls(16,nw) = 0d0

          if (javiusp == 1) then
              vicl = viusp(L)
          else
              vicl = vicouv
          endif

          delty  = ba(k1)/wuw                        ! cell area / wall width is distance between internal point and mirror point
          delty  = 0.5d0*delty
          suxw   = -(cs*ustar*vicl/delty)*wuw*bai(k1)
          suyw   = -(sn*ustar*vicl/delty)*wuw*bai(k1)
          if (L1 .ne. 0) then
             csl     = csu(L1) ; snl = snu(L1) ; ac1 = walls(10,nw)
             if ( jasfer3D.eq.1 ) then
                suu(L1) = suu(L1) + (csl*wall2linx(nw,1,suxw,suyw) + snl*wall2liny(nw,1,suxw,suyw))*ac1
             else
                suu(L1) = suu(L1) + (csl*suxw + snl*suyw)*ac1
             endif
          endif
          if (L2 .ne. 0) then
             csl     = csu(L2) ; snl = snu(L2) ; ac2 = walls(11,nw)
             if ( jasfer3D.eq.1 ) then
                suu(L2) = suu(L2) + (csl*wall2linx(nw,2,suxw,suyw) + snl*wall2liny(nw,2,suxw,suyw))*ac2
             else
                suu(L2) = suu(L2) + (csl*suxw + snl*suyw)*ac2
             endif
          endif
       else if (irov == 0) then                     ! free slip
          if ( jasfer3D.eq.1 ) then
             walls(16,nw) = (cs*nod2wallx(nw,ucx(k1),ucy(k1)) + sn*nod2wally(nw,ucx(k1),ucy(k1)))
             walls(16,nw) = cs*ucx(k1) + sn*ucy(k1)
          endif
       endif

    enddo

    if ( izbndpos.eq.0 ) then
       do L = lnxi+1,lnx  ! quick fix for open boundaries
          suu(L) = 2d0*suu(L)
       enddo
    end if

    adve = adve - suu

 endif

 call timstop(handle_umod)


 end subroutine setumod
