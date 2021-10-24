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

subroutine transport()                           ! transport for now, advect salinity and add
                                                  ! high order limited terms to uqcx, uqcy
 use m_flowgeom
 use m_flow
 use Timers
 use m_flowtimes
 use m_ship
 use m_sediment
 use m_netw, only : xk,yk,zk
 use m_flowtimes
 use m_physcoef, only : idensform, difmolsal
 use m_partitioninfo
 use m_timer
 use m_missing
 use unstruc_display, only: jaGUI
 use unstruc_messages
 use m_transport, only: NUMCONST, constituents, ISALT, ITEMP, ISED1, ISEDN, ITRA1, itraN, itrac2const

 implicit none

 integer :: L, k, ku, kd, k1, k2, kb, ierr, n, ntmx, it, jupwsal, jupq, itmax, jaimplorg, java

 integer :: L1, L2, Li, ip, is, maxit = 100, limtyp, kl1, kl2, kl2s, kku

 integer :: jalim2D, nx, k3, k4

 double precision               :: ds1, ds2, ds, sak, saku, teku, half, cf, tetaj2i, vv, tetav1, cadv, difsalw, diftemw, difsedw

 double precision               :: qst, qstu, qstd, qds, ql, qh, epssa, sasum, sasum0=0, diff, baroc, barocup, dif, dift, difs, ho

 double precision               :: ucxku, ucyku, s1ku, sl1, sl2, sl3, fi, qb, wsemx, dgrlay, dtvi, hsk, xx,yy,dmorfax, dv, aa

 double precision, allocatable, save  :: dsq(:), pp(:), pm(:), qp(:), qm(:), alf(:) ! todo kuzmin limiting jalim2D==1

 double precision, external     :: dslim, setrho, dlimitercentral

 integer                        :: j, kj, kdj, kuj, kl1j, kl2j, kbj, kij, ki, jastep, kk, kb1, kb2, n1, n2, kkua, kkub, ku2

 integer                        :: LL, Lb, Lt, kt, km, ivert, ja, m, LL1, LL2, jachange

 double precision               :: sedku(mxgr)           !< upper slope sed value                   , dimension = mxgr
 double precision               :: flx  (mxgr)           !< sed erosion flux (kg/s)                 , dimension = mxgr
 double precision               :: seq  (mxgr)           !< sed equilibrium transport rate (kg/m/s) , dimension = mxgr
 double precision               :: wse  (mxgr)           !< effective fall velocity (m/s)           , dimension = mxgr, ws*crefa=wse*seq


 double precision               :: cpuorg(3), cpunew(3), adv, adv1, hordif, qsk, qsa

 double precision               ::  a(kmxx), b(kmxx), c(kmxx), d(kmxx), e(kmxx)
 double precision               :: ta(kmxx),tb(kmxx),tc(kmxx),td(kmxx),te(kmxx)
 double precision               :: sa(kmxx),sb(kmxx),sc(kmxx),sd(kmxx),se(kmxx)


 double precision, allocatable  :: sa00(:), vold(:), cch(:), ccv(:), diagn(:) ! help arrays scalar transport

 double precision               :: dq(kmxx), samiobnd, samerr2, dsadn

 double precision               :: dfac1, dfac2, src, viL, diuspL, qdsL

 integer                        :: ierror, k3D, noadvection = 0

 double precision, allocatable  :: skmx(:)

 double precision               :: valtop

 call timstrt('Transport', handle_extra(52)) ! transport

 if ( stm_included .and. jased.ne.0 .and. jatransportmodule.eq.0 ) then
    call mess(LEVEL_FATAL, 'unstruc::transport - Please use transport module when sediment model number == 4')
 end if

 if (jasal == 0) then
    ! limtypsa = 0
    maxitverticalforestersal = 0
 endif
 if (jatem == 0) then
    limtypTM = 0 ; maxitverticalforestertem = 0
 endif
 if (jased == 0) then
    limtypsed = 0
 endif
 if (iadvec == 0) then
    limtypmom = 0
 endif

 ! It is not allowed to change the selection of limiters for sal/tem/sed as defined in the input file. Therefore, the next line is now a comment (and will be removed in future)
 ! limtyp = max(Limtypsa, limtyptm, limtypsed)                   ! check if limiter need be applied

 if (jasal == 0 .and. jatem == 0 .and. jased == 0 .and. jatransportmodule == 0 ) return    ! no salinity, or sediment, no higher orders

 if (jasal > 0) then

     do k  = 1,nbnds                              ! set 1D or 3D sal boundary conditions
       LL = kbndS(3,k)
       call getLbotLtop(LL,Lb,Lt)
       kb = 0
       do L  = Lb,Lt
          kb = ln(1,L) ; ki = ln(2,L)
          if (q1(L) >= 0 .or. keepstbndonoutflow == 1) then
              kk      = kmxd*(k-1)+L-Lb+1
              constituents(isalt, kb) = zbnds(kk)                 ! inflow
              salmax  = max( salmax, constituents(isalt, kb) )
          else
              constituents(isalt, kb) = constituents(isalt, ki)   ! outflow
          endif
          !if (jasteric > 0) then
          !   steric(1,kb) = zbnds(kk)
          !endif
       enddo

       if ( kb.gt.0 ) then
          valtop = constituents(isalt, kb)
          do L=Lt+1,Lb+kmxL(LL)-1
             kb      = ln(1,L)
             constituents(isalt, kb) = valtop
          end do
       end if
    enddo

    if ( jampi.eq.1 .and. jatransportmodule == 0) then
       if ( jatimer.eq.1 ) call starttimer(IMPIREDUCE)
       call reduce_double_max(salmax)
       if ( jatimer.eq.1 ) call stoptimer(IMPIREDUCE)
    end if

 endif

 if (jatem > 0) then
    do k  = 1,nbndtm                                  ! set 1D or 3D temp boundary conditions
       LL = kbndTM(3,k)
       call getLbotLtop(LL,Lb,Lt)
       kb = 0
       do L  = Lb,Lt
          kb = ln(1,L) ; ki = ln(2,L)
          if (q1(L) >= 0  .or. keepstbndonoutflow == 1) then
              kk        = kmxd*(k-1)+L-Lb+1
              constituents(itemp, kb)  = zbndTM(kk)                  ! inflow
          else
              constituents(itemp, kb)  = constituents(itemp, ki)     ! outflow
          endif
          !if (jasteric > 0) then
          !   steric(2,kb) = zbndTM(kk)
          !endif
       enddo

       if ( kb.gt.0 ) then
          valtop = constituents(itemp, kb)
          do L=Lt+1,Lb+kmxL(LL)-1
             kb                      = ln(1,L)
             constituents(itemp, kb) = valtop
          end do
       end if

    enddo
   ! tem1 = tem1 + 50d0 ! tkelvn
 endif

 if (jased > 0 .and. jased < 4) then

    if (nbndz > 0) then
       call setequilibriumsedimentbnds(nbndz,6,kbndz,kbanz,0)
    endif
    if (nbndu > 0) then
       call setequilibriumsedimentbnds(nbndu,6,kbndu,kbanu,1)
    endif

    do k  = 1,nbndsd                                 ! set prescribed sediment boundary conditions
       kb = kbndSd(1,k)
       ki = kbndSd(2,k)
       L  = kbndSd(3,k)
       do j = 1,mxgr
          if (q1(L) > 0) then
             sed(j,kb) = zbndsd(k)                   ! inflow ,  todo, check vectormax over grainsizes if boundaryprescribed, else
          endif
       enddo
    enddo

    if (dmorfac > 0 .and. time1 >= tstart_user + TMorfspinup) then
       jamorf = 1
    endif
    dvolbot = 0d0

 endif

! begin DEBUG
  if ( jatransportmodule.ne.0 ) then
     goto 1234
  endif
! end DEBUG

 if (limtypsa == 6) then

    dsadx = 0d0; dsady = 0d0
    do LL = 1,lnx
       Lb = Lbot(LL) ; Lt = Lb - 1 + kmxL(LL)
       do L  = Lb, Lt
          k1 = ln(1,L)
          k2 = ln(2,L)
          dsadn = dxi(LL)*( constituents(isalt,k2) - constituents(isalt,k1) )
          dsadx(k1) = dsadx(k1) + wcx1(LL)*dsadn
          dsady(k1) = dsady(k1) + wcy1(LL)*dsadn
          dsadx(k2) = dsadx(k2) + wcx2(LL)*dsadn
          dsady(k2) = dsady(k2) + wcy2(LL)*dsadn
       enddo
    enddo

 endif

 if (jasal > 0)  then
    supq   = 0d0 ; qsho = 0d0
 endif

 if (jatem > 0) then
    tupq   = 0d0 ; qtho = 0d0
  endif

 if (jased > 0) then
    sdupq  = 0d0
 endif

 jalim2D = 3                                                       ! 2D limiting
 if (jalim2D > 0) then
    if (.not. allocated(pp) ) then
       allocate ( pp(ndkx) , stat = ierr)
       call aerr('pp(ndkx)', ierr, lnkx+ndkx )
    endif
    if (jalim2D <= 2) then
        if (.not. allocated(pm) ) then
            allocate ( dsq(ndx), pm(ndx), qp(ndx), qm(ndx), alf(lnx)  , stat = ierr)
            call aerr('dsq(ndx), pm(ndx), qp(ndx), qm(ndx), alf(lnx)' , ierr, 6*ndx)
        endif
        dsq = 0d0
    endif
 endif

 do LL  = 1,lnx                                                    ! upwind (supq) + limited high order (dsq)


  if (q1(LL) .ne. 0d0) then

    call getLbotLtop(LL,Lb,Lt)
    do L = Lb,Lt

       k1  = ln(1,L) ; k2 = ln(2,L)

       if (q1(L) > 0) then
                                                                       !   ->      ds1   ds2
          k = k1 ; kd = k2 ; is =  1 ; half = acl(LL)       ; ip = 0   !   ->   ku     k     kd

       else
                                                                       !   <-      ds2   ds1
          k = k2 ; kd = k1 ; is = -1 ; half = 1d0-acl(LL)   ; ip = 3   !   <-   kd     k     ku

       endif

       ql  = is*q1(L)

       if (jasal > 0) then
           supq(kd) = supq(kd) + ql*constituents(isalt,k)
           supq(k ) = supq(k ) - ql*constituents(isalt,k)
       endif

       if (jatem > 0) then
           tupq(kd) = tupq(kd) + ql*constituents(itemp, k)
           tupq(k ) = tupq(k ) - ql*constituents(itemp, k)
       endif

       if (jased > 0) then
          do j = 1,mxgr ! grainsize loop
             sdupq(j,kd) = sdupq(j,kd) + ql*sed(j,k)
             sdupq(j,k ) = sdupq(j,k ) - ql*sed(j,k)
          enddo
       endif

       if (limtyp > 0 ) then  ! .and. L <= lnxi) then

          if (hs(ln(1,LL)) < Chkadvd .or. hs(ln(2,LL)) < Chkadvd) cycle

          kku  = klnup(1+ip,LL) ; if (kku == 0) cycle ; kkua = abs(kku)

          if (kmx > 0) then
             ku   = kbot(kkua) + kmxn(kkua) - ( Lb + kmxL(LL) - L) ; if (ku < kbot(kkua) .or. ku > ktop(kkua) ) cycle
          else
             ku   = abs(kku)
          endif

          if (kku < 0) then

             if (limtypsa > 0) then
                 saku = constituents(isalt,ku)
             endif

             if (limtyptm > 0) then
                 teku = constituents(itemp,ku)
             endif

             if (limtypsed > 0) then
                do j = 1,mxgr ! grainsize loop
                   sedku(j) = sed(j,ku)
                enddo
             endif

          else

             kkub = iabs( klnup(2+ip,LL) )
             if (kmx > 0) then
                ku2  = kbot(kkub) + kmxn(kkub) - ( Lb + kmxL(LL) - L) ; if (ku2 < kbot(kkub) .or. ku2 > ktop(kkub) ) cycle
             else
                ku2  = kkub
             endif

             sl1  = slnup(1+ip,LL) ; sl2  = slnup(2+ip,LL)

             if (limtypsa > 0) then
                saku  = constituents(isalt,ku)*sl1 + constituents(isalt,ku2)*sl2
             endif

             if (limtyptm > 0) then
                 teku  = constituents(itemp,ku)*sl1 + constituents(itemp,ku2)*sl2
             endif

             if (limtypsed  > 0) then
                do j = 1,mxgr ! grainsize loop
                   sedku(j) = sed(j,ku)*sl1 + sed(j,ku2)*sl2
                enddo
             endif

          endif

          sl3 = slnup(3+ip,LL)
          cf  =  dts*abs(u1(L))*dxi(LL)  ! cflj(L)  !cfli(k ) ! cflj(L)
          cf  =  half*max( 0d0,1d0-cf )

          if (limtypsa > 0) then                ! set high order term for salinity
             ! if (min(sa1(kd), sa1(k), saku) > 1d-3 .and. max(sa1(kd), sa1(k), saku) < salmax -1d-3) then ! lower order near top
             if ( .true. ) then ! lower order near top

                ds2  =  constituents(isalt, kd) - constituents(isalt, k)        ! ds1 = voorlopende slope, ds2 = eigen slope
                ds1  = (constituents(isalt, k)  - saku )*sl3

                if (abs(ds2)  > eps10 .and. abs(ds1) > eps10) then
                   if (Limtypsa == 7) then
                       ds  =  0.5d0*ds2 ! central only for cursusdemo
                   else if (Limtypsa == 6) then
                       ds1 = is*(dsadx(k)*csu(LL) + dsady(k)*snu(LL))
                       ds  = cf*dlimitercentral(ds1, ds2, limtypsa)
                   else
                       ds  = cf*dslim(ds1, ds2, limtypsa)
                   endif
                   if (abs(ds) > eps10) then
                      qsho(L)  =   ds*ql
                   endif

                endif
                  ! plotlin(L) = qds
             endif
          endif

          if (limtyptm > 0) then                ! set high order term for temperature
              ds2  =  constituents(itemp, kd) - constituents(itemp, k)        ! ds1 = voorlopende slope, ds2 = eigen slope
              ds1  = (constituents(itemp, k)  - teku )*sl3
              if (abs(ds2)  > eps10 .and. abs(ds1) > eps10) then
                 ds  =  cf*dslim(ds1, ds2, limtypsa)
                 if (abs(ds) > eps10) then
                    qtho(L)  =  ds*ql
                 endif
              endif
          endif

          if (limtypsed > 0) then                           ! set high order term for sediment, transport limiter equal to that for salinity , = limtypsal
             do j = 1,mxgr  ! grainsize loop
                ds2           =  sed(j,kd) - sed(j,k)       ! ds1 = voorlopende slope, ds2 = eigen slope
                ds1           = (sed(j,k)  - sedku(j) )*sl3
                if (abs(ds2)  > eps10 .and. abs(ds1) > eps10) then
                   ds  =  cf*dslim(ds1, ds2, limtypsed)
                   if (abs(ds)    > eps10) then
                      qds         =  ds*ql
                      sdupq(j,kd) =  sdupq(j,kd) + qds
                      sdupq(j,k ) =  sdupq(j,k ) - qds
                   endif
                  ! plotlin(L) = qds
                endif
             enddo
          endif

       endif


     enddo ! vertical

   endif


 enddo  ! horizontal


 nrimptran  = 0
 if (jasal > 0 .and. nrimptran > 0) then

    if (.not. allocated(sa00) ) then
       allocate ( sa00(ndkx), cch(lnkx) )
    endif

    cfli = 0d0 ; cflj = 0d0 ; tetaj = 0d0

    do L = 1,lnx                                       ! set courant j for downwind i, add j's for i
       if (q1(L) .ne. 0d0) then
           k1 = ln(1,L) ; k2 = ln(2,L)
           if (     q1(L) > 0 .and. vol0(k2) > 0d0) then
               cflj(L)  =  dts*q1(L)/vol0(k2)
               cfli(k2) =  cfli(k2) + cflj(L)
           else if (q1(L) < 0 .and. vol0(k1) > 0d0) then
               cflj(L)  = -dts*q1(L)/vol0(k1)
               cfli(k1) =  cfli(k1) + cflj(L)
           endif
       endif
    enddo

    do k = 1,ndx
       if (cfli(k) > 1) nrimptran = nrimptran + 1
    enddo

    if (nrimptran > 0) then
       do L  = 1,lnx                                     ! set tetaj as tetai upwind cell
          tetaj(L) = 0d0
          if (q1(L) .ne. 0d0) then
             k1 = ln(1,L) ; k2 = ln(2,L)
             if (     q1(L) > 0) then
                tetaj(L) = max(0d0, 1d0 - 1d0/cfli(k1) )
             else if (q1(L) < 0) then
                tetaj(L) = max(0d0, 1d0 - 1d0/cfli(k2) )
             endif
          endif
        enddo
    endif

    sa00 = sa1 / max( 1d0, cfli )                                ! normalise with bbk

    do L = 1,lnx                                                 ! set up ccn, normalise with bbk
       k1 = ln(1,L) ; k2 = ln(2,L)
       if (     q1(L) > 0) then
          cch(L)  =  cflj(L)*tetaj(L) / max( 1d0, cfli(k2) )     ! L komt binnen voor k2
       else if (q1(L) < 0) then
          cch(L)  =  cflj(L)*tetaj(L) / max( 1d0, cfli(k1) )
       else
          cch(L) = 0d0
       endif
    enddo

    epssa = 1d9
    do it = 1,maxit

       sa0 = sa1
       sa1 = sa00

       do L = 1,lnx
          if ( cch(L) > 0 ) then
             k1 = ln(1,L) ; k2 = ln(2,L)
             if (     q1(L) > 0) then
                sa1(k2) = sa1(k2) + sa0(k1)*cch(L)
             else if (q1(L) < 0) then
                sa1(k1) = sa1(k1) + sa0(k2)*cch(L)
             endif
          endif
       enddo
       epssa = maxval(dabs(sa0-sa1))

       if (epssa < eps10) exit

    enddo

 endif

 if (limtypsa > 0 .and. jalim2D > 0)  then ! prepare for Kuzmin 2D limiting (comp and applied math 2008)

    if (jalim2D == 1) then
       pp = 0 ; pm = 0 ; qp = 0 ; qm = 0 ;alf  = 1d0
       do L = 1,lnx
          k1 = ln(1,L) ; k2 = ln(2,L)
          if (q1(L) > 0d0) then
             pp(k2) = pp(k2) + max( 0d0,  alf(L)* qsho(L) )           ! set   upwind pp, see 4.83
             pm(k2) = pm(k2) + min( 0d0,  alf(L)* qsho(L) )           !
             qp(k1) = qp(k1) + max( 0d0,   q1(L)*(constituents(isalt,k2)-constituents(isalt,k1)) )  ! set downwind qq
             qm(k1) = qm(k1) + min( 0d0,   q1(L)*(constituents(isalt,k2)-constituents(isalt,k1)) )  !
          else
             pp(k1) = pp(k1) + max( 0d0, -alf(L)* qsho(L) )           ! set   upwind pp
             pm(k1) = pm(k1) + min( 0d0, -alf(L)* qsho(L) )           !
             qp(k2) = qp(k2) + max( 0d0,  -q1(L)*(constituents(isalt,k1)-constituents(isalt,k2)) )  ! set downwind qq
             qm(k2) = qm(k2) + min( 0d0,  -q1(L)*(constituents(isalt,k1)-constituents(isalt,k2)) )  !
          endif
       enddo


       do L = 1,lnx
          k1 = ln(1,L) ; k2 = ln(2,L) ; aa = 1d0
          if (qsho(L) > 0d0) then
             if (pp(k1) .ne. 0d0 .and. qp(k1) .ne. 0d0) then
                aa = qp(k1) / pp(k1)
             endif
          else if (qsho(L) < 0d0 ) then
             if (pm(k2) .ne. 0d0 .and. qm(k2) .ne. 0d0) then
                aa = qm(k2) / pm(k2)
             endif
          endif
          if (aa > 0d0) then
              if ( aa < 1d0) then
                 alf(L) = aa
              endif
          else
              alf(L) = 1d0
          endif
       enddo

    else if (jalim2D == 2) then

       pp = supq ; pm = 0d0 ;  qp = -1d3 ; qm = 1d3

       do L = 1,lnx
          k1 = ln(1,L) ; k2 = ln(2,L)
          if (q1(L) > 0d0) then
             kd = k2 ; k = k1 ; is =  1d0
          else
             kd = k1 ; k = k2 ; is = -1d0
          endif
          pp(kd) = pp(kd) + qsho(L)
          pm(k ) = pm(k ) + qsho(L)

          qp(kd) = max(qp(kd),  constituents(isalt,k )) ! min and max of neighbours
          qm(kd) = min(qm(kd),  constituents(isalt,k ))
          qp(k ) = max(qp(k ),  constituents(isalt,kd))
          qm(k ) = min(qm(k ),  constituents(isalt,kd))

       enddo

       do L = 1,lnx
          k1 = ln(1,L) ; k2 = ln(2,L)
          if (q1(L) > 0d0) then ! limit outflow only: check the upwind cell
             k = k1
          else
             k = k2
          endif
          aa = 1d0


          if (pp(k) > 0) then

             if (pm(k) > 0) then
                 if (constituents(isalt,k) > qp(k) ) then  ! local max
                   aa = pp(k) / pm(k)
                 endif
             endif

          else if (pp(k) < 0) then

             if (pm(k) < 0) then
                 if ( constituents(isalt,k) < qm(k) ) then ! local min
                     aa = pp(k) / pm(k)
                 endif
             endif

          endif
          if (aa < 0d0) then
              aa = 0d0
          else if (aa > 1d0) then
              aa = 1d0
          endif
          alf(L) = aa
        enddo

    else if (jalim2D == 3) then ! testing to see if ho increases max - min, if so, switch off ho

       pp = supq

       do LL = 1,lnx
          if (q1(LL) .ne. 0) then
             call getLbotLtop(LL,Lb,Lt)
             do L = Lb, Lt
                k1 = ln(1,L) ; k2 = ln(2,L)
                if (q1(L) > 0) then
                   is =  1
                else
                   is = -1
                endif
                pp(k2) = pp(k2) + qsho(L)*is
                pp(k1) = pp(k1) - qsho(L)*is
             enddo
          endif
       enddo

       !$OMP PARALLEL DO                        &
       !$OMP PRIVATE(kk,k,kb,kt,aa,n,L)
       do kk = 1,ndx
          if (kfs(kk) <= 0) cycle
          call getkbotktop(kk,kb,kt)
          do k = kb,kt
             if (vol1(k) > 0d0) then
                aa = constituents(isalt,k) + dts*pp(k)/vol1(k)
                if ( aa < 0d0 .or. aa > salmax ) then
                    do n = 1,nd(kk)%lnx
                       L = iabs( nd(kk)%ln(n) )
                       qsho(L) = 0d0
                    enddo
                endif
             endif
          enddo
       enddo
       !$OMP END PARALLEL DO

    endif

    do LL = 1,lnx
       if (q1(LL) .ne. 0d0) then
          call getLbotLtop(LL,Lb,Lt)
          do L = Lb, Lt
             k1 = ln(1,L) ; k2 = ln(2,L)
             if (q1(L) > 0) then
                is =  1
             else
                is = -1
             endif
             if (qsho(L) .ne. 0d0) then
                supq(k2) = supq(k2) + qsho(L)*is
                supq(k1) = supq(k1) - qsho(L)*is
             endif
             if (limtyptm > 0) then
                if (qtho(L) .ne. 0d0) then
                   tupq(k2) = tupq(k2) + qtho(L)*is
                   tupq(k1) = tupq(k1) - qtho(L)*is
                endif
             endif
             !if (limtypsd > 0) then
             !   if (qtho(L) .ne. 0d0) then
             !      sdupq(k2) = sdupq(k2) + qsdho(L)*is
             !      sdupq(k1) = sdupq(k1) - qsdho(L)*is
             !   endif
             !endif
          enddo
       endif
    enddo

   ! plotlin = alf

 endif

! if (jasal > 0 .or. jatem > 0) then

    if (dicouv >= 0d0 ) then  ! horizontal diffusion
       do LL = 1,lnx
          if (hu(LL) > 0d0) then
             !n1  = ln(1,LL) ; n2 = ln(2,LL)
             !dfac1 = 1d0/dble(nd(n1)%lnx)
             !dfac2 = 1d0/dble(nd(n2)%lnx)
!            safe for triangles, quads and pentagons, but not for hexahedrons:
             dfac1 = 0.2d0
             dfac2 = 0.2d0
             call getLbotLtop(LL,Lb,Lt)

             if (jadiusp == 1) then
                 diuspL = diusp(LL)
             else
                 diuspL = dicouv
             endif

             do L   = Lb,Lt
                k1  = ln(1,L) ; k2 = ln(2,L)
                viL = max(0d0, viu(L))
                qds = (sigsali*viL + diuspl)*dxi(LL)*au(L)
                qds = min( qds,  dfac1*( vol1(k1)/dts - sqi(k1) ) , dfac2*( vol1(k2)/dts - sqi(k2) ) ) ! zie Borsboom sobek note
                qds = max( 0d0, qds)
                if (jasal > 0) then
                   if (jacreep == 1) then
                      ds2 = dsalL(L)
                   else
                      ds2 = constituents(isalt,k2) - constituents(isalt,k1)
                   endif
                   qsa = qds*ds2
                   supq(k2) =  supq(k2) - qsa
                   supq(k1) =  supq(k1) + qsa
                endif
                if (jatem > 0) then
                   if (jacreep == 1) then
                       ds2 = dtemL(L)
                   else
                       ds2 = constituents(itemp, k2) - constituents(itemp, k1)
                   endif
                   qsa = qds*ds2
                   tupq(k2) =  tupq(k2) - qsa
                   tupq(k1) =  tupq(k1) + qsa
                endif
                if (jased > 0) then
                   do j = 1,mxgr
                      ds2 = sed(j,k2) - sed(j,k1)
                      qsa = qds*ds2
                      sdupq(j,k2) =  sdupq(j,k2) - qsa
                      sdupq(j,k1) =  sdupq(j,k1) + qsa
                   enddo
                endif

             enddo
          endif
       enddo
    endif

!   UPDATE SALT BY ADDING SALT FLUXES

    if (kmx == 0) then                                                        ! 2D


       !$OMP PARALLEL DO               &
       !$OMP PRIVATE(k,src,j)
       do k = 1,ndxi
          if ( vol1(k) > eps4 ) then
              if (jasal > 0) then
                  src = 0d0 ; if (numsrc > 0) src = salsrc(k)
                  constituents(isalt,k) = constituents(isalt,k)  + dts*(supq(k) - constituents(isalt,k)*sq(k)  + src ) / vol1(k)
              endif
              if (jatem > 0) then
                  constituents(itemp,k) = constituents(itemp,k)  + dts*(tupq(k) - constituents(itemp,k)*sq(k) + heatsrc(k) ) / vol1(k)
              endif
              if (jased > 0) then
                 do j = 1,mxgr
                    sed(j,k)  = sed(j,k)  + dts*(sdupq(j,k) - sed(j,k)*sq(k) ) / vol1(k)
                 enddo
              endif
           endif
       enddo
       !$OMP END PARALLEL DO

       if (jampi > 0) then

!         update sa1
          if (jasal > 0) then
             if ( jatimer.eq.1 ) call starttimer(IUPDSALL)
             call update_ghosts(ITYPE_Sall, 1, Ndx, sa1, ierror)
             if ( jatimer.eq.1 ) call stoptimer(IUPDSALL)
          endif
       end if

    else                                                                      ! 3D

     tetav1 = 1d0-tetav

     !$xOMP PARALLEL DO                                                                                 &
     !$xOMP PRIVATE(kk,kb,kt,km,ku,kd,qst,qstd,qstu,ho,dif,a,b,c,d,e,java,n,adv,adv1,cf,ds1,ds2,m,ja,src)     &
     !$xOMP REDUCTION(+:sam1tot)

     do kk = 1,ndxi
        if (kfs(kk) <= 0) cycle
        call getkbotktop(kk,kb,kt)
        !if ( kt < kb ) cycle
        !if ( vol1(kb) < eps10 ) cycle
        km = kt - kb + 1

        difsalw = 0d0 ; diftemw = 0d0 ; difsedw = 0d0
        if (dicoww >= 0d0 .and. hs(kk) > epshsdif) then
           if (jasal > 0) difsalw = dicoww + difmolsal
           if (jatem > 0) diftemw = dicoww + difmoltem
           if (jased > 0) difsedw = dicoww + 0d0 ! difmolsed
        endif

        if (javasal == 1 .or. javasal == 2 ) then                  ! vertical explicit
           do k = kb, kt - 1
              if (qw(k) > 0) then                                  ! ku = upwind
                 ku = k   ; kd = k+1 ; qst =  qw(k)                ! from k to k+1
              else
                 ku = k+1 ; kd = k   ; qst = -qw(k)                ! from k+1 to k
              endif

              qstd = qst*(constituents(isalt,ku) ) ! -sa1(kd))     ! incoming - self
              qstu = qstd           ! 0d0
              if (javasal > 1) then
                 ho   = qst*(constituents(isalt,kd)-constituents(isalt,ku))*0.5d0
                 qstd = qstd + ho                                  ! ho
                 qstu = qstu + ho                                  ! ho
              endif
              if (difsalw > 0d0) then
                 dif  = (sigsali*vicwws(k) + difsalw)*(constituents(isalt,ku) - constituents(isalt,kd) )*ba(kk) / ( 0.5d0*(zws(k+1) - zws(k-1)) )
                 qstd = qstd + dif ; qstu = qstu + dif
              endif
              supq(kd) = supq(kd) + qstd
              supq(ku) = supq(ku) - qstu

           enddo

           do k = kb,kt
              if (vol1(k) > 0d0 ) then
                 src = 0d0 ; if (numsrc > 0) src = salsrc(k)
                 constituents(isalt,k)  = constituents(isalt,k)  + dts*(supq(k) - constituents(isalt,k)*sq(k) + src ) / vol1(k)
              endif
           enddo

        else if (javasal >= 3) then                                               ! vertical implicit

           java    = javasal
           if (java >= 5) then                          ! Forrester alternative: profile upwind if negative stratification
              java = 4
              if (hs(kk) < chkadvd) then
                 java = 3
              else
                 do k = kb+1, kt
                    if (constituents(isalt,k) > constituents(isalt,k-1) ) then
                       java = 3
                       exit
                    endif
                 enddo
              endif
           endif

           if (jasal > 0) then
               a(1:km) = 0d0 ; b(1:km) = 1d0 ; c(1:km) = 0d0
               src = 0d0 ; if (numsrc > 0) src = salsrc(kb)
               d(1)  = constituents(isalt,kb)  + dts*(supq(kb) - constituents(isalt,kb) *sq(kb) + src )  / vol1(kb)    ! put sa0 in d
           endif
           if (jatem > 0) then
               ta(1:km) = 0d0 ; tb(1:km) = 1d0 ; tc(1:km) = 0d0
               src   = heatsrc(kb)
               td(1) = constituents(itemp, kb) + dts*(tupq(kb) - constituents(itemp,kb)*sq(kb) + src ) / vol1(kb)     ! put sa0 in d
!              BEGIN DEBUG
!               td(1) = tem1(kb) + dts*src / vol1(kb)     ! put sa0 in d
!              END DEBUG
           endif
           if (jased > 0) then
               sa(1:km) = 0d0 ; sb(1:km) = 1d0 ; sc(1:km) = 0d0
               src   = 0d0 ! sedsrc(kk)
               do j = 1,mxgr ! grainsize loop
                  sd(1) = sed(j,kb) + dts*(sdupq(j,kb) - sed(j,kb)*sq(kb) + src ) / vol1(kb)   ! put sa0 in d
               enddo
           endif


           if (java == 3) then                          ! upwind implicit

              do k = kb, kt - 1
                 n = k - kb + 1
                 if (difsalw > 0d0) then
                    dif = dts*(sigsali*vicwws(k) + difsalw)*ba(kk) / ( 0.5d0*(zws(k+1) - zws(k-1)) )  ! m3
                 else
                    dif = 0d0
                 endif
                 if (diftemw > 0d0) then
                    dift = dts*(sigtemi*vicwws(k) + diftemw)*ba(kk) / ( 0.5d0*(zws(k+1) - zws(k-1)) )  ! m3
                 else
                    dift = 0d0
                 endif
                 if (difsedw > 0d0) then
                    difs = dts*(sigsedi*vicwws(k) + difsedw)*ba(kk) / ( 0.5d0*(zws(k+1) - zws(k-1)) )  ! m3
                 else
                    difs = 0d0
                 endif

                 if (qw(k) > 0) then
                     adv1  =  dts*qw(k) ; adv  = 0d0 ! m3
                 else if (qw(k) < 0) then
                     adv   = -dts*qw(k) ; adv1 = 0d0
                 else
                     adv = 0d0 ; adv1 = 0d0
                 endif

                 if (jasal > 0) then
                    b(n+1) = b(n+1) + ( dif  + adv *tetav) / vol1(k+1)
                    a(n+1) = a(n+1) - ( dif  + adv1*tetav) / vol1(k+1)
                    b(n  ) = b(n  ) + ( dif  + adv1*tetav) / vol1(k  )
                    c(n  ) = c(n  ) - ( dif  + adv *tetav) / vol1(k  )
                 endif

                 if (jatem > 0) then
                    tb(n+1) = tb(n+1) + ( dift  + adv *tetav) / vol1(k+1)
                    ta(n+1) = ta(n+1) - ( dift  + adv1*tetav) / vol1(k+1)

                    tb(n  ) = tb(n  ) + ( dift  + adv1*tetav) / vol1(k  )
                    tc(n  ) = tc(n  ) - ( dift  + adv *tetav) / vol1(k  )
                 endif

                 if (jased > 0) then
                    sb(n+1) = sb(n+1) + ( difs  + adv *tetav) / vol1(k+1)
                    sa(n+1) = sa(n+1) - ( difs  + adv1*tetav) / vol1(k+1)

                    sb(n  ) = sb(n  ) + ( difs  + adv1*tetav) / vol1(k  )
                    sc(n  ) = sc(n  ) - ( difs  + adv *tetav) / vol1(k  )
                 endif

                 if (jasal > 0) then
                    src    = 0d0 ; if (numsrc > 0) src = salsrc(k+1)
                    d(n+1) = constituents(isalt, k+1) + dts*( supq(k+1) - constituents(isalt, k+1)*sq(k+1) + src) / vol1(k+1)              ! horizontal explicit, org allinout
                    if (tetav .ne. 1d0) then
                       d(n+1) = d(n+1) - tetav1*( constituents(isalt, k+1)*adv  - constituents(isalt, k)*adv1 )   / vol1(k+1)
                       d(n  ) = d(n  ) - tetav1*( constituents(isalt, k  )*adv1 - constituents(isalt, k+1)*adv  ) / vol1(k  )
                    endif
                 endif

                 if (jatem > 0) then
                    src     = heatsrc(k+1)
                    td(n+1) = constituents(itemp, k+1) + dts*( tupq(k+1) - constituents(itemp, k+1)*sq(k+1) + src)  / vol1(k+1)

                    if (tetav .ne. 1d0) then
                       td(n+1) = td(n+1) - tetav1*(  constituents(itemp,k+1)*adv  - constituents(itemp, k)  *adv1 ) / vol1(k+1)
                       td(n  ) = td(n  ) - tetav1*(  constituents(itemp,k  )*adv1 - constituents(itemp, k+1)*adv  ) / vol1(k  )
                    endif
                 endif

                 if (jased > 0) then
                    do j = 1,mxgr ! grainsize loop
                       src     = 0d0 ! sedsrc(k+1)
                       sd(n+1) = sed(j,k+1) + dts*( sdupq(j,k+1) - sed(j,k+1)*sq(k+1) + src  )  / vol1(k+1)

                       if (tetav .ne. 1d0) then
                          sd(n+1) = td(n+1) - tetav1*( sed(j,k+1)*adv  - sed(j,k)  *adv1 ) / vol1(k+1)
                          sd(n  ) = td(n  ) - tetav1*( sed(j,k  )*adv1 - sed(j,k+1)*adv  ) / vol1(k  )
                       endif
                    enddo
                 endif


               !                     b(k+1):+adv   / vol1(k+1)    a(k+1):-adv1 / vol1(k+1)
               !  d(n+1) = d(n+1) - sa0(k+1)*adv   / vol1(k+1) + sa0(k)  *adv1 / vol1(k+1)  ! allinout explicit:
               !  d(n  ) = d(n  ) - sa0(k  )*adv1  / vol1(k)   + sa0(k+1)*adv  / vol1(k  )
               !                     b(k  ):+adv1  / vol1(k)      c(k)  :-adv  / vol1(k  )


              enddo

           else if (java == 4) then                             ! central implicit

              do k = kb, kt - 1
                 n = k - kb + 1

                 if (difsalw > 0d0) then
                    dif  = dts*(sigsali*vicwws(k) + difsalw)*ba(kk) / ( 0.5d0*(zws(k+1) - zws(k-1)) )  ! m3
                 else
                    dif = 0d0
                 endif
                 if (diftemw > 0d0) then
                    dift = dts*(sigtemi*vicwws(k) + diftemw)*ba(kk) / ( 0.5d0*(zws(k+1) - zws(k-1)) )  ! m3
                 else
                    dift = 0d0
                 endif
                 if (difsedw > 0d0) then
                    difs = dts*(sigsedi*vicwws(k) + difsedw)*ba(kk) / ( 0.5d0*(zws(k+1) - zws(k-1)) )  ! m3
                 else
                    difs = 0d0
                 endif

                 adv     = 0.5d0*dts*qw(k)*tetav
!                BEGIN DEBUG
!                 adv     = 0d0
!                 dift    = 0d0
!                 dift = dts*(diftemw)*ba(kk) / ( 0.5d0*(zws(k+1) - zws(k-1)) )  ! m3
!                END DEBUG

                 if (jasal > 0) then
                    b(n+1)  = b(n+1)  + (dif - adv) / vol1(k+1)
                    a(n+1)  = a(n+1)  - (dif + adv) / vol1(k+1)

                    b(n  )  = b(n  )  + (dif + adv) / vol1(k)
                    c(n  )  = c(n  )  - (dif - adv) / vol1(k)
                 endif

                 if (jatem > 0) then
                    tb(n+1) = tb(n+1) + ( dift  - adv ) / vol1(k+1)
                    ta(n+1) = ta(n+1) - ( dift  + adv ) / vol1(k+1)

                    tb(n  ) = tb(n  ) + ( dift  + adv ) / vol1(k  )
                    tc(n  ) = tc(n  ) - ( dift  - adv ) / vol1(k  )
                 endif

                 if (jased > 0) then
                    sb(n+1) = sb(n+1) + ( difs  - adv ) / vol1(k+1)
                    sa(n+1) = sa(n+1) - ( difs  + adv ) / vol1(k+1)

                    sb(n  ) = sb(n  ) + ( difs  + adv ) / vol1(k  )
                    sc(n  ) = sc(n  ) - ( difs  - adv ) / vol1(k  )
                 endif

                 if (jasal > 0) then
                    src = 0d0 ; if (numsrc > 0) src = salsrc(k+1)
                    d(n+1)  = constituents(isalt,k+1) + dts*( supq(k+1) - constituents(isalt,k+1)*sq(k+1) + src ) / vol1(k+1)
                    if (tetav .ne. 1d0) then
                       adv    = 0.5d0*dts*qw(k)*tetav1*( constituents(isalt, k) + constituents(isalt,k+1) )
                       d(n+1) = d(n+1) + adv/vol1(k+1)
                       d(n  ) = d(n  ) - adv/vol1(k)
                    endif
                 endif

                 if (jatem  > 0) then
                    src     = heatsrc(k+1)
                    td(n+1) = constituents(itemp,k+1) + dts*( tupq(k+1) - constituents(itemp, k+1)*sq(k+1) + src ) / vol1(k+1)
                    if (tetav .ne. 1d0) then
                       adv     = 0.5d0*dts*qw(k)*tetav1*( constituents(itemp, k) + constituents(itemp, k+1) )
                       td(n+1) = td(n+1) + adv/vol1(k+1)
                       td(n  ) = td(n  ) - adv/vol1(k)
                    endif
                 endif

                 if (jased  > 0) then
                    src     = 0d0 ! sedsrc(k+1)
                    sd(n+1) = sed(1,k+1) + dts*( supq(k+1) - sed(1,k+1)*sq(k+1) + src ) / vol1(k+1)
                    if (tetav .ne. 1d0) then
                       adv    = 0.5d0*dts*qw(k)*tetav1*(sed(1,k) + sed(1,k+1))
                       sd(n+1) = sd(n+1) + adv/vol1(k+1)
                       sd(n  ) = sd(n  ) - adv/vol1(k)
                    endif
                 endif


              enddo


           endif

           if (jasal > 0) then
              call tridag(a,b,c,d,e,constituents(isalt,kb:kt),km)
           endif
           if (jatem > 0) then
              call tridag(ta,tb,tc,td,e,constituents(itemp,kb:kt),km)
           endif
           if (jased > 0) then
              do j = 1,mxgr
                 call tridag(sa,sb,sc,sd,e,sed(j,kb:kt),km)
              enddo
           endif


        endif

        if (maxitverticalforestersal > 0) then

            ! call foresterpoint(sa1(kb:), vol1(kb:), a, d, km, kmxn(kk), maxitverticalforestersal, 1)
            call foresterpoint2(constituents, numconst, ndkx, isalt, vol1(kb:), a, d, km, kmxn(kk), kb, maxitverticalforestersal, 1)

        endif

        if (maxitverticalforestertem > 0) then

            call foresterpoint2(constituents, numconst, ndkx, itemp, vol1(kb:), a, d, km, kmxn(kk), kb, maxitverticalforestertem, -1)

        endif

     end do
     !$xOMP END PARALLEL DO

       if ( jampi.eq.1 ) then
!         update sa1
          if ( jatimer.eq.1 ) call starttimer(IUPDSALL)
          if (jasal > 0) then
          call update_ghosts(ITYPE_Sall3D, 1, Ndkx, sa1, ierror)
          endif
          if (jatem > 0) then
             call update_ghosts(ITYPE_Sall3D, 1, Ndkx, tem1, ierror)
          endif
          if ( jatimer.eq.1 ) call stoptimer(IUPDSALL)
       end if
    endif ! 3D

! begin DEBUG
1234 continue
    if ( jatransportmodule.eq.1 ) then
       call apply_tracer_bc()
       call update_constituents(0) ! do all constituents
    end if
! end DEBUG

  if ( jasal.gt.0 ) then    !  compute salt error

      if (jatransportmodule == 0) sam0 = sam1
      sam0tot = sam1tot
      sam1tot = 0d0

     !$OMP PARALLEL DO                &
     !$OMP PRIVATE(kk,kb,kt,km,k)     &
     !$OMP REDUCTION(+:sam1tot)
     do kk = 1,ndxi
        call getkbotktop(kk,kb,kt)
        if ( kt < kb ) cycle
        if ( vol1(kb) < eps10 ) cycle
        km = kt - kb + 1

        do k = kb,kt
            sam1tot = sam1tot + constituents(isalt,k)*vol1(k)
            if (jatransportmodule == 0) then
               sam1(k) =        constituents(isalt,k)*vol1(k)               ! mass balance
               same(k) = sam1(k) - sam0(k) - dts*( supq(k) + salsrc(k) )    ! mass balance
            endif
        enddo
     enddo
     !$OMP END PARALLEL DO

      saminbnd = 0d0 ; samoutbnd = 0d0

      do LL = lnxi + 1, 0 !  lnx                                ! copy on outflow
          call getLbotLtop(LL,Lb,Lt)
          if (Lt < Lb) then
              cycle
          endif
          do L = Lb, Lt
             kb = ln(1,L) ; ki = ln(2,L)
             if (q1(L) > 0) then
                saminbnd  = saminbnd  + q1(L)*constituents(isalt,kb)*dts   ! mass in
             else
                samoutbnd = samoutbnd - ( q1(L)*constituents(isalt,ki)+qsho(L) ) *dts   ! mass out
             endif
          enddo
       enddo
       samerr = sam1tot - sam0tot !  - saminbnd + samoutbnd
   endif

   !$OMP PARALLEL DO             &
   !$OMP PRIVATE(kk,kb,kt,k)
   do kk = 1,ndx ! i
      call getkbotktop(kk,kb,kt)
      if ( kt < kb ) cycle
      do k = kb,kt
         rho(k)  = setrho(k)
      enddo
      do k = kt+1 , kb + kmxn(kk) - 1
         rho(k) = rho(kt)
      enddo
   enddo
   !$OMP END PARALLEL DO

   if (stm_included) then 
      !$OMP PARALLEL DO             &
      !$OMP PRIVATE(kk,kb,kt,k)
       do kk = 1,ndx ! i5
         call getkbotktop(kk,kb,kt)
         do k = kt+1 , kb + kmxn(kk) - 1
            rhowat(k) = rhowat(kt)    ! UNST-5170
         enddo
      enddo
      !$OMP END PARALLEL DO
   endif
 
   ! propagate rho
   if (jabaroctimeint == 5) then  ! rho advection
       dts  = 0.5d0*dts
       if (jarhoxu > 0) then
           rho0 = rho
       endif
       call update_constituents(1) ! do rho only
       dts = 2.0d0*dts
   endif

! endif ! came from if (jasal > 0 .or. jatem > 0) then line 676, a jump to inside a check, I remove this check for clarity
 
 if (jarhoxu > 0 .and. jacreep == 1) then
     do LL = 1,lnx
        do L = Lbot(LL), Ltop(LL)
           k1 = ln(1,L) ; k2 = ln(2,L)
           rhou(L) = 0.5d0*( rho(k1) + rho(k2) )
        enddo
     enddo
 endif
 
 if (jased > 0 .and. jased < 4) then

    dmorfax = max(1d0,dmorfac)

    if ( jaceneqtr == 1) then           ! original cell centre equilibriumtransport approach

       if (dmorfac > 0d0) then
          blinc = 0d0
       endif

       jastep = 1 ! 1 = first hor. transport, then limiting

       !$OMP PARALLEL DO    &
       !$OMP PRIVATE(k,flx,seq,wse,hsk,dtvi,wsemx,j,qb,kj,dgrlay,kb) &
       !$OMP REDUCTION(+:dvolbot)
       do k  = 1,ndxi
          kb = kbot(k)
          if (vol1(kb) > 0d0) then

             flx = 0d0
             if (kmx == 0) then
                call getequilibriumtransportrates(k, seq, wse, mxgr, hsk)                     ! get per flowcell and store in small array seq
             else
                wse    = ws
                seq(1) = 0d0
             endif

             dtvi  = dts/vol1(kb)
             wsemx = 0.45d0*vol1(kb) / ( ba(k)*dts )
             do j  = 1,mxgr

                if ( Wse(j) > wsemx) then
                     Wse(j) = wsemx
                endif
                qb = Wse(j)*ba(k)                                                             ! (m3/s)

                if (jastep == 0) then
                   flx(j)    = qb*( seq(j) - sed(j,kb) )                                      ! (m3/s).(kg/m3) = kg/s   , positive = erosion
                   sed(j,kb) = sed(j,kb) + dtvi*(sdupq(j,kb) + flx(j) )                       ! horizontal + vertical transport
                else
                   sed(j,kb) = sed(j,kb) + dtvi*(sdupq(j,kb)          )                       ! horizontal transport
                   flx(j)    = qb*( seq(j) - sed(j,kb) )                                      ! (m3/s).(kg/m3) = kg/s   , positive = erosion
                   sed(j,kb) = sed(j,kb) + dtvi*(            + flx(j) )                       ! vertical transport
                endif

                dgrlay      = - dts*dmorfax*flx(j) / (rhosed(j)*ba(k)*rhobulkrhosed)          ! (s)*( )* (kg/s) * (m3 / kg) / m2 = (m)

                if (jamorf   == 1) then
                    grainlay(j,k) = grainlay(j,k)  + dgrlay
                    blinc(k)      = blinc(k)       + dgrlay
                    dvolbot       = dvolbot        + dgrlay*ba(k)
                endif

             enddo

          else

             sed( :, k ) = 0d0

          endif
       enddo
       !$OMP END PARALLEL DO


    else


       if (jatransportmodule == 0) then
          !$OMP PARALLEL DO    &
          !$OMP PRIVATE(k,j,dtvi,kb)
          do k = 1,ndxi
             kb = kbot(k)
             if (vol1(kb) > 0d0) then                                            ! horizontal transport over flow nodes
                dtvi = dts/vol1(kb)
                do j = 1,mxgr
                   sed(j,kb) = sed(j,kb) + dtvi*(sdupq(j,kb) - sed(j,kb)*sq(kb) )    ! horizontal transport
                enddo
             else
                sed(:, kb) = 0d0
             endif
          enddo
          !$OMP END PARALLEL DO
       endif

       sedi = 0d0

       !$OMP PARALLEL DO    &
       !$OMP PRIVATE(kk,flx, seq, wse, hsk,n,k,dtvi,wsemx,j,qb,dgrlay,kb) &
       !$OMP REDUCTION(+:dvolbot)

       do kk = 1,mxban

          flx = 0d0

          call getequilibriumtransportrates(kk, seq, wse, mxgr, hsk)    ! get per netnode and store in small array seq

          n  = nban(1,kk)  ! net node
          k  = nban(2,kk)  ! flow node
          kb = kbot(k)

          if ( vol1(kb) > 0 .and. hsk > 0) then

             dtvi  = dts/vol1(kb)                                       ! (s/m3)
             wsemx = 0.45d0*vol1(kb) / ( ba(k)*dts )                    ! (m/s) was 0.45

             do j  = 1,mxgr
                if ( Wse(j) > wsemx) then
                     Wse(j) = wsemx
                endif
                qb = Wse(j) * banf(kk)                                  ! (m3/s)
                flx(j)      = qb*( seq(j) - sed(j,kb) )                 ! (m3/s).(kg/m3) = kg/s   , positive = erosion

                !  if (zk(n) > skmx(n) ) then                           ! no flux if net point above max surrouding waterlevels
                !     flx(j) = max( 0d0, flx(j) )
                !  endif

                sedi(j,k)   = sedi(j,k)  + dtvi*flx(j)                  ! vertical transport (s/m3)*(kg/s) = (kg/m3)

                dgrlay      = - dts*dmorfax*flx(j)  / (rhosed(j)*ban(n)*rhobulkrhosed)      ! (s)*( )* (kg/s) * (m3 / kg) * (1/m2) = m

                if (jamorf == 1) then
                    grainlay(j,n) = grainlay(j,n)    + dgrlay
                    zk(n)         = zk(n)            + dgrlay
                    dvolbot       = dvolbot + banf(kk)*dgrlay
                endif

             enddo

          endif


       enddo
       !$OMP END PARALLEL DO

       !$OMP PARALLEL DO    &
       !$OMP PRIVATE(k,j,kb)
       do k = 1,ndxi
          kb = kbot(k)
          do j  = 1,mxgr
             sed(j,kb) = max(0d0, sed(j,kb) + sedi(j,k) )
          enddo
       enddo
       !$OMP END PARALLEL DO

    endif ! jacenterfluxes

 endif    ! jased


 do LL = lnxi + 1, lnx                           ! copy on outflow
    call getLbotLtop(LL,Lb,Lt)
    if (Lt < Lb) then
        cycle
    endif
    do L = Lb, Lt
       if (q1(L) < 0) then
          kb = ln(1,L) ; ki = ln(2,L)
          if (jasal > 0 .and. keepstbndonoutflow == 0) then
              constituents(isalt,kb)  = constituents(isalt,ki)
          endif
          if (jatem > 0  .and. keepstbndonoutflow == 0) then
              constituents(itemp, kb) = constituents(itemp,ki)
          endif
          if (jased > 0) then
             do j = 1,mxgr
                sed(j,kb) = sed(j,ki)
             enddo
          endif
       endif
    enddo
 enddo

 if (kplotordepthaveraged == 2) then
    if (jasal > 0) then
       call getverticallyaveraged(sa1,ndkx)
    endif
 endif

 do k = 1, 0!  ndxi ! for test selectiveZ.mdu
    if (xz(k) > 270) then
       do kk = kbot(k), ktop(k)
          if (zws(kk) < -5d0) then
             constituents(isalt,kk) = 30d0
          else
             constituents(isalt,kk) = 0d0
          endif
       enddo
    endif
 enddo

 call timstop(handle_extra(52)) ! transport

 end subroutine transport
