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

   subroutine flow_waveinit
   use m_flow
   use m_flowgeom
   use m_waves
   use m_xbeach_data
   use m_xbeach_avgoutput
   use m_xbeach_readkey
   use m_xbeach_filefunctions
   use m_xbeach_errorhandling
   use m_xbeach_paramsconst
   use M_SAMPLES
   use m_missing
   use m_alloc
   use m_sferic, only: jsferic, jasfer3D
   use m_polygon, only: NPL, xpl, ypl, zpl
   use m_ec_basic_interpolation, only: triinterp2
   use m_flowexternalforcings, only: transformcoef
   use dfm_error

   implicit none

   integer      :: ierr
   integer      :: minp0, jdla, nm, ibnd, kb, ki

   ierr = DFM_NOERR

   call realloc(uin, nbndw, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('uin  (nbndw)', ierr, nbndw)
   call realloc(vin, nbndw, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('vin  (nbndw)', ierr, nbndw)
   call realloc(u1rm, nbndu, stat=ierr, keepExisting=.false., fill=0d0)   ! remember u1 state
   call aerr('u1rm  (nbndu)', ierr, nbndu)

   call realloc(ypar, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('ypar(lnx)', ierr, lnx)
   call realloc(cfwavhi, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('cfwavhi(lnx)', ierr, lnx)
   call realloc(cfhi_vanrijn, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('cfhi_vanrijn(lnx)', ierr, lnx)
   call realloc(taubxu, lnx, stat=ierr, keepExisting = .false., fill = 0d0)   ! Always needs to be allocated, even if jawave == 0, used in gettau()
   call aerr('taubxu(lnx)', ierr, lnx)
   call realloc(taubu, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('taubu(lnx)', ierr, lnx)
   call realloc(ktb, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('ktb  (ndx)', ierr, ndx)
   call realloc(taux_cc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('taux_cc  (ndx)', ierr, ndx)
   call realloc(tauy_cc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('tauy_cc  (ndx)', ierr, ndx)
   call realloc(ust_mag, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('ust_mag  (ndx)', ierr, ndx)
   call realloc(fwav_mag, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fwav_mag  (ndx)', ierr, ndx)
   call realloc(wblt, lnx, stat=ierr, keepExisting = .false., fill = 0d0  )
   call aerr('wblt(lnx)', ierr, lnx)

   if (jawave==3 .or. jawave==6) then
      call realloc(wavfu, lnkx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('wavfu  (lnkx)', ierr, lnx)
      call realloc(wavfv, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('wavfv  (lnx)', ierr, lnx)
      call realloc(wavmubnd, lnkx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('wavmubnd  (lnkx)', ierr, lnx)
      call realloc(sxwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('sxwav  (ndx)', ierr, ndx)
      call realloc(sywav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('sywav  (ndx)', ierr, ndx)
      call realloc(sbxwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('sbxwav  (ndx)', ierr, ndx)
      call realloc(sbywav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('sbywav  (ndx)', ierr, ndx)
      call realloc(uorbwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('uorbwav  (ndx)', ierr, ndx)
      call realloc(wlenwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('wlenwav  (ndx)', ierr, ndx)

      call realloc(mxwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('mxwav(ndx)', ierr, ndx)
      call realloc(mywav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('mywav(ndx)', ierr, ndx)

      call realloc(dsurf, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('dsurf(ndx)', ierr, ndx)
      call realloc(dwcap, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('dwcap(ndx)', ierr, ndx)
      call realloc(kdismx, lnx, stat=ierr, keepExisting = .false., fill = 0  )
      call aerr('kdismx(lnx)', ierr, lnx)

   end if
   !
   if  (jawave > 0) then
      call realloc( hwavcom,   ndx, stat=ierr, keepExisting = .false., fill = hwavuni)
      call aerr   ('hwavcom   (ndx)', ierr, ndx)
   endif
   !
   if  (jawave == 6) then
      call realloc( hwav,   ndx, stat=ierr, keepExisting = .false., fill = hwavuni)
      call aerr   ('hwav   (ndx)', ierr, ndx)
      call realloc( twav,   ndx, stat=ierr, keepExisting = .false., fill = twavuni)
      call aerr   ('twav   (ndx)', ierr, ndx)
      call realloc( rlabda,  ndx,  stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr   ('rlabda  (ndx)',     ierr, ndx)
      call realloc( uorb,    ndx,  stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr   ('uorb    (ndx)',     ierr, ndx)
   endif

   if (jawave .eq. 4) then
      if (trim(instat)=='stat' .or. &
          trim(instat)=='stat_table') then
         call allocstatsolverarrays(ierr)
      endif

      call realloc(ee0, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('ee0  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(ee1, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('ee1  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(cwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('cwav  (ndx)', ierr, ndx)
      call realloc(cgwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('cgwav  (ndx)', ierr, ndx)
      call realloc(kwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('kwav  (ndx)', ierr, ndx)
      call realloc(km, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('km  (ndx)', ierr, ndx)
      call realloc(umwci, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('umwci  (ndx)', ierr, ndx)
      call realloc(vmwci, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('vmwci  (ndx)', ierr, ndx)
      call realloc(zswci, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('zswci  (ndx)', ierr, ndx)
      call realloc(nwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('nwav  (ndx)', ierr, ndx)
      call realloc(ctheta, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('ctheta  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(sigmwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('sigmwav  (ndx)', ierr, ndx)
      call realloc(sigt, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('sigt  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(horadvec, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('horadvec  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(thetaadvec, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('thetaadvec  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(rhs, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('rhs  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(rrhoradvec, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('rrhoradvec  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(rrthetaadvec, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('rrthetaadvec  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(rr, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('rr  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(H, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('H  (ndx)', ierr, ndx)
      call realloc(fw, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('fw  (ndx)', ierr, ndx)
      call realloc(E, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('E  (ndx)', ierr, ndx)
      call realloc(DR, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('DR  (ndx)', ierr, ndx)
      call realloc(R, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('R  (ndx)', ierr, ndx)
      call realloc(rr, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('rr  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(Sxx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Sxx  (ndx)', ierr, ndx)
      call realloc(Syy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Syy  (ndx)', ierr, ndx)
      call realloc(Sxy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Sxy  (ndx)', ierr, ndx)
      call realloc(Fx, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Fx  (lnx)', ierr, lnx)
      call realloc(Fy, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Fy  (lnx)', ierr, lnx)
      call realloc(Fx_cc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Fx_cc  (ndx)', ierr, ndx)
      call realloc(Fy_cc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Fy_cc  (ndx)', ierr, ndx)
      call realloc(urms, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('urms  (lnx)', ierr, lnx)
      call realloc(urms_cc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('urms_cc  (ndx)', ierr, ndx)
      call realloc(ust, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('ust  (lnx)', ierr, lnx)
      call realloc(vst, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('vst  (lnx)', ierr, lnx)

      call realloc(ustx_cc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('ustx_cc  (ndx)', ierr, ndx)
      call realloc(usty_cc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('usty_cc  (ndx)', ierr, ndx)

      call realloc(dhsdx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('dhsdx  (ndx)', ierr, ndx)
      call realloc(dhsdy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('dhsdy  (ndx)', ierr, ndx)

      call realloc(thetamean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('thetamean  (ndx)', ierr, ndx)
      call realloc(Qb, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Qb  (ndx)', ierr, ndx)
      call realloc(D, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('D  (ndx)', ierr, ndx)
      call realloc(Df, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Df  (ndx)', ierr, ndx)
      call realloc(Dtot, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Dtot  (ndx)', ierr, ndx)
      call realloc(BR, ndx, stat=ierr, keepExisting = .false., fill = beta)
      call aerr('BR  (ndx)', ierr, ndx)
      call realloc(bi, nbndw, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('bi  (nbndw)', ierr, nbndw)
      !call realloc(rolthick, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      !call aerr('rolthick  (ndx)', ierr, ndx)
      !call realloc(kturb, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      !call aerr('kturb  (ndx)', ierr, ndx)
      call realloc(Tbore, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Tbore  (ndx)', ierr, ndx)
      if (wci>0d0 .or. trim(absgentype)=='abs_2d') then
         call realloc(xbducxdx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('xbducxdx  (ndx)', ierr, ndx)
         call realloc(xbducydx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('xbducydx  (ndx)', ierr, ndx)
         call realloc(xbducxdy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('xbducxdy  (ndx)', ierr, ndx)
         call realloc(xbducydy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('xbducydy  (ndx)', ierr, ndx)
      endif
      if (trim(absgentype)=='abs_2d') then
         call realloc(dbetadx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('dbetadx  (ndx)', ierr, ndx)
         call realloc(dbetady, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('dbetady  (ndx)', ierr, ndx)
      endif
      call realloc(hdisp, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('hdisp  (ndx)', ierr, ndx)

      if (windmodel .eq. 0) then
         call realloc(L1, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('L1  (ndx)', ierr, ndx)
         call realloc(Ltemp, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('Ltemp  (ndx)', ierr, ndx)
         call realloc(L0, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('L0  (ndx)', ierr, ndx)
         !call realloc(khdisp, ndx, stat=ierr, keepExisting = .false., fill = 0d0)   ML: unused
         !call aerr('khdisp  (ndx)', ierr, ndx)
      endif

      if (windmodel .eq. 1) then
         call realloc(tt1, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('tt1  (ntheta,ndx)', ierr, ntheta*ndx)
         call realloc(cwavt, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cwavt  (ntheta,ndx)', ierr, ntheta*ndx)
         call realloc(cgwavt, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cgwavt  (ntheta,ndx)', ierr, ntheta*ndx)
         call realloc(kwavt, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('kwavt  (ntheta,ndx)', ierr, ntheta*ndx)
         call realloc(nwavt, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('nwavt  (ntheta,ndx)', ierr, ntheta*ndx)
         call realloc(horadvec2, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('horadvec2  (ntheta,ndx)', ierr, ntheta*ndx)
         call realloc(thetaadvec2, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('thetaadvec2  (ntheta,ndx)', ierr, ntheta*ndx)
         call realloc(L0t, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('L0t  (ntheta,ndx)', ierr, ntheta*ndx)
         call realloc(L1t, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('L1t  (ntheta,ndx)', ierr, ntheta*ndx)
         call realloc(Ltempt, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('Ltempt  (ntheta,ndx)', ierr, ntheta*ndx)
         call realloc(ma, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('ma  (ntheta,ndx)', ierr, ntheta*ndx)
         call realloc(mb, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('mb  (ntheta,ndx)', ierr, ntheta*ndx)
         call realloc(wmagcc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('wmagcc  (ndx)', ierr, ntheta*ndx)
         call realloc(windspreadfac, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('windspreadfac (ntheta,ndx)', ierr, ntheta*ndx)
         call realloc(wsorE, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('wsorE  (ntheta,ndx)', ierr, ntheta*ndx)
         call realloc(wsorT, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('wsorT  (ntheta,ndx)', ierr, ntheta*ndx)
         call realloc(egradcg, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('egradcg  (ntheta,ndx)', ierr, ntheta*ndx)
         call realloc(ddT, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('ddT  (ndx)', ierr, ntheta*ndx)
         call realloc(SwE, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('SwE  (ndx)', ierr, ntheta*ndx)
         call realloc(SwT, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('SwT  (ndx)', ierr, ntheta*ndx)
      endif

      !    for stationary solver
      !call realloc(isweepup, (/2,ntheta*Ndxi/), stat=ierr, keepExisting = .false., fill = 0)
      !call aerr('isweepup (2*ntheta*ndxi)', ierr, 2*ntheta*Ndxi)
      !call realloc(isweepdown, (/2,ntheta*Ndxi/), stat=ierr, keepExisting = .false., fill = 0)
      !call aerr('isweepdown (2*ntheta*ndxi)', ierr, 2*ntheta*Ndxi)

      ! handle wave friction, has to be post-poned until here because of unavailability of ndx
      if (wavefricfile .ne. ' ') then
         call check_file_exist(wavefricfile)   ! if not, program will exit here
         call writelog('lws','(a,a,a)','Warning: wave friction coefficient values from file ''',&
            trim(wavefricfile), &
            ''' will be used in computation')
         call oldfil(minp0, wavefricfile)
         call reasam(minp0, 0)
         !
         jdla = 1
         fw  = dmiss
         CALL triinterp2(xz, yz, fw, ndx,JDLA, &
            XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef)
         !
         ! mirror boundary cells if undefined if equal to dmiss
         do ibnd = lnxi+1, lnx            ! loop over boundary flow links
            kb = ln(1,ibnd)      ! point outside net
            ki = ln(2,ibnd)      ! point inside net
            if (fw(kb) == dmiss) then
               fw(kb) = fw(ki)
            endif
         enddo
         ! if node value still equal to dmiss (values are not defined on flow nodes) - throw error
         do nm = 1, ndx  ! loop over flow nodes
            if (fw(nm) == dmiss) then
               call xbeach_errorhandler()
            endif
         enddo
         call delsam(-1)
         call doclose(minp0)

      else
         fw = wavefricval
      endif

      !if (jamombal>0) then    ! compute some gradients
      call realloc(xbdsdx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('xbdsdx  (ndx)', ierr, ndx)
      call realloc(xbdsdy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('xbdsdy  (ndx)', ierr, ndx)

      !end if

      if (jaavgwavquant .eq. 1) then            !! arrays for statistical output wave quantities
         call realloc(E_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('E_mean  (ndx)', ierr, ndx)
         call realloc(E_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('E_var  (ndx)', ierr, ndx)
         call realloc(E_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('E_min  (ndx)', ierr, ndx)
         call realloc(E_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('E_max  (ndx)', ierr, ndx)
         call realloc(E_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('E_varcross  (ndx)', ierr, ndx)
         call realloc(E_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('E_varsquare  (ndx)', ierr, ndx)

         call realloc(H_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('H_mean  (ndx)', ierr, ndx)
         call realloc(H_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('H_var  (ndx)', ierr, ndx)
         call realloc(H_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('H_min  (ndx)', ierr, ndx)
         call realloc(H_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('H_max  (ndx)', ierr, ndx)
         call realloc(H_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('H_varcross  (ndx)', ierr, ndx)
         call realloc(H_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('H_varsquare  (ndx)', ierr, ndx)

         call realloc(R_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('R_mean  (ndx)', ierr, ndx)
         call realloc(R_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('R_var  (ndx)', ierr, ndx)
         call realloc(R_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('R_min  (ndx)', ierr, ndx)
         call realloc(R_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('R_max  (ndx)', ierr, ndx)
         call realloc(R_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('R_varcross  (ndx)', ierr, ndx)
         call realloc(R_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('R_varsquare  (ndx)', ierr, ndx)

         call realloc(D_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('D_mean  (ndx)', ierr, ndx)
         call realloc(D_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('D_var  (ndx)', ierr, ndx)
         call realloc(D_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('D_min  (ndx)', ierr, ndx)
         call realloc(D_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('D_max  (ndx)', ierr, ndx)
         call realloc(D_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('D_varcross  (ndx)', ierr, ndx)
         call realloc(D_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('D_varsquare  (ndx)', ierr, ndx)

         call realloc(DR_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('DR_mean  (ndx)', ierr, ndx)
         call realloc(DR_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('DR_var  (ndx)', ierr, ndx)
         call realloc(DR_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('DR_min  (ndx)', ierr, ndx)
         call realloc(DR_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('DR_max  (ndx)', ierr, ndx)
         call realloc(DR_varcross, ndx, stat=ierr, keepExisting = .false., fill = tiny(0d0))
         call aerr('DR_varcross  (ndx)', ierr, ndx)
         call realloc(DR_varsquare, ndx, stat=ierr, keepExisting = .false., fill = tiny(0d0))
         call aerr('DR_varsquare  (ndx)', ierr, ndx)

         call realloc(ust_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('ust_mean  (ndx)', ierr, ndx)
         call realloc(ust_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('ust_var  (ndx)', ierr, ndx)
         call realloc(ust_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('ust_min  (ndx)', ierr, ndx)
         call realloc(ust_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('ust_max  (ndx)', ierr, ndx)
         call realloc(ust_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('ust_varcross  (ndx)', ierr, ndx)
         call realloc(ust_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('ust_varsquare  (ndx)', ierr, ndx)

         call realloc(vst_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('vst_mean  (ndx)', ierr, ndx)
         call realloc(vst_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('vst_var  (ndx)', ierr, ndx)
         call realloc(vst_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('vst_min  (ndx)', ierr, ndx)
         call realloc(vst_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('vst_max  (ndx)', ierr, ndx)
         call realloc(vst_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('vst_varcross  (ndx)', ierr, ndx)
         call realloc(vst_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('vst_varsquare  (ndx)', ierr, ndx)

         call realloc(urms_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('urms_mean  (ndx)', ierr, ndx)
         call realloc(urms_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('urms_var  (ndx)', ierr, ndx)
         call realloc(urms_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('urms_min  (ndx)', ierr, ndx)
         call realloc(urms_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('urms_max  (ndx)', ierr, ndx)
         call realloc(urms_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('urms_varcross  (ndx)', ierr, ndx)
         call realloc(urms_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('urms_varsquare  (ndx)', ierr, ndx)

         call realloc(thetamean_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('thetamean_mean  (ndx)', ierr, ndx)
         call realloc(thetamean_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('thetamean_var  (ndx)', ierr, ndx)
         call realloc(thetamean_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('thetamean_min  (ndx)', ierr, ndx)
         call realloc(thetamean_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('thetamean_max  (ndx)', ierr, ndx)
         call realloc(thetamean_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('thetamean_varcross  (ndx)', ierr, ndx)
         call realloc(thetamean_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('thetamean_varsquare  (ndx)', ierr, ndx)
         call realloc(thetamean_sin, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('thetamean_sin  (ndx)', ierr, ndx)
         call realloc(thetamean_cos, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('thetamean_cos  (ndx)', ierr, ndx)

         call realloc(sigmwav_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('sigmwav_mean  (ndx)', ierr, ndx)
         call realloc(sigmwav_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('sigmwav_var  (ndx)', ierr, ndx)
         call realloc(sigmwav_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('sigmwav_min  (ndx)', ierr, ndx)
         call realloc(sigmwav_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('sigmwav_max  (ndx)', ierr, ndx)
         call realloc(sigmwav_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('sigmwav_varcross  (ndx)', ierr, ndx)
         call realloc(sigmwav_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('sigmwav_varsquare  (ndx)', ierr, ndx)

         call realloc(cwav_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cwav_mean  (ndx)', ierr, ndx)
         call realloc(cwav_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cwav_var  (ndx)', ierr, ndx)
         call realloc(cwav_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('cwav_min  (ndx)', ierr, ndx)
         call realloc(cwav_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('cwav_max  (ndx)', ierr, ndx)
         call realloc(cwav_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cwav_varcross  (ndx)', ierr, ndx)
         call realloc(cwav_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cwav_varsquare  (ndx)', ierr, ndx)

         call realloc(cgwav_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cgwav_mean  (ndx)', ierr, ndx)
         call realloc(cgwav_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cgwav_var  (ndx)', ierr, ndx)
         call realloc(cgwav_min, ndx, stat=ierr, keepExisting = .false., fill =huge(0d0))
         call aerr('cgwav_min  (ndx)', ierr, ndx)
         call realloc(cgwav_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('cgwav_max  (ndx)', ierr, ndx)
         call realloc(cgwav_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cgwav_varcross  (ndx)', ierr, ndx)
         call realloc(cgwav_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cgwav_varsquare  (ndx)', ierr, ndx)

         call realloc(s1_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('s1_mean  (ndx)', ierr, ndx)
         call realloc(s1_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('s1_var  (ndx)', ierr, ndx)
         call realloc(s1_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('s1_min  (ndx)', ierr, ndx)
         call realloc(s1_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('s1_max  (ndx)', ierr, ndx)
         call realloc(s1_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('s1_varcross  (ndx)', ierr, ndx)
         call realloc(s1_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('s1_varsquare  (ndx)', ierr, ndx)

         call realloc(Fx_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('Fx_mean  (ndx)', ierr, ndx)
         call realloc(Fx_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('Fx_var  (ndx)', ierr, ndx)
         call realloc(Fx_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('Fx_min  (ndx)', ierr, ndx)
         call realloc(Fx_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('Fx_max  (ndx)', ierr, ndx)
         call realloc(Fx_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('Fx_varcross  (ndx)', ierr, ndx)
         call realloc(Fx_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('Fx_varsquare  (ndx)', ierr, ndx)

         call realloc(Fy_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('Fy_mean  (ndx)', ierr, ndx)
         call realloc(Fy_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('Fy_var  (ndx)', ierr, ndx)
         call realloc(Fy_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('Fy_min  (ndx)', ierr, ndx)
         call realloc(Fy_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('Fy_max  (ndx)', ierr, ndx)
         call realloc(Fy_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('Fy_varcross  (ndx)', ierr, ndx)
         call realloc(Fy_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('Fy_varsquare  (ndx)', ierr, ndx)

         call realloc(u_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('u_mean  (ndx)', ierr, ndx)
         call realloc(u_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('u_var  (ndx)', ierr, ndx)
         call realloc(u_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('u_min  (ndx)', ierr, ndx)
         call realloc(u_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('u_max  (ndx)', ierr, ndx)
         call realloc(u_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('u_varcross  (ndx)', ierr, ndx)
         call realloc(u_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('u_varsquare  (ndx)', ierr, ndx)

         call realloc(v_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('v_mean  (ndx)', ierr, ndx)
         call realloc(v_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('v_var  (ndx)', ierr, ndx)
         call realloc(v_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('v_min  (ndx)', ierr, ndx)
         call realloc(v_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('v_max  (ndx)', ierr, ndx)
         call realloc(v_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('v_varcross  (ndx)', ierr, ndx)
         call realloc(v_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('v_varsquare  (ndx)', ierr, ndx)
      end if
   end if
   end subroutine flow_waveinit
