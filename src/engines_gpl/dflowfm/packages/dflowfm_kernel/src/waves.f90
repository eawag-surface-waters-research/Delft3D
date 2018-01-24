!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2018.                                
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

! $Id: waves.f90 54191 2018-01-22 18:57:53Z dam_ar $
! $HeadURL: https://repos.deltares.nl/repos/ds/trunk/additional/unstruc/src/waves.f90 $
!
!
!
! Generic wave array mapping for all wave models in FM
subroutine map_flowwave_exchange
   use m_sferic, only: pi, dg2rd, rd2dg
   use m_flowparameters, only: jawave, epshs
   use m_flow, only: ucx, ucy, hs, hu, wavfu
   use MessageHandling
   use m_waves, only: hwav, twav, phiwav, rlabda, uorb, ustokes, vstokes, Mxwav, Mywav, taubxu
   use m_xbeach_data, only: H, sigmwav, urms, thetamean, L1, ust, vst,  urms_cc, Fx_cc, Fy_cc, taux_cc, tauy_cc,ustx_cc,usty_cc
   use m_flowwave
   use m_physcoef, only: ag
   use m_alloc
   use m_flowgeom, only: lnx, acl, csu, snu, ndx, ln, wcl, nd, wcx1, wcx2, wcy1, wcy2
   use m_wind
   
   implicit none
   
   integer                                     :: ierr, k1, k2, k3, L, n, LL, LLL
   double precision, allocatable, dimension(:) :: ustbulk, ustbcos, ustbsin, ust_mag_u
   double precision, dimension(:), allocatable :: windx, windy
   
   if (jawave .eq. 0) then
      fwx%have_waves = .false.
      return
   end if
   
   allocate(ust_mag_u(1:lnx), stat=ierr)
   
   if ((jawave .gt. 0 .and. jawave .le. 2)) then                ! Fetch models

      if (.not. allocated(windx))  then
         allocate (windx(ndx), stat=ierr)
         allocate (windy(ndx), stat=ierr)
      end if
      windx = 0.0d0
      windy = 0.0d0
      do n = 1,ndx
         do LL=1,nd(n)%lnx
            LLL = iabs(nd(n)%ln(LL))
            k1 = ln(1,LLL) ; k2 = ln(2,LLL)
            k3 = 2 ; if( nd(n)%ln(LL) > 0 ) k3 = 1
            windx(n) = windx(n) + wx(LLL) * wcL(k3,LLL)
            windy(n) = windy(n) + wy(LLL) * wcL(k3,LLL)
         end do
      end do

      fwx%have_waves = .true.
      if (allocated(fwx%hrms) .and. allocated(hwav)) then
         if (.not. allocated(ustbulk) ) allocate(ustbulk(1:ndx), ustbcos(1:ndx), ustbsin(1:ndx), stat=ierr)
         ustbulk = 0d0; ustbcos = 0d0; ustbsin = 0d0
         
         fwx%hrms     = hwav
         fwx%tp       = twav
         fwx%uorb_rms = uorb
         fwx%rlabda   = rlabda
         fwx%teta     = atan2(windx, windy)*rd2dg                                ! degrees, no wave direction, so we assume alignment w wind
                                                                                 ! and cartesian ccw convention
                                                                                 ! JRE TO DO: if jawave .le. 2: always wind, so take wind dir?
         ustbulk  = 1d0/16d0*ag/max(hs,epshs)*hwav*hwav*twav/max(rlabda,0.1)     ! Very crude approximation
         ustbcos = ustbulk*dcos(fwx%teta*dg2rd)
         ustbsin = ustbulk*dsin(fwx%teta*dg2rd)
         
         do L=1,lnx                                    !! facenormal decomposition
            k1 = ln(1,L); k2 = ln(2,L)
            if (hu(L) > 0d0) then
               fwx%ustokes(L)    =       acL(L) * (csu(L)*ustbcos(k1)+snu(L)*ustbsin(k1)) + &
                                   (1d0-acL(L)) * (csu(L)*ustbcos(k2)+snu(L)*ustbsin(k2))
                                   
               fwx%vstokes(L)    =       acL(L) * (-snu(L)*ustbcos(k1)+csu(L)*ustbsin(k1)) + &
                                   (1d0-acL(L)) * (-snu(L)*ustbcos(k2)+csu(L)*ustbsin(k2))
            else
               fwx%ustokes(L) = 0d0
               fwx%vstokes(L) = 0d0
            end if
         enddo
         ust_mag_u = sqrt(fwx%ustokes*fwx%ustokes + fwx%vstokes*fwx%vstokes)
         fwx%ust_mag = 0d0
         do L=1,lnx
            k1=ln(1,L);k2=ln(2,L)
            fwx%ust_mag(k1) = fwx%ust_mag(k1)+wcl(1,L)*ust_mag_u(L)
            fwx%ust_mag(k2) = fwx%ust_mag(k2)+wcl(2,L)*ust_mag_u(L)
         enddo
         
         deallocate(ustbulk, ust_mag_u, stat=ierr)
      else
         call mess(LEVEL_ERROR, 'Error in map_flowwave_exchange(): wave communication arrays not associated/allocated.')
      end if      
   end if
   
   if (jawave .eq. 3) then                                    ! SWAN coupling
      fwx%have_waves = .true.
      if (allocated(fwx%hrms) .and. allocated(hwav)) then
         fwx%hrms     = hwav
         fwx%tp       = twav
         fwx%uorb_rms = uorb
         fwx%teta     = phiwav                    ! Cartesian CCW, dgr
         fwx%rlabda   = rlabda                    ! rlabda from m_waves
         fwx%ustokes  = ustokes                   ! Stokes velocity in flow links
         fwx%vstokes  = vstokes
         
         ust_mag_u = sqrt(fwx%ustokes*fwx%ustokes + fwx%vstokes*fwx%vstokes)
         fwx%ust_mag=0d0; fwx%fwav_mag=0d0
         do L=1,lnx
            k1=ln(1,L);k2=ln(2,L)
            fwx%ust_mag(k1) = fwx%ust_mag(k1)+wcl(1,L)*ust_mag_u(L)
            fwx%ust_mag(k2) = fwx%ust_mag(k2)+wcl(2,L)*ust_mag_u(L)
            fwx%fwav_mag(k1) = fwx%fwav_mag(k1)+wcl(1,L)*wavfu(L)
            fwx%fwav_mag(k2) = fwx%fwav_mag(k2)+wcl(2,L)*wavfu(L)
         enddo
      else
         call mess(LEVEL_ERROR, 'Error in map_flowwave_exchange(): wave communication arrays not associated/allocated.')
      end if
   end if
   
   if (jawave .eq. 4) then                                    ! Wave group motions
      fwx%have_waves = .true.
      if (allocated(fwx%hrms) .and. allocated(H)) then
         fwx%hrms     = H
         fwx%tp       = 2d0 * pi / sigmwav
         fwx%uorb_rms = urms_cc
         fwx%teta     = thetamean*rd2dg    ! ok, cartesian CCW
         fwx%rlabda   = L1                 ! wave length from xbeach_dispersion()
         fwx%ustokes  = ust
         fwx%vstokes  = vst
         !
         ust_mag_u = sqrt(fwx%ustokes*fwx%ustokes + fwx%vstokes*fwx%vstokes)
         fwx%ust_mag=0d0; taux_cc=0d0; tauy_cc=0d0
         do L=1,lnx
            k1=ln(1,L);k2=ln(2,L)
            fwx%ust_mag(k1) = fwx%ust_mag(k1)+wcl(1,L)*ust_mag_u(L)
            fwx%ust_mag(k2) = fwx%ust_mag(k2)+wcl(2,L)*ust_mag_u(L)
            taux_cc(k1) = taux_cc(k1)+wcx1(L)*taubxu(L)
            taux_cc(k2) = taux_cc(k2)+wcx2(L)*taubxu(L)
            tauy_cc(k1) = tauy_cc(k1)+wcy1(L)*taubxu(L)
            tauy_cc(k2) = tauy_cc(k2)+wcy2(L)*taubxu(L)
         enddo
         fwx%fwav_mag = sqrt(Fx_cc*Fx_cc + Fy_cc*Fy_cc)
         !
      else
         call mess(LEVEL_ERROR, 'Error in map_flowwave_exchange(): wave communication arrays not associated/allocated.')
      end if
   end if
   
   1234 continue
   
   return

end subroutine map_flowwave_exchange

subroutine flow_waveinit
   use m_flowwave
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

   call realloc(fwx%ustokes, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fwx%ustokes  (lnx)', ierr, lnx)
   call realloc(fwx%vstokes, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fwx%vstokes  (lnx)', ierr, lnx)
   call realloc(fwx%hrms, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fwx%hrms   (ndx)', ierr, ndx)
   call realloc(fwx%tp, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fwx%tp  (ndx)', ierr, ndx)
   call realloc(fwx%teta, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fwx%teta  (ndx)', ierr, ndx)
   call realloc(fwx%uorb_rms, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fwx%uorb_rms  (ndx)', ierr, ndx)
   call realloc(fwx%rlabda, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fwx%rlabda  (ndx)', ierr, ndx)
   call realloc(fwx%wblt, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fwx%wblt  (lnx)', ierr, lnx)
   call realloc(fwx%ust_mag, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fwx%ust_mag  (ndx)', ierr, ndx)
   call realloc(fwx%fwav_mag, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fwx%fwav_mag  (ndx)', ierr, ndx)
   
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
   call realloc(taus, ndx, stat=ierr, keepExisting = .false., fill = 0d0)     ! in subroutine gettaus for jawave <= 2 ..
   call aerr('taus  (ndx)', ierr, ndx)
   call realloc(ktb, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('ktb  (ndx)', ierr, ndx)

   if (jawave == 3) then
      call realloc(wavfu, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('wavfu  (lnx)', ierr, lnx)
      call realloc(wavmubnd, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('wavmubnd  (lnx)', ierr, lnx)
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
      call realloc(ustokes, lnkx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('ustokes(lnkx)', ierr, lnkx)
      call realloc(vstokes, lnkx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('vstokes(lnkx)', ierr, lnkx)
      
      call realloc(hsu, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('hsu(lnx)', ierr, lnx)
      !call realloc(bluf, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      !call aerr('bluf(lnx)', ierr, lnx)
      call realloc(cvalu0, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('cvalu0(lnx)', ierr, lnx)
      !call realloc(z0ucur, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      !call aerr('z0ucur(lnx)', ierr, lnx)

      call realloc(dsurf, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('dsurf(ndx)', ierr, ndx)
      call realloc(dwcap, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('dwcap(ndx)', ierr, ndx)
      call realloc(kdismx, lnx, stat=ierr, keepExisting = .false., fill = 0  )
      call aerr('kdismx(lnx)', ierr, lnx)

  end if
  if  (jawave > 0) then     
      call realloc(rlabda, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('rlabda(ndx)', ierr, ndx)
      call realloc( hwav,   ndx, stat=ierr, keepExisting = .false., fill = hwavuni)
      call aerr   ('hwav   (ndx)', ierr, ndx)
      call realloc( twav,   ndx, stat=ierr, keepExisting = .false., fill = twavuni)
      call aerr   ('twav   (ndx)', ierr, ndx)
      call realloc( phiwav, ndx, stat=ierr, keepExisting = .false., fill = phiwavuni)
      call aerr   ('phiwav (ndx)', ierr, ndx)
      call realloc(uorb, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('uorb  (ndx)', ierr, ndx)
  endif

  if (jawave .eq. 4) then
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
     call realloc(wsor, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
     call aerr('wsor  (ntheta,ndx)', ierr, ntheta*ndx)
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
     call realloc(taux_cc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
     call aerr('taux_cc  (ndx)', ierr, ndx)
     call realloc(tauy_cc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
     call aerr('tauy_cc  (ndx)', ierr, ndx)
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
     call realloc(xbducxdx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
     call aerr('xbducxdx  (ndx)', ierr, ndx)
     call realloc(xbducydx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
     call aerr('xbducydx  (ndx)', ierr, ndx)
     call realloc(xbducxdy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
     call aerr('xbducxdy  (ndx)', ierr, ndx)
     call realloc(xbducydy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
     call aerr('xbducydy  (ndx)', ierr, ndx)
     
     call realloc(L1, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
     call aerr('L1  (ndx)', ierr, ndx)
     call realloc(Ltemp, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
     call aerr('Ltemp  (ndx)', ierr, ndx)
     call realloc(L0, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
     call aerr('L0  (ndx)', ierr, ndx)
     call realloc(khdisp, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
     call aerr('khdisp  (ndx)', ierr, ndx)
     call realloc(hdisp, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
     call aerr('hdisp  (ndx)', ierr, ndx)
             
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
     
        call realloc(ust_mean, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('ust_mean  (lnx)', ierr, lnx)
        call realloc(ust_var, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('ust_var  (lnx)', ierr, lnx)
        call realloc(ust_min, lnx, stat=ierr, keepExisting = .false., fill = huge(0d0))
        call aerr('ust_min  (lnx)', ierr, lnx)
        call realloc(ust_max, lnx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
        call aerr('ust_max  (lnx)', ierr, lnx)
        call realloc(ust_varcross, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('ust_varcross  (lnx)', ierr, lnx)
        call realloc(ust_varsquare, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('ust_varsquare  (lnx)', ierr, lnx)
     
        call realloc(vst_mean, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('vst_mean  (lnx)', ierr, lnx)
        call realloc(vst_var, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('vst_var  (lnx)', ierr, lnx)
        call realloc(vst_min, lnx, stat=ierr, keepExisting = .false., fill = huge(0d0))
        call aerr('vst_min  (lnx)', ierr, lnx)
        call realloc(vst_max, lnx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
        call aerr('vst_max  (lnx)', ierr, lnx)
        call realloc(vst_varcross, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('vst_varcross  (lnx)', ierr, lnx)
        call realloc(vst_varsquare, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('vst_varsquare  (lnx)', ierr, lnx)
     
        call realloc(urms_mean, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('urms_mean  (lnx)', ierr, lnx)
        call realloc(urms_var, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('urms_var  (lnx)', ierr, lnx)
        call realloc(urms_min, lnx, stat=ierr, keepExisting = .false., fill = huge(0d0))
        call aerr('urms_min  (lnx)', ierr, lnx)
        call realloc(urms_max, lnx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
        call aerr('urms_max  (lnx)', ierr, lnx)
        call realloc(urms_varcross, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('urms_varcross  (lnx)', ierr, lnx)
        call realloc(urms_varsquare, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('urms_varsquare  (lnx)', ierr, lnx)
     
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
     
        call realloc(Fx_mean, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('Fx_mean  (lnx)', ierr, lnx)
        call realloc(Fx_var, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('Fx_var  (lnx)', ierr, lnx)
        call realloc(Fx_min, lnx, stat=ierr, keepExisting = .false., fill = huge(0d0))
        call aerr('Fx_min  (lnx)', ierr, lnx)
        call realloc(Fx_max, lnx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
        call aerr('Fx_max  (lnx)', ierr, lnx)
        call realloc(Fx_varcross, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('Fx_varcross  (lnx)', ierr, lnx)
        call realloc(Fx_varsquare, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('Fx_varsquare  (lnx)', ierr, lnx)
     
        call realloc(Fy_mean, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('Fy_mean  (lnx)', ierr, lnx)
        call realloc(Fy_var, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('Fy_var  (lnx)', ierr, lnx)
        call realloc(Fy_min, lnx, stat=ierr, keepExisting = .false., fill = huge(0d0))
        call aerr('Fy_min  (lnx)', ierr, lnx)
        call realloc(Fy_max, lnx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
        call aerr('Fy_max  (lnx)', ierr, lnx)
        call realloc(Fy_varcross, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('Fy_varcross  (lnx)', ierr, lnx)
        call realloc(Fy_varsquare, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('Fy_varsquare  (lnx)', ierr, lnx)
     
        call realloc(u_mean, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('u_mean  (lnx)', ierr, lnx)
        call realloc(u_var, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('u_var  (lnx)', ierr, lnx)
        call realloc(u_min, lnx, stat=ierr, keepExisting = .false., fill = huge(0d0))
        call aerr('u_min  (lnx)', ierr, lnx)
        call realloc(u_max, lnx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
        call aerr('u_max  (lnx)', ierr, lnx)
        call realloc(u_varcross, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('u_varcross  (lnx)', ierr, lnx)
        call realloc(u_varsquare, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('u_varsquare  (lnx)', ierr, lnx)
     
        call realloc(v_mean, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('v_mean  (lnx)', ierr, lnx)
        call realloc(v_var, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('v_var  (lnx)', ierr, lnx)
        call realloc(v_min, lnx, stat=ierr, keepExisting = .false., fill = huge(0d0))
        call aerr('v_min  (lnx)', ierr, lnx)
        call realloc(v_max, lnx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
        call aerr('v_max  (lnx)', ierr, lnx)
        call realloc(v_varcross, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('v_varcross  (lnx)', ierr, lnx)
        call realloc(v_varsquare, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
        call aerr('v_varsquare  (lnx)', ierr, lnx)
     end if
  end if
end subroutine flow_waveinit

subroutine tauwave()
    use m_sediment
    use m_sferic
    use m_flowparameters
    use m_flow, only: plotlin, rhog, rhomean, ag, s1, s0, hu, jaconveyance2D, hs, u1, v, taus, frcu, ifrcutp, cfuhi, huvli, z0ucur, z0urou !, wavmu 
    use m_flowgeom
    use m_physcoef, only:  rhomean, ee, sag, vonkar
    use m_waves
    use m_flowwave, only: fwx
    use m_bedform, only: bfmpar
    use unstruc_messages
    
    implicit none
    
    double precision           :: uorb1, k0, k0h, phigrid, phiwave, phi
    integer                    :: k, k1, k2, n, L, mout 
    integer                    :: jatauw = 1 
    integer                    :: wlenwav_from_SWAN = 0
    integer                    :: uorbwav_from_SWAN = 0
    double precision           :: hk, sh2hk,hksh2,rn,asg,ew,sxx,syy,dtau,shs, h2k, cc, cg, omeg, hminlwi
    double precision           :: dsk2, rk, astar, fw, hss, per, astarc, tauwav, taucur, tauwci, cdrag, z0, uorbu, tpu
    double precision           :: cz, frcn, uuu, vvv, umod, umodsq, cvalue, costu, sintu, abscos, uorbhs, waveps, u2dh
    double precision           :: xpar, ymxpar, lfc, cj, coeffb, coeffp, coeffq, ci,coeffa, coeffm, coeffn, yparL
    double precision           :: hpr, wu2, b21, ai, BL1, BL2, hus, ust, ac1, ac2
    double precision           :: ar, alfaw, wblt, rz, cf, cwall
    double precision           :: a, ks, phivr
    double precision           :: hrmsu, rlabdau, rr,umax,t1,u11,a11,raih,rmax, uon, uoff, uwbih
    double precision           :: rksru, rksmru, gamma, ksc, uratio, ka, ca
    integer                    :: ifrctyp
    
    double precision, external         :: tanhsafe, sinhsafe, sinhsafei
    
    double precision, dimension(8)             :: coeffi     ! Coefficient i in expression for parametrized models
    double precision, dimension(8)             :: coeffj     ! Coefficient j in expression for parametrized models
    double precision, dimension(8, 4)          :: aa         ! Coefficient a(i) in expression for parameter a
    double precision, dimension(8, 4)          :: bb         ! Coefficient b(i) in expression for parameter b
    double precision, dimension(8, 4)          :: mm         ! Coefficient m(i) in expression for parameter n
    double precision, dimension(8, 4)          :: nn         ! Coefficient n(i) in expression for parameter n
    double precision, dimension(8, 4)          :: pp         ! Coefficient p(i) in expression for parameter p
    double precision, dimension(8, 4)          :: qq         ! Coefficient q(i) in expression for parameter q

!!! ! data statemens
!!!!
!!!    data bb/      0.29,  0.65,  0.27,  0.73,  0.22,  0.32,  0.47, -0.06, &
!!!                  0.55,  0.29,  0.51,  0.40,  0.73,  0.55,  0.29,  0.26, &
!!!                 -0.10, -0.30, -0.10, -0.23, -0.05,  0.00, -0.09,  0.08, &
!!!                 -0.14, -0.21, -0.24, -0.24, -0.35,  0.00, -0.12, -0.03/
!!!    !
!!!    data pp/     -0.77, -0.60, -0.75, -0.68, -0.86, -0.63, -0.70, -1.00, &
!!!                  0.10,  0.10,  0.13,  0.13,  0.26,  0.05,  0.13,  0.31, &
!!!                  0.27,  0.27,  0.12,  0.24,  0.34,  0.00,  0.28,  0.25, &
!!!                  0.14, -0.06,  0.02, -0.07, -0.07,  0.00, -0.04, -0.26/
!!!    !
!!!    data qq/      0.91,  1.19,  0.89,  1.04, -0.89,  1.14,  1.65,  0.38, &
!!!                  0.25, -0.68,  0.40, -0.56,  2.33,  0.18, -1.19,  1.19, &
!!!                  0.50,  0.22,  0.50,  0.34,  2.60,  0.00, -0.42,  0.25, &
!!!                  0.45, -0.21, -0.28, -0.27, -2.50,  0.00,  0.49, -0.66/
!!!    !
!!!    data coeffj/  3.00,  0.50,  2.70,  0.50,  2.70,  3.00,  0.60,  1.50/
!!!    !
!!!    !-----for tau_max
!!!    data aa/     -0.06, -0.01, -0.07,  0.11,  0.05,  0.00, -0.01, -0.45, &
!!!                  1.70,  1.84,  1.87,  1.95,  1.62,  2.00,  1.58,  2.24, &
!!!                 -0.29, -0.58, -0.34, -0.49, -0.38,  0.00, -0.52,  0.16, &
!!!                  0.29, -0.22, -0.12, -0.28,  0.25,  0.00,  0.09, -0.09/
!!!    !
!!!    data mm/      0.67,  0.63,  0.72,  0.65,  1.05,  0.00,  0.65,  0.71, &
!!!                 -0.29, -0.09, -0.33, -0.22, -0.75,  0.50, -0.17,  0.27, &
!!!                  0.09,  0.23,  0.08,  0.15, -0.08,  0.00,  0.18, -0.15, &
!!!                  0.42, -0.02,  0.34,  0.06,  0.59,  0.00,  0.05,  0.03/
!!!    !
!!!    data nn/      0.75,  0.82,  0.78,  0.71,  0.66,  0.00,  0.47,  1.19, &
!!!                 -0.27, -0.30, -0.23, -0.19, -0.25,  0.50, -0.03, -0.66, &
!!!                  0.11,  0.19,  0.12,  0.17,  0.19,  0.00,  0.59, -0.13, &
!!!                 -0.02, -0.21, -0.12, -0.15, -0.03,  0.00, -0.50,  0.12/
!!!    !
!!!    data coeffi/  0.80,  0.67,  0.82,  0.67,  0.82,  1.00,  0.64,  0.77/

    waveps = 1d-8
    alfaw  = 20d0
    hminlwi = 1d0/hminlw

 ! parameterized bottom friction models

   do L = 1,lnx
       k1 = ln(1,L); k2 = ln(2,L)
       ! Use Eulerian velocities (U_GLM = U_Eulerian + U_stokes)..
       uuu = u1(L) - fwx%ustokes(L)
       
       if (jaconveyance2D >=3 .or. L <= lnx1D ) then      ! based on subroutine furu
           vvv = 0.0d0          
       else
           ! Use Eulerian velocities (U_GLM = U_Eulerian + U_stokes)..
           vvv = v(L) - fwx%vstokes(L)
       endif
   
       umodsq    = uuu*uuu + vvv*vvv
       umod      = max(1.0d-4, sqrt(umodsq))
       taubxu(L) = 0.0d0                      ! Set to zero, to consider the case with waves
       ypar(L)   = 0.0d0
       cfwavhi(L)= 0.0d0
       !
       ! TO DO: Replace the following messing with angles by an inproduct, without
       ! the expensive atan2 call
       !
       ! phigrid: angle between "normal direction on link" and "positive x-axis"
       phigrid = atan2(snu(L),csu(L)) * rd2dg
       ! phiwave: angle between "wave propagation direction" and "positive x-axis"
       !          Interpolate from nodes to links
       phiwave = acl(L)*fwx%teta(k1) + (1.0d0-acl(L))*fwx%teta(k2)
       ! phi: angle between "wave propagation direction" and "normal direction on link"
       phi     = phiwave - phigrid
        
       ! interpolate uorbu, tpu and wavmu from flownodes to flowlinks
       uorbu = acl(L)*fwx%uorb_rms(k1) + (1.0d0-acl(L))*fwx%uorb_rms(k2)
       tpu   = acl(L)*fwx%tp(k1)       + (1.0d0-acl(L))*fwx%tp(k2)
   
       !wavmu(L) = (acl(L)*mxwav(k1) + (1.0d0-acl(L))*mxwav(k2)) * csu(L) + &
       !           (acl(L)*mywav(k1) + (1.0d0-acl(L))*mywav(k2)) * snu(L)
       
       ! get current related roughness height
       call getczz0(hu(L),dble(frcu(L)),ifrcutp(L),cz,z0)
              
       if (modind > 0) then
          if (hu(L) > epshu) then

             costu = dcos(dg2rd*phi)     
             sintu = dsin(dg2rd*phi)     

             astarc = 30.*pi**2     ! critical value for astar

             abscos = abs(uuu*costu + vvv*sintu)/umod
             !
             ! wave friction factor and drag coefficient
             !
             astar  = tpu*uorbu/z0

             if (astar>astarc) then
                fw = 0.00251d0*exp(14.1d0/(astar**0.19))
             else                                           ! for relative small uorbs or large friction
                fw = 0.3d0
             endif
             !
             ! magnitude of bottom friction due to waves alone
             ! and due to current alone
             !
             tauwav = 0.5d0*rhomean*fw*uorbu**2           ! wave related bed shear stress
             u2dh = umod                                    ! AM: INCLUDE STOKES DRIFT?
             cdrag = ag/(cz**2) 
             taucur = rhomean*cdrag*u2dh**2               ! current related bed shear stress
             !cdrag = cfuhi(L)*hu(L)
             !
             ! parameterized models
             !
             call getymxpar(modind,tauwav, taucur, fw, cdrag, abscos, yparL, ymxpar)
             ypar(L) = yparL
             !!!if (tauwav<1.0E-8) then
             !!!    xpar    = 1.0d0                               ! X-parameter in D3D-FLOW manual 9.7.5
             !!!    ypar(L) = 1.0d0                               ! Y-parameter in D3D-FLOW manual 9.7.5
             !!!    ymxpar  = 1.0d0                               ! Z-parameter in D3D-FLOW manual 9.7.5
             !!!else
             !!!    xpar = taucur/(taucur + tauwav)
             !!!    if (xpar<1.0d-8 .or. modind==9) then
             !!!        ypar(L)= 0.0d0
             !!!        ymxpar = 1.0d0
             !!!    else
             !!!        lfc    = log10(fw/cdrag)
             !!!        !
             !!!        cj     = abscos**coeffj(modind)
             !!!        coeffb = (bb(modind, 1) + bb(modind, 2)*cj)                     &
             !!!               & + (bb(modind, 3) + bb(modind, 4)*cj)*lfc
             !!!        coeffp = (pp(modind, 1) + pp(modind, 2)*cj)                     &
             !!!               & + (pp(modind, 3) + pp(modind, 4)*cj)*lfc
             !!!        coeffq = (qq(modind, 1) + qq(modind, 2)*cj)                     &
             !!!               & + (qq(modind, 3) + qq(modind, 4)*cj)*lfc
             !!!        ypar(L)= xpar*(1.0d0 + coeffb*(xpar**coeffp)*((1.0d0 - xpar)**coeffq))
             !!!        !
             !!!        ci     = abscos**coeffi(modind)
             !!!        coeffa = (aa(modind, 1) + aa(modind, 2)*ci)                     &
             !!!               & + (aa(modind, 3) + aa(modind, 4)*ci)*lfc
             !!!        coeffm = (mm(modind, 1) + mm(modind, 2)*ci)                     &
             !!!               & + (mm(modind, 3) + mm(modind, 4)*ci)*lfc
             !!!        coeffn = (nn(modind, 1) + nn(modind, 2)*ci)                     &
             !!!               & + (nn(modind, 3) + nn(modind, 4)*ci)*lfc
             !!!        ymxpar = 1.0d0 + coeffa*(xpar**coeffm)*((1.0d0 - xpar)**coeffn)
             !!!    endif
             !!!endif
             !
             ! bottom friction for combined waves and current
             !
             taubxu(L) = ymxpar*(taucur + tauwav)                       ! maximum shear stress due to waves and currents, eq to taubxu in D3D
             ! ypar*(taucur + tauwav) is mean shear stress
             if (modind < 9) then
                !tauwci = ypar*(taucur + tauwav)
                !!
                !! primary and secondary bottom friction terms
                !!
                !taubpu(L) = tauwci/(umod*rhomean + waveps)             ! D3D style: taubpu = (g*U)/C**2
                !
                ! no waveps needed here: hu>0 and umod=max(umod,waveps)
                cfwavhi(L) = tauwav/ (rhomean*umod**2)*min(huvli(L),hminlwi)   ! tau = cf * rhomean * ||u|| u, and tau/(rho h) appears in (depth-averaged) momentum equation and in D3D taubpu = tau/ (rho ||u||)                                         
             elseif (modind==9) then
                ac1 = acl(L); ac2 = 1d0-ac1
                uorbhs   = sqrt(2.0d0)*uorbu
                hrmsu    = ac1*fwx%hrms(k1)+ac2*fwx%hrms(k2)
                rlabdau  = ac1*fwx%rlabda(k1)+ac2*fwx%rlabda(k2)
                rr       = -0.4d0 * sqrt(2d0) / hu(L) + 1d0
                umax     = rr * 2d0 * uorbhs
                t1       = tpu  * sqrt(ag/hu(L))
                u11      = umax / sqrt(ag*hu(L))
                a11      = -0.0049_fp*t1**2 - 0.069_fp*t1 + 0.2911_fp
                raih     = max(0.5_fp , -5.25_fp-6.1_fp*tanh(a11*u11-1.76_fp))
                rmax     = max(0.62_fp , min(0.75_fp , -2.5_fp*hu(L)/max(rlabdau,1.0e-20_fp) + 0.85_fp))
                uon      = umax * (0.5_fp + (rmax-0.5_fp)*tanh((raih-0.5_fp)/(rmax-0.5_fp)))
                uoff     = umax - uon
                uon      = max(1.0e-5_fp , uon)
                uoff     = max(1.0e-5_fp , uoff)
                uwbih    = (0.5_fp*uon**3.0_fp + 0.5_fp*uoff**3.0_fp)**(1.0_fp/3.0_fp)   
                rksru    = ac1*bfmpar%rksr(k1)+ac2*bfmpar%rksr(k2)                            ! these exist, okay
                rksmru   = ac1*bfmpar%rksmr(k1)+ac2*bfmpar%rksmr(k2)
                !
                ! Van Rijn 2004 formulation
                !
                phivr      = acos((uuu*costu+vvv*sintu) / umod)
                gamma      = 0.8_fp + phivr - 0.3_fp*phivr**2
                ksc        = sqrt(rksru**2 + rksmru**2)
                uratio     = min(uwbih/(u2dh+waveps) , 5.0_fp)
                ka         = ksc * exp(gamma*uratio)
                ka         = min(ka , 10.0_fp*ksc , 0.2_fp*hu(L))
                ca         = 18.0_fp * log10(12.0_fp*hu(L)/max(ka,waveps))
                cfhi_vanrijn(L) = min(huvli(L),hminlwi)*ag / ca**2         ! umod * rhomean * ag * umod / ca**2
             endif

             if (tpu > 1d-1) then
                ks         = z0*33d0
                omega      = 2d0 * pi / tpu
                a          = uorbu / omega
                !
                wblt = 0.09d0 * alfaw * (ks/max(hu(L),epshu)) * (a/ks)**0.82_fp
                wblt = max(alfaw*ee*z0/hu(L) , wblt)
                fwx%wblt(L) = min(0.5_fp, wblt)*hu(L)
             else
                fwx%wblt(L) = 0d0
             endif
          endif
       endif
       !
       if (modind == 0) then      
          if (hu(L)>epshu) then
             z0urou(L) = hu(L)/(ee*(exp(vonkar*cz/sag) - 1d0))
             rz = 1d0 + hu(L)/(ee*z0urou(L))
             cf = log(rz)/vonkar
             cwall         = 1d0/(cf**2)            
             taubxu(L)    = rhomean*cwall*umod*umod
          endif
       else
          if (hu(L) > epshu) then
             ! Avoid z0 of zero
             ust  = sqrt(ypar(L)*(taucur + tauwav)/rhomean)
             if (ust > waveps) then
                cf = min(umod/ust,40.0_fp)
                z0urou(L) = hu(L)/((exp(vonkar*cf) - 1d0)*ee)
                z0urou(L) = min(z0urou(L), 10d0)
                !
             endif
             if (modind == 9) then
                z0urou(L) = max(3.33e-5_fp , ka/30.0)
             endif
          endif
       endif
   enddo
      
   !      In Delft3D this parameter is named 'maximum bottom friction' via the taumax
   !      This is NOT the same as 'bed shear stress' which is defined in Delft3D as rhow*(taubpu*u1 + taubsu)
   !      MIND: taus computed here ~= gettaus!!
   taus(:)   = 0.0d0
   do L=1,LNx
      k1=ln(1,L)
      k2=ln(2,L)
       if (hu(L) > epshu) then
           taus(k1) = taus(k1) + taubxu(L)*wcL(1,L)
           taus(k2) = taus(k2) + taubxu(L)*wcL(2,L)
       end if
   enddo

end subroutine tauwave
 
subroutine wave_uorbrlabda()
   use m_waves, only: uorb, wlenwav, uorbwav, twav, hwav, gammax, rlabda, jauorb
   use m_flow, only: hs
   use m_flowgeom, only: ndx
   use m_physcoef, only: ag
   use m_sferic, only: pi

   implicit none
   
   integer                            :: k
   integer                            :: uorbwav_from_SWAN=0
   integer                            :: wlenwav_from_SWAN=0
   
   double precision                   :: hss, per, omeg, k0, k0h, rk, uorb1
   
   do k = 1,ndx
   
       hss  = max(0.01, hs(k))                    
       per = max(0.01, twav(k))                   ! wave period
   
       hwav(k) = min(hwav(k), gammax*hs(k))       ! Prevent unrealistic Hrms in shallow water
       omeg       = 2.0*pi/per
       k0         = omeg*omeg/ag
       k0h        = k0*hss
       if (k0h>pi) then                ! if deep water
           rk = k0
       elseif (k0h<0.005) then         ! if very shallow water
           rk = omeg/sqrt(ag*hss)
       else
           call getwavenr(hss,per,rk)
       endif
       if (wlenwav_from_SWAN.eq.1) then
           rlabda(k) = wlenwav(k)      
       else
           rlabda(k) = 2.0*pi/rk
       endif
       if (rk*hss<80d0) then            ! if not very deep water
           if (uorbwav_from_SWAN.eq.1) then
               Uorb(k)    = uorbwav(k) 
           else                        
               Uorb(k)      = 0.5d0*hwav(k)*omeg/sinh(rk*hss)
               !Uorb(k)    = uorb1*sqrt(pi)/2d0                            ! See note Dano on orbital velocities in D3D, SWAN and XBeach
           endif
       else
           Uorb(k) = 0d0
       endif
   enddo
   
   if (jauorb==0) then       ! old d3d convention
      uorb = uorb*sqrt(pi)/2d0
   end if
     
end subroutine wave_uorbrlabda

   
subroutine setmodind(rouwav, modind)
 implicit none
 integer     :: modind
 character*4 rouwav
 if (rouwav=='FR84') then
     modind = 1
 elseif (rouwav=='MS90') then
     modind = 2
 elseif (rouwav=='HT91') then
     modind = 3
 elseif (rouwav=='GM79') then
     modind = 4
 elseif (rouwav=='DS88') then
     modind = 5
 elseif (rouwav=='BK67') then
     modind = 6
 elseif (rouwav=='CJ85') then
     modind = 7
 elseif (rouwav=='OY88') then
     modind = 8
 elseif (rouwav=='VR04') then
     modind = 9
 !else 
 !    modind = 0
 endif
end subroutine setmodind

!> subroutine to compute wave forces from SWAN output
!> originates from Bas Stengs, extended by AM
subroutine setwavfu()
   use unstruc_messages
   use MessageHandling
   use m_flowparameters
   use m_flowgeom
   use m_flow, only: hu, huvli, wavfu, rhomean
   use m_waves
   use m_physcoef, only: ag
   implicit none
   
   integer          :: mout
   double precision :: wavfx, wavfy, wavfbx, wavfby
   double precision :: wavfu_loc, wavfbu_loc, twavL
   double precision :: fmax, ac1, ac2, hminlwi
   
   integer          :: L, k1, k2
   
   ! todo: compute only once after (re)initialization
   facmax = 0.25d0*sqrt(ag)*rhomean*gammax**2
   hminlwi = 1d0/hminlw
   wavfu = 0d0
   !sywav = 0d0
   
   do L = 1,lnx   
       if (hu(L) < epshu) cycle
       if (L > lnx1D) then         
           k1 = ln(1,L) ; k2 = ln(2,L)
           ac1 = acl(L)
           ac2 = 1d0-ac1
           ! interpolation from flownodes to flowlinks
           wavfx = ac1*sxwav(k1) + ac2*sxwav(k2)        
           wavfy = ac1*sywav(k1) + ac2*sywav(k2)  
   
           wavfbx = ac1*sbxwav(k1) + ac2*sbxwav(k2)        
           wavfby = ac1*sbywav(k1) + ac2*sbywav(k2)        
   
           twavL = ac1*twav(k1)   + ac2*twav(k2)
   
           ! projection in face-normal direction
           wavfu_loc  = wavfx*csu(L)  + wavfy*snu(L)                
           wavfbu_loc = wavfbx*csu(L) + wavfby*snu(L)
   
           ! limit forces
           fmax       = facmax*hu(L)**1.5 / max(0.1d0, twavL)
   
           wavfu_loc  = min(max(wavfu_loc, -fmax),fmax)
           wavfbu_loc = min(max(wavfbu_loc,-fmax),fmax)
   
           ! for 3D: account for relative top-layer height in wavfu_loc, e.g.
           !         wavfu(L) = wavfu_loc * dz(L)/hu(LL) + wavfbu_loc
           wavfu(L) = wavfu_loc + wavfbu_loc
       else
           ! then get data from network points. Turn off for now..
       endif
       !wavfu(L) = wavfu(L)/ (rhomean*hu(L))                       ! Dimensions [m/s^2]
       wavfu(L) = wavfu(L) * min(huvli(L), hminlwi) / rhomean       ! Dimensions [m/s^2]
   enddo
   
end subroutine setwavfu


subroutine setwavmubnd()
   use m_flowgeom
   use m_flowparameters
   use m_flowexternalforcings
   use m_flow, only: hu, huvli, wavmubnd
   use m_waves
   implicit none
   
   double precision :: ac1, ac2
   
   integer          :: kb, ki, L, n
   double precision :: hminlwi
   
   hminlwi = 1d0/hminlw
   
      !  wavmubnd is defined on the whole mesh, but has non-zero values at the open boundaries only
      wavmubnd = 0d0
      
      do n=1,nbndu
         kb = kbndu(1,n)
         ki = kbndu(2,n)
         L  = kbndu(3,n)
         ! interpolate cell-centered mass fluxes to flow links
         if (hu(L) < epshu) cycle
         ac1 = acl(L)
         ac2 = 1d0-ac1
         wavmubnd(L) = (ac1*mxwav(kb) + ac2*mxwav(ki)) * csu(L) + &
                       (ac1*mywav(kb) + ac2*mywav(ki)) * snu(L)
   
         wavmubnd(L) = wavmubnd(L) * min(huvli(L),hminlwi)
      end do
      
      do n=1,nbndz
         if ( kbndz(4,n).eq.5 ) then   ! riemann boundaries
            kb = kbndz(1,n)
            ki = kbndz(2,n)
            L  = kbndz(3,n)
            if (hu(L) < epshu) cycle
            if ( wavmubnd(L).ne.0d0 ) cycle
            ! interpolate cell-centered mass fluxes to flow links
            ac1 = acl(L)
            ac2 = 1d0-ac1
            wavmubnd(L) = (ac1*mxwav(kb) + ac2*mxwav(ki)) * csu(L) + &
                          (ac1*mywav(kb) + ac2*mywav(ki)) * snu(L)
   
            wavmubnd(L) = wavmubnd(L) * min(huvli(L),hminlwi)
         end if
      end do
      
      !  normal-velocity boundaries
      do n=1,nbndn
         kb = kbndn(1,n)
         ki = kbndn(2,n)
         L  = kbndn(3,n)
         if (hu(L) < epshu) cycle
         if ( wavmubnd(L).ne.0d0 ) cycle
         ! interpolate cell-centered mass fluxes to flow links
         ac1 = acl(L)
         ac2 = 1d0-ac1
         wavmubnd(L) = (ac1*mxwav(kb) + ac2*mxwav(ki)) * csu(L) + &
                       (ac1*mywav(kb) + ac2*mywav(ki)) * snu(L)
   
         wavmubnd(L) = wavmubnd(L) * min(huvli(L),hminlwi)
      end do
      
      !  tangential-velocity boundaries: not needed to define mass fluxes

   return
end subroutine setwavmubnd

subroutine wave_comp_stokes_velocities()
   use m_flowparameters
   use m_flowgeom
   use m_flow, only: hu, huvli
   use m_waves
   implicit none

   double precision :: Mu, Mv, hminlwi    ! link-based and link-oriented wave-induced volume fluxes

   integer :: k1, k2, L
   integer :: ierror ! error (1) or not (0)

   ierror = 1
   hminlwi = 1d0/hminlw
   ustokes = 0d0
   vstokes = 0d0

   do L=1,Lnxi ! [TRUNKMERGE] JR: Was lnx in trunk.
      if ( hu(L).gt.epswav ) then
         k1 = ln(1,L); k2 = ln(2,L)
         Mu =    acL(L) *(csu(L)*(Mxwav(k1)) + snu(L)*(Mywav(k1))) + &
            (1d0-acL(L))*(csu(L)*(Mxwav(k2)) + snu(L)*(Mywav(k2)))

         Mv =    acL(L) *(-snu(L)*(Mxwav(k1)) + csu(L)*(Mywav(k1))) + &
            (1d0-acL(L))*(-snu(L)*(Mxwav(k2)) + csu(L)*(Mywav(k2)))
         ustokes(L) = Mu * min(huvli(L),hminlwi)                           ! "Corrects" for unphysical longshore drift along waterline 
         vstokes(L) = Mv * min(huvli(L),hminlwi)
      else
         ustokes(L) = 0d0
         vstokes(L) = 0d0
      end if
   end do
   
   do L=lnxi+1,lnx                   ! Randen: Neumann
      if (hu(L)>epswav)  then
         k1 = ln(1,L) ! buiten
         k2 = ln(2,L) ! binnen
         Mxwav(k1) = Mxwav(k2);  Mywav(k1) = Mywav(k2)
         Mu =    acL(L) *(csu(L)*(Mxwav(k1)) + snu(L)*(Mywav(k1))) + &
               (1d0-acL(L))*(csu(L)*(Mxwav(k2)) + snu(L)*(Mywav(k2)))
      
         Mv =    acL(L) *(-snu(L)*(Mxwav(k1)) + csu(L)*(Mywav(k1))) + &
            (1d0-acL(L))*(-snu(L)*(Mxwav(k2)) + csu(L)*(Mywav(k2)))
         ustokes(L) = Mu * min(huvli(L),hminlwi)                           
         vstokes(L) = Mv * min(huvli(L),hminlwi)
      else
         ustokes(L) = 0d0
         vstokes(L) = 0d0
      end if
   end do

   ierror = 0
1234 continue
   return
end subroutine wave_comp_stokes_velocities

