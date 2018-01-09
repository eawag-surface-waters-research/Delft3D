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

! $Id: waves.f90 52266 2017-09-02 11:24:11Z klecz_ml $
! $HeadURL: https://repos.deltares.nl/repos/ds/branches/dflowfm/20161017_dflowfm_codecleanup/engines_gpl/dflowfm/packages/dflowfm_kernel/src/waves.f90 $
 subroutine tauwaveswan()
 use m_sediment
 use m_sferic
 use m_flowparameters
 use m_flow, only: plotlin, rhog, rhomean, ag, s1, s0, taubpu, hu, jaconveyance2D, hs, u1, v, taus, frcu, ifrcutp, cfuhi, wavmu, huvli
 use m_flowgeom
 use m_physcoef, only: frcuni, ifrctypuni, rhomean, ee, sag, vonkar
 use m_waves
 use unstruc_messages

 implicit none
 double precision           :: uorb1, k0, k0h, phigrid, phiwave, phi
 integer                    :: k, jatauw = 1, wlenwav_from_SWAN = 0, uorbwav_from_SWAN = 0
 integer                    :: n
 double precision           :: hk, sh2hk,hksh2,rn,asg,ew,sxx,syy,dtau,shs, h2k, cc, cg, omeg, ustokesbas
 double precision           :: dsk2, rk, astar, fw, hss, per, astarc, tauwav, taucur, tauwci, cdrag, z0, uorbu, tpu
 double precision           :: cz, frcn, uuu, vvv, umod, umodsq, cvalue, costu, sintu, abscos, uorbhs, waveps, u2dh
 double precision           :: xpar, ymxpar, lfc, cj, coeffb, coeffp, coeffq, ci,coeffa, coeffm, coeffn, yparL
 double precision           :: hpr, wu2, b21, ai, BL1, BL2, hus
 double precision           :: ar
 integer                    :: L, ifrctyp, k1, k2
 logical                    :: actual_avg

 double precision, external         :: tanhsafe, sinhsafe, sinhsafei

    double precision, dimension(8)             :: coeffi     ! Coefficient i in expression for parametrized models
    double precision, dimension(8)             :: coeffj     ! Coefficient j in expression for parametrized models
    double precision, dimension(8, 4)          :: aa         ! Coefficient a(i) in expression for parameter a
    double precision, dimension(8, 4)          :: bb         ! Coefficient b(i) in expression for parameter b
    double precision, dimension(8, 4)          :: mm         ! Coefficient m(i) in expression for parameter n
    double precision, dimension(8, 4)          :: nn         ! Coefficient n(i) in expression for parameter n
    double precision, dimension(8, 4)          :: pp         ! Coefficient p(i) in expression for parameter p
    double precision, dimension(8, 4)          :: qq         ! Coefficient q(i) in expression for parameter q

!!! ! Data statemens
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
!!!    !-----for Tau_max
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


waveps = 1d-4          ! avoid a zero denominator

 ! FIRST COMPUTE UORB (here Uorb), copy from setwav.f90 (Delft3D)

 do k = 1,ndx       ! Do for all cells (FLOW NODE STATEMENT)

     ! Start computing uorb from copied material
     hss  = max(0.01, hs(k))                    ! total water height at cell center~= zero. Bas; In D3D the s0 is taken instead of the s1 (old time step)
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
         rlabda(k) = wlenwav(k)      ! If wave length exists from SWAN, use this one. Bas; strange statement, should be defined before 'rk'.
     else
         rlabda(k) = 2.0*pi/rk
     endif
     if (rk*hss<80.) then            ! if not very deep water
         if (uorbwav_from_SWAN.eq.1) then
             Uorb(k)    = uorbwav(k) ! Bas: if Uorb exist from SWAN, use this one
         else                        ! if Uorb not exist from SWAN
             uorb1      = 0.5d0*hwav(k)*omeg/sinh(rk*hss)
             Uorb(k)    = uorb1*sqrt(pi)/2d0      ! sqrt(pi)/2d0 needs to be removed?
         endif
     else
         Uorb(k) = 0.0
     endif
 enddo

 ! write(*,*) "max(uorb):", maxval(uorb)

 
 ! parameterized bottom friction models

 ! NOW INCLUDE WAVE EFFECTS

 ! initialize grid indices
 do L = 1,lnx   ! Do statement over nr of flow links (internal + boundary)      (FLOW LINK STATEMENT)
     !
     ! kcscuttest = .false.
     ! actual_avg =      (cstbnd .and. .not.zmodel .and. (kcs(nm)==2 .or. kcs(nmu)==2)) &
     !            & .or. (kcs(nm)==3 .or. kcs(nmu)==3)                                  &
     !            & .or. (zmodel .and. kcscuttest)
     actual_avg = .false.                   ! assume for now that this is not relevant
     ! Use Eulerian velocities (U_GLM = U_Eulerian + U_stokes)..
     uuu = u1(L) - ustokes(L)
     if (actual_avg) then
     !     svvv = max(kfv(ndm) + kfv(ndmu) + kfv(nm) + kfv(nmu), 1)
     !     vvv  = (v1(ndm, kmaxx)*kfv(ndm) + v1(ndmu, kmaxx)*kfv(ndmu)         &
     !          & + v1(nm, kmaxx)*kfv(nm) + v1(nmu, kmaxx)*kfv(nmu))/svvv
     else
         if (jaconveyance2D >=3 .or. L <= lnx1D ) then      ! based on subroutine furu
             vvv = 0.0d0                                    ! Consider Lagrangian velocities (U_GLM = U_Eulerian + U_stokes)..
         else
             ! Use Eulerian velocities (U_GLM = U_Eulerian + U_stokes)..
             vvv = v(L) - vstokes(L)
         endif
     endif
     umodsq    = uuu*uuu + vvv*vvv
     umod      = max(1.0d-4, sqrt(umodsq))
     taubpu(L) = 0.0d0                      ! Set to zero, to consider the case with waves
     ypar(L)   = 0.0d0
     cfwavhi(L)= 0.0d0
      
     ! interpolate uorbu, tpu and wavmu from flownodes to flowlinks
     uorbu = acl(L)*uorb(ln(1,L)) + (1.0d0-acl(L))*uorb(ln(2,L))
     tpu   = acl(L)*twav(ln(1,L)) + (1.0d0-acl(L))*twav(ln(2,L))

     wavmu(L) = (acl(L)*mxwav(ln(1,L)) + (1.0d0-acl(L))*mxwav(ln(2,L))) * csu(L) + &
                (acl(L)*mywav(ln(1,L)) + (1.0d0-acl(L))*mywav(ln(2,L))) * snu(L)


     if (hu(L) > 0) then                    ! For all wet cells. Delft3D uses kfu statement here
         !
         ! angle between waves and current
         !
         !
         ! TO DO: Replace the following messing with angles by an inproduct, without
         ! the expensive atan2 call
         !
         ! phigrid: angle between "normal direction on link" and "positive x-axis"
         phigrid = atan2(snu(L),csu(L)) / dg2rd
         ! phiwave: angle between "wave propagation direction" and "positive x-axis"
         !          Interpolate from nodes to links
         phiwave = acl(L)*phiwav(ln(1,L)) + (1.0d0-acl(L))*phiwav(ln(2,L))
         ! phi: angle between "wave propagation direction" and "normal direction on link"
         phi     = phiwave - phigrid
         
         costu = dcos(dg2rd*phi)     
         sintu = dsin(dg2rd*phi)     
         
         astarc = 30.*pi**2     ! critical value for astar

         abscos = abs(uuu*costu + vvv*sintu)/umod
         
         ! get roughness height
         call getczz0(hu(L),dble(frcu(L)),ifrcutp(L),cz,z0)
         ! z0 = z0ucur(L)
         
         !
         ! wave friction factor and drag coefficient
         !
         uorbhs = sqrt(2.0d0)*uorbu
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
         taubu(L) = ymxpar*(taucur + tauwav)                       ! shear stress due to waves and current
         !
         if (modind < 9) then
             !tauwci = ypar*(taucur + tauwav)
             !!
             !! primary and secondary bottom friction terms
             !!
             !taubpu(L) = tauwci/(umod*rhomean + waveps)             ! D3D style: taubpu = (g*U)/C**2
             !
             ! no waveps needed here: hu>0 and umod=max(umod,waveps)
             cfwavhi(L) = tauwav/ (rhomean*umod**2)*huvli(L)   ! tau = cf * rhomean * ||u|| u, and tau/(rho h) appears in (depth-averaged) momentum equation and in D3D taubpu = tau/ (rho ||u||)
         endif
         !
     else ! Delft3D: kfu(nm)<>1    (if the cell is dry)
         !
         ! "this is essential for TRATUR"
         !
        ! dfu(L)    = 0.0d0
        ! deltau(L) = 0.0d0
     endif
 enddo

 ! PUT taubu to wl-points (taus): based on subroutine setumod
 ! Bas; In Delft3D this parameter is named 'maximum bottom friction' via the taumax
 !      This is NOT the same as 'bed shear stress' which is defined in Delft3D as rhow*(taubpu*u1 + taubsu)
 !      MIND: taus computed here ~= gettaus!!
 taus(:)   = 0.0d0
 do L=1,LNx
    k1=ln(1,L)
    k2=ln(2,L)
     if (hu(L) > epshu) then
         taus(k1) = taus(k1) + taubu(L)*wcL(1,L)
         taus(k2) = taus(k2) + taubu(L)*wcL(2,L)
     end if
 enddo

 end subroutine tauwaveswan

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
 endif
 end subroutine setmodind

!> subroutine to compute wave forces from SWAN output
!>   originates from Bas Stengs, extended by AM
subroutine setwavfu()
use unstruc_messages
use MessageHandling
use m_flowparameters
use m_flowgeom
use m_flow, only: hu, huvli, wavfu, rhomean
use m_waves
use m_physcoef, only: ag
implicit none

double precision :: wavfx, wavfy, wavfbx, wavfby
double precision :: wavfu_loc, wavfbu_loc, twavL
double precision :: fmax, ac1, ac2

integer          :: L, k1, k2

! todo: compute only once after (re)initialization
facmax = 0.25d0*sqrt(ag)*rhomean*gammax**2

wavfu = 0d0

! Set relevant data to velocity points for 1D case: based on subroutine addlink2D
do L = 1,lnx   ! Do statement over nr of flow links (internal + boundary)      (FLOW LINK STATEMENT)
    if (hu(L) < epshu) cycle
    if (L > lnx1D) then         ! then get data from cell centre. Not sure about the '>' sign.. Also 1D/2D phenomena
        k1 = ln(1,L) ; k2 = ln(2,L)
        ac1 = acl(L)
        ac2 = 1d0-ac1
        ! interpolation from flownodes to flowlinks
        wavfx = ac1*sxwav(k1) + ac2*sxwav(k2)        ! Define variable on velocity point determined by weighted factor
        wavfy = ac1*sywav(k1) + ac2*sywav(k2)  

        wavfbx = ac1*sbxwav(k1) + ac2*sbxwav(k2)        ! Define variable on velocity point determined by weighted factor
        wavfby = ac1*sbywav(k1) + ac2*sbywav(k2)        ! Define variable on velocity point determined by weighted factor

        twavL = ac1*twav(k1)   + ac2*twav(k2)

        ! projection in face-normal direction
        wavfu_loc  = wavfx*csu(L)  + wavfy*snu(L)                ! bas; in theory not certain, but dimensions are correct [N/m^2]
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
    !wavfu(L) = wavfu(L)/ (rhomean*hu(L))           ! Dimensions [m/s^2]
    wavfu(L) = wavfu(L) * huvli(L) / rhomean        ! Dimensions [m/s^2]
enddo
end subroutine setwavfu


subroutine setwavmubnd()
use m_flowgeom
use m_flowparameters
use m_flowexternalforcings
use m_flow, only: hu, wavmubnd
use m_waves
implicit none

double precision :: ac1, ac2

integer          :: kb, ki, L, n

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

      wavmubnd(L) = wavmubnd(L) / hu(L)
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

         wavmubnd(L) = wavmubnd(L) / hu(L)
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

      wavmubnd(L) = wavmubnd(L) / hu(L)
   end do
   
   !  tangential-velocity boundaries: not needed to define mass fluxes

return
end subroutine setwavmubnd

!
!
! ==========================================================================
!> 
!--------------------------------------------------------------------
!  Delft3D-Wave coupling
!--------------------------------------------------------------------
subroutine wave_comp_stokes_velocities()
   use m_flowparameters
   use m_flowgeom
   use m_flow, only: hu, huvli
   use m_waves
   implicit none

   double precision :: Mu, Mv    ! link-based and link-oriented wave-induced volume fluxes

   integer :: k1, k2, L
   integer :: ierror ! error (1) or not (0)

   ierror = 1

   do L=1,Lnx
      if ( hu(L).gt.epswav ) then
         k1 = ln(1,L); k2 = ln(2,L)
         Mu =    acL(L) *(csu(L)*(Mxwav(k1)) + snu(L)*(Mywav(k1))) + &
            (1d0-acL(L))*(csu(L)*(Mxwav(k2)) + snu(L)*(Mywav(k2)))

         Mv =    acL(L) *(-snu(L)*(Mxwav(k1)) + csu(L)*(Mywav(k1))) + &
            (1d0-acL(L))*(-snu(L)*(Mxwav(k2)) + csu(L)*(Mywav(k2)))
         ustokes(L) = Mu * huvli(L)
         vstokes(L) = Mv * huvli(L)
    
         !ustokes(L) = snu(L)*0.5d0/0.7d0 
         !vstokes(L) = csu(L)*0.5d0/0.7d0 
    
      else
         ustokes(L) = 0d0
         vstokes(L) = 0d0
      end if
   end do

   ierror = 0
1234 continue
   return
end subroutine wave_comp_stokes_velocities
