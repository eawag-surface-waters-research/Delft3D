!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2022.                                
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

   subroutine tauwave()
   use m_sediment
   use m_sferic
   use m_flowparameters
   use m_flow, only: plotlin, rhog, rhomean, ag, s1, s0, hu, jaconveyance2D, hs, u1, v, taus, frcu, ifrcutp, cfuhi, huvli, z0ucur, z0urou , tausmax
   use m_flowgeom
   use m_flowtimes, only: time1
   use m_physcoef, only:  rhomean, ee, sag, vonkar
   use m_waves
   use m_bedform, only: bfmpar
   use m_turbulence, only: rho
   use unstruc_messages

   implicit none

   double precision           :: uorb1, k0, k0h, phigrid, phiwave, phi
   integer                    :: k, k1, k2, n, L, mout
   integer                    :: jatauw = 1
   double precision           :: hk, sh2hk,hksh2,rn,asg,ew,sxx,syy,dtau,shs, h2k, cc, cg, omeg, hminlwi
   double precision           :: dsk2, rk, astar, fw, hss, per, astarc, tauwav, taucur, tauwci, cdrag, z0, uorbu, tpu
   double precision           :: cz, frcn, uuu, vvv, umod, umodsq, cvalue, costu, sintu, abscos, uorbhs, waveps, u2dh
   double precision           :: xpar, ymxpar, lfc, cj, coeffb, coeffp, coeffq, ci,coeffa, coeffm, coeffn, yparL
   double precision           :: hpr, wu2, b21, ai, BL1, BL2, ust, ac1, ac2, rhoL
   double precision           :: ar, alfaw, wbl, rz, cf, cwall
   double precision           :: a, ks, phivr
   double precision           :: hrmsu, rlabdau, rr,umax,t1,u11,a11,raih,rmax, uon, uoff, uwbih
   double precision           :: rksru, rksmru, gamma, ksc, uratio, ka, ca
   double precision           :: cosk1, cosk2, sink1, sink2
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

   waveps = 1d-8
   alfaw  = 20d0
   hminlwi = 1d0/hminlw

   ! parameterized bottom friction models

   do L = 1,lnx
      k1 = ln(1,L); k2 = ln(2,L)
      ac1 = acl(L); ac2 = 1d0-ac1
      !
      ! Use Eulerian velocities
      uuu = u1(L) - ustokes(L)

      if (jaconveyance2D >=3 .or. L <= lnx1D ) then      ! based on subroutine furu
         vvv = 0.0d0
      else
         ! Use Eulerian velocities
         vvv = v(L) - vstokes(L)
      endif

      umodsq    = uuu*uuu + vvv*vvv
      umod      = max(1.0d-4, sqrt(umodsq))
      taubxu(L) = 0.0d0
      ypar(L)   = 0.0d0
      cfwavhi(L)= 0.0d0
      !
      ! phigrid: angle between "normal direction on link" and "positive x-axis"
      phigrid = atan2(snu(L),csu(L)) * rd2dg
      ! phiwave: angle between "wave propagation direction" and "positive x-axis"
      !          Interpolate from nodes to links
      cosk1 = cos(phiwav(k1)*dg2rd); sink1 = sin(phiwav(k1)*dg2rd)
      cosk2 = cos(phiwav(k2)*dg2rd); sink2 = sin(phiwav(k2)*dg2rd)
      cosk1 = ac1*cosk1+ac2*cosk2; sink1 = ac1*sink1+ac2*sink2
      !
      phiwave = atan2(sink1, cosk1)*rd2dg
      ! phi: angle between "wave propagation direction" and "normal direction on link"
      phi     = phiwave - phigrid
      !phi     = modulo(phi,360d0)
      !if (phi>180d0) phi = 360d0-phi

      ! interpolate uorbu, tpu and wavmu from flownodes to flowlinks
      uorbu = ac1*uorb(k1) + ac2*uorb(k2)
      tpu   = ac1*twav(k1) + ac2*twav(k2)

      ! get water density on flow link
      rhoL = ac1*rho(k1) + ac2*rho(k2)

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
            astar  = tpu*uorbu/max(z0,eps10)

            if (astar>astarc) then
               fw = 0.00251d0*exp(14.1d0/(astar**0.19))
            else                                           ! for relative small uorbs or large friction
               fw = 0.3d0
            endif
            !
            ! magnitude of bottom friction due to waves alone
            ! and due to current alone
            !
            tauwav = 0.5d0*rhomean*fw*ftauw*uorbu*uorbu           ! wave related bed shear stress
            cdrag = ag/(cz**2)
            taucur = rhoL*cdrag*umod*umod                         ! current related bed shear stress
            !
            ! parameterized models
            !
            call getymxpar(modind,tauwav, taucur, fw, cdrag, abscos, yparL, ymxpar)
            ypar(L) = yparL
            !
            ! bottom friction for combined waves and current
            !
            taubxu(L) = ymxpar*(taucur + tauwav)                        ! maximum shear stress due to waves and currents, eq to taubxu in D3D
            taubu(L)  = yparL*(taucur + tauwav)                         ! mean shear stress
            if (modind < 9) then
               !tauwci = ypar*(taucur + tauwav)
               !!
               !! primary and secondary bottom friction terms
               !!
               !taubpu(L) = tauwci/(umod*rhomean + waveps)                    ! D3D style: taubpu = (g*U)/C**2
               !
               ! no waveps needed here: hu>0 and umod=max(umod,waveps)
               cfwavhi(L) = tauwav/ (rhomean*umod**2)*min(huvli(L),hminlwi)   ! tau = cf * rhomean * ||u|| u, and tau/(rho h) appears in (depth-averaged) momentum equation and in D3D taubpu = tau/ (rho ||u||)
            elseif (modind==9) then
               uorbhs   = sqrt(2.0d0)*uorbu
               hrmsu    = ac1*hwav(k1)+ac2*hwav(k2)
               rlabdau  = ac1*rlabda(k1)+ac2*rlabda(k2)
               rr       = -0.4d0 * sqrt(2d0) / hu(L) + 1d0
               umax     = rr * 2d0 * uorbhs
               t1       = tpu  * sqrt(ag/hu(L))
               u11      = umax / sqrt(ag*hu(L))
               a11      = -0.0049d0*t1**2 - 0.069d0*t1 + 0.2911d0
               raih     = max(0.5d0 , -5.25d0-6.1d0*tanh(a11*u11-1.76d0))
               rmax     = max(0.62d0 , min(0.75 , -2.5d0*hu(L)/max(rlabdau,1.0d-20) + 0.85d0))
               uon      = umax * (0.5d0 + (rmax-0.5d0)*tanh((raih-0.5d0)/(rmax-0.5d0)))
               uoff     = umax - uon
               uon      = max(1.0d-5 , uon)
               uoff     = max(1.0d-5 , uoff)
               uwbih    = (0.5d0*uon**3d0 + 0.5d0*uoff**3d0)**(1d0/3d0)
               rksru    = ac1*bfmpar%rksr(k1)+ac2*bfmpar%rksr(k2)                            ! these exist, okay
               rksmru   = ac1*bfmpar%rksmr(k1)+ac2*bfmpar%rksmr(k2)
               !
               ! Van Rijn 2004 formulation
               !
               phivr      = acos((uuu*costu+vvv*sintu) / umod)
               gamma      = 0.8d0 + phivr - 0.3d0*phivr**2
               ksc        = sqrt(rksru**2 + rksmru**2)
               uratio     = min(uwbih/umod , 5.0d0)
               ka         = ksc * exp(gamma*uratio)
               ka         = min(ka , 10d0*ksc , 0.2d0*hu(L))
               ca         = 18d0 * log10(12d0*hu(L)/max(ka,waveps))
               cfhi_vanrijn(L) = min(huvli(L),hminlwi)*ag / ca**2         ! umod * rhomean * ag * umod / ca**2
            endif
         endif
      endif
      !
      if (modind == 0) then
         if (hu(L)>epshu) then
            !z0urou(L) = hu(L)/(ee*(exp(vonkar*cz/sag) - 1d0))
            z0urou(L) = hu(L)*exp(-1d0 - vonkar*cz/sag)            ! for compatibility with setczz0 definitions
            rz = 1d0 + hu(L)/(ee*z0urou(L))
            cf = log(rz)/vonkar
            cwall     = 1d0/(cf**2)
            taubxu(L) = rhoL*cwall*umod*umod
         endif
      else
         if (hu(L) > epshu) then
            ! Avoid z0 of zero
            ust  = sqrt(ypar(L)*(taucur + tauwav)/rhoL)
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
   if (jamaptaucurrent>0) then
      tausmax   = 0d0
      do L=1,LNx
         k1=ln(1,L)
         k2=ln(2,L)
         if (hu(L) > epshu) then
            tausmax(k1) = tausmax(k1) + taubxu(L)*wcL(1,L)
            tausmax(k2) = tausmax(k2) + taubxu(L)*wcL(2,L)
         end if
      enddo
   endif
   !
   taus   = 0d0    ! not in if block, used in GUI
   do L=1,LNx
      k1=ln(1,L)
      k2=ln(2,L)
      if (hu(L) > epshu) then
         taus(k1) = taus(k1) + taubu(L)*wcL(1,L)
         taus(k2) = taus(k2) + taubu(L)*wcL(2,L)
      end if
   enddo

   end subroutine tauwave
