!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      subroutine respup ( pmsa   , fl     , ipoint , increm , noseg  ,
     +                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     +                    noq3   , noq4   )
      use m_dhkmrk


!***********************************************************************
!
!     Description of the module :
!
!        RESUSPENSION FORMULAS van Rijn Pick-up
!
! Name    T   L I/O   Description                                    Units
! ----    --- -  -    -------------------                            -----
! IM1S2         I IM1 in layer S2                           (gDM/m2)
! Tau           I total bottom shear stress                   (N/m2)
! TauShields    I Shields shear stress for resusp. pick-up    (N/m2)
! GRAIN50       I Grain size (D50)                               (m)
! GRAV          I Gravitational acceleration                  (m/s2)
! KinViscos     I Kinematic viscosity                         (m2/s)
! PowNs2Pup     I Power of shear stress in pick-up resuspension layer S2 (-)
! RHOSAND       I bulk density sand                         (gDM/m3)
! RhoWater      I density of water                           (kg/m3)
! PORS2         I porosity of sediment layer S2     (m3pores/m3bulk)
! ThickS2       I thickness of layer S2 van Rijn pick-up resusp. (m)
! Surf          I horizontal surface area of a DELWAQ segment   (m2)
! Depth         I depth of segment                               (m)
! DELT          I timestep for processes                         (d)
! MinDepth      I minimum waterdepth for sedimentation           (m)
! MaxResPup     I Maximum resuspension pick-up              (g/m2/d)
! FactResPup    I Factor in  resuspension pick-up (3.3e-4)       (-)
! fResS2Pup     O pick-up resuspension flux IM1 from S2     (g/m2/d)
! Pshields      O resuspension probability S2 pick-up            (-)
! FrIM1S2Pup    O fraction IM1 in layer S2 pick-up         (gDM/gDM)
! dResS2Pup     F pick-up resuspension flux IM1 from S2     (g/m3/d)

      implicit none

      real(4) pmsa(*)     !i/o process manager system array, window of routine to process library
      real(4) fl(*)       ! o  array of fluxes made by this process in mass/volume/time
      integer ipoint( 26) ! i  array of pointers in pmsa to get and store the data
      integer increm( 26) ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! i  number of computational elements in the whole model schematisation
      integer noflux      ! i  number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! i  active-inactive, surface-water-bottom, see manual for use
      integer noq1        ! i  nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! i  nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! i  nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt( 26)   !    local work array for the pointering
      integer iseg        !    local loop counter for computational element loop

      real     im1s2, im2s2, im3s2

      integer iflux
      integer ikmrk2
      real(4) tau
      real(4) tcrrs2
      real(4) grain50
      real(4) grav
      real(4) kinviscos
      real(4) powns2pup
      real(4) rhosand
      real(4) rhowater
      real(4) pors2
      real(4) thicks2
      real(4) surf
      real(4) depth
      real(4) delt
      real(4) mindep
      real(4) maxrespup
      real(4) factrespup
      integer swfrims2
      real(4) press2
      real(4) frim1s2pup
      real(4) frim2s2pup
      real(4) frim3s2pup
      real(4) tims2
      real(4) frtims2pup
      real(4) flrim1s2
      real(4) flrim2s2
      real(4) flrim3s2
      real(4) flres2
      real(4) rhosandkg
      real(4) s
      real(4) dster
      real(4) rest
      real(4) rfdms2
      real(4) rfim1s2
      real(4) rfim2s2
      real(4) rfim3s2
      real(4) mrim1s2
      real(4) mrim2s2
      real(4) mrim3s2

      ipnt        = ipoint

      iflux = 0
      do 9000 iseg = 1 , noseg
      if (btest(iknmrk(iseg),0)) then
      call dhkmrk(2,iknmrk(iseg),ikmrk2)
      if ((ikmrk2.eq.0).or.(ikmrk2.eq.3)) then

      im1s2      = pmsa( ipnt(1 ) )
      im2s2      = pmsa( ipnt(2 ) )
      im3s2      = pmsa( ipnt(3 ) )
      tau        = pmsa( ipnt(4 ) )
      tcrrs2     = pmsa( ipnt(5 ) )
      grain50    = pmsa( ipnt(6 ) )
      grav       = pmsa( ipnt(7 ) )
      kinviscos  = pmsa( ipnt(8 ) )
      powns2pup  = pmsa( ipnt(9 ) )
      rhosand    = pmsa( ipnt(10) )
      rhowater   = pmsa( ipnt(11) )
      pors2      = pmsa( ipnt(12) )
      thicks2    = pmsa( ipnt(13) )
      surf       = pmsa( ipnt(14) )
      depth      = pmsa( ipnt(15) )
      delt       = pmsa( ipnt(16) )
      mindep     = pmsa( ipnt(17) )
      maxrespup  = pmsa( ipnt(18) )
      factrespup = pmsa( ipnt(19) )
      swfrims2   = nint(pmsa( ipnt(20) ))

!***********************************************************************
!**** Processes connected to the RESUSENSION van Rijn Pick-up
!***********************************************************************

      press2 = 0.0

!     Calculate resuspension probability in S2
      if (tau .eq. -1.0) then
         press2 = 1.0
      else
!        Compare with critical shear stress
         press2 = max ( 0.0, (tau/tcrrs2 - 1.0) )
      endif

!     Fraction TIM1 in S2
      tims2      = im1s2+im2s2+im3s2
      if (swfrims2 .eq. 1) then
!         mass fraction determined as mass IMx / (mass sand + mass TIM)
          frim1s2pup = im1s2/(rhosand*thicks2*(1.-pors2) + tims2)
          frim2s2pup = im2s2/(rhosand*thicks2*(1.-pors2) + tims2)
          frim3s2pup = im3s2/(rhosand*thicks2*(1.-pors2) + tims2)
          frtims2pup = tims2/(rhosand*thicks2*(1.-pors2) + tims2)
      else
!         original delwaq method: fraction = mass IMx / mass sand
          frim1s2pup = im1s2/(rhosand*thicks2*(1.-pors2))
          frim2s2pup = im2s2/(rhosand*thicks2*(1.-pors2))
          frim3s2pup = im3s2/(rhosand*thicks2*(1.-pors2))
          frtims2pup = tims2/(rhosand*thicks2*(1.-pors2))
      endif

!     No resuspension when depth below min depth
      if ( depth .lt. mindep) then
         flrim1s2 = 0.0
         flrim2s2 = 0.0
         flrim3s2 = 0.0
         flres2   = 0.0
      else

!        Resuspension

         rhosandkg = rhosand/1000.
         s         = rhosandkg/rhowater
         dster     = grain50*((s-1.)*grav/(kinviscos*kinviscos))**(1./3.)
         rest      = factrespup*rhosandkg*((s-1.)*grav*grain50)**0.5
         rfdms2    = frtims2pup*rest*(dster**0.3)*(press2**powns2pup)

         ! Convert  kg/m2/s to g/m2/d

         rfdms2   = rfdms2*1000.*86400.

         ! Maximise by MaxResPup

         rfdms2   = min(rfdms2,maxrespup)

         if ( frtims2pup .gt. 1.e-20 ) then
            rfim1s2 = rfdms2*frim1s2pup/frtims2pup
            rfim2s2 = rfdms2*frim2s2pup/frtims2pup
            rfim3s2 = rfdms2*frim3s2pup/frtims2pup
         else
            rfim1s2 = 0.0
            rfim2s2 = 0.0
            rfim3s2 = 0.0
         endif

         ! Limit resuspension to available material

         mrim1s2 = max (0.0, im1s2 / delt )
         mrim2s2 = max (0.0, im2s2 / delt )
         mrim3s2 = max (0.0, im3s2 / delt )

         flrim1s2 = min ( rfim1s2 , mrim1s2 )
         flrim2s2 = min ( rfim2s2 , mrim2s2 )
         flrim3s2 = min ( rfim3s2 , mrim3s2 )

         flres2   = flrim1s2 + flrim2s2 + flrim3s2

      endif

      fl( 1 + iflux ) = flrim1s2 / depth
      fl( 2 + iflux ) = flrim2s2 / depth
      fl( 3 + iflux ) = flrim3s2 / depth
      pmsa (ipnt (21) ) = flrim1s2
      pmsa (ipnt (22) ) = flrim2s2
      pmsa (ipnt (23) ) = flrim3s2
      pmsa (ipnt (24) ) = flres2
      pmsa (ipnt (25) ) = press2
      pmsa (ipnt (26) ) = frtims2pup

      endif
      endif

      iflux = iflux + noflux
      ipnt  = ipnt + increm

 9000 continue

      return

      end
