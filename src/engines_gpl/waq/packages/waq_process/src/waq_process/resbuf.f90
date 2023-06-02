!!  Copyright (C)  Stichting Deltares, 2022-2023.
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
  
  subroutine RESBUF     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
  use m_dhkmrk

!
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type         Name         I/O Description
!
      real(4) ::   pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) ::   fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ::   ipoint(45)  ! I  Array of pointers in pmsa to get and store the data
      integer ::   increm(45)  ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer ::   noseg       ! I  Number of computational elements in the whole model schematisation
      integer ::   noflux      ! I  Number of fluxes, increment in the fl array
      integer ::   iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer ::   iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer ::   noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer ::   noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer ::   noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer ::   noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
!
!*******************************************************************************
!     This process replaces RESPUP and S12TIM 
!             
!     Author Jos van Gils
!
!     Type    Name         I/O Description                                        Unit
!
!***********************************************************************
!
!     Description of the module :
!
!        RESUSPENSION FORMULAS van Rijn Pick-up
!
! Name    T   L I/O   Description                              Units
! ----    --- -  -    -------------------                      -----
! IM1S2         I IM1 in layer S2                           (gDM/m2)
! Tau           I total bottom shear stress                   (N/m2)
! TauShields    I Shields shear stress for resusp. pick-up    (N/m2)
! GRAIN50       I Grain size (D50)                               (m)
! GRAV          I Gravitational acceleration                  (m/s2)
! KinViscos     I Kinematic viscosity                         (m2/s)
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

      integer ::   ipnt( 45)   !    local work array for the pointering
      integer ::   iseg        !    local loop counter for computational element loop
      integer ::   iflux
      integer ::   ikmrk2

      ! input items
      real(4) ::   im1s2
      real(4) ::   im2s2
      real(4) ::   im3s2
      real(4) ::   tau
      real(4) ::   tcrrs2
      real(4) ::   grain50
      real(4) ::   grav
      real(4) ::   kinviscos
      real(4) ::   rhosand
      real(4) ::   rhowater
      real(4) ::   pors2
      real(4) ::   thicks2
      real(4) ::   surf
      real(4) ::   depth
      real(4) ::   delt
      real(4) ::   mindep
      real(4) ::   maxrespup
      real(4) ::   factrespup
      integer(4) ::   isw_zf   
      real(4) ::   im1s1    
      real(4) ::   zresim1  
      real(4) ::   vresim1  
      real(4) ::   tcrrim1  
      real(4) ::   im2s1    
      real(4) ::   zresim2  
      real(4) ::   vresim2  
      real(4) ::   tcrrim2  
      real(4) ::   im3s1    
      real(4) ::   zresim3  
      real(4) ::   vresim3  
      real(4) ::   tcrrim3  
      real(4) ::   dms1       
      real(4) ::   dms2     
      
      ! output
      real(4) ::   flrim1s2
      real(4) ::   flrim2s2
      real(4) ::   flrim3s2
      real(4) ::   flres2
      real(4) ::   press2
      real(4) ::   frtims2pup
      real(4) ::   frpoms2pup
      real(4) ::   flrim1s1
      real(4) ::   flrim2s1
      real(4) ::   flrim3s1
      real(4) ::   flrdms1
      real(4) ::   flrdms2

      ! other
      real(4) ::   frim1s2pup
      real(4) ::   frim2s2pup
      real(4) ::   frim3s2pup
      real(4) ::   tims2
      real(4) ::   rhosandkg
      real(4) ::   s
      real(4) ::   dster
      real(4) ::   rest
      real(4) ::   rfdms2
      real(4) ::   rfim1s2
      real(4) ::   rfim2s2
      real(4) ::   rfim3s2
      real(4) ::   mrim1s2
      real(4) ::   mrim2s2
      real(4) ::   mrim3s2
      
      ! new
      real(4) ::   press1im1, press1im2, press1im3, flres1, tims1

      ipnt        = ipoint

      iflux = 0
      do iseg = 1 , noseg
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
            rhosand    = pmsa( ipnt(9 ) )
            rhowater   = pmsa( ipnt(10) )
            pors2      = pmsa( ipnt(11) )
            thicks2    = pmsa( ipnt(12) )
            surf       = pmsa( ipnt(13) )
            depth      = pmsa( ipnt(14) )
            delt       = pmsa( ipnt(15) )
            mindep     = pmsa( ipnt(16) )
            maxrespup  = pmsa( ipnt(17) )
            factrespup = pmsa( ipnt(18) )

            isw_zf     = nint(pmsa( ipnt(19) ))
            im1s1      = pmsa( ipnt(20) )
            zresim1    = pmsa( ipnt(21) )
            vresim1    = pmsa( ipnt(22) )
            tcrrim1    = pmsa( ipnt(23) )
            im2s1      = pmsa( ipnt(24) )
            zresim2    = pmsa( ipnt(25) )
            vresim2    = pmsa( ipnt(26) )
            tcrrim2    = pmsa( ipnt(27) )
            im3s1      = pmsa( ipnt(28) )
            zresim3    = pmsa( ipnt(29) )
            vresim3    = pmsa( ipnt(30) )
            tcrrim3    = pmsa( ipnt(31) )

            dms1       = pmsa( ipnt(32) )
            dms2       = pmsa( ipnt(33) )

      !**********************************************************************************
      !**** Processes connected to the RESUSPENSION S1 fluff layer 
      !**********************************************************************************

      !     Calculation of resuspension probability in S1
            if (tau .eq. -1.0) then
                 press1im1 = 1.0
                 press1im2 = 1.0
                 press1im3 = 1.0
            else
      !         Compare with critical shear stress
                press1im1 = max ( 0.0, (tau/tcrrim1 - 1.0) )
                press1im2 = max ( 0.0, (tau/tcrrim2 - 1.0) )
                press1im3 = max ( 0.0, (tau/tcrrim3 - 1.0) )
            endif

      !     Calculate resuspension

      !     No resuspension when depth below min depth
            if ( depth .lt. mindep) then
               flrim1s1 = 0.0
               flrim2s1 = 0.0
               flrim3s1 = 0.0
               flres1   = 0.0
               flrdms1  = 0.0
            else
      !        Resuspension from S1
               if ( isw_zf .eq. 0 ) then
                  ! Add zero and first order resuspension
                  flrim1s1 = zresim1 + ( vresim1 * im1s1 )
                  flrim2s1 = zresim2 + ( vresim2 * im2s1 )
                  flrim3s1 = zresim3 + ( vresim3 * im3s1 )
               else
                  ! Take the minimum of the first order and second order
                  flrim1s1 = min(zresim1,( vresim1 * im1s1 ))
                  flrim2s1 = min(zresim2,( vresim2 * im2s1 ))
                  flrim3s1 = min(zresim3,( vresim3 * im3s1 ))
               endif

      !        Apply P and limit resuspension to available material in S1
               flrim1s1 = min ( flrim1s1 * press1im1,  max (0.0, im1s1 / delt ) )
               flrim2s1 = min ( flrim2s1 * press1im2,  max (0.0, im2s1 / delt ) )
               flrim3s1 = min ( flrim3s1 * press1im3,  max (0.0, im3s1 / delt ) )
         
               flres1  = flrim1s1 + flrim2s1 + flrim3s1
               tims1   = im1s1+im2s1+im3s1
               if (tims1.gt.0.0) then
                  flrdms1 = flres1*dms1/tims1
               else
                  flrdms1 = 0.0
               endif
            endif

      !**********************************************************************************
      !**** Processes connected to the van Rijn Pick-up
      !**********************************************************************************

      !     Calculate resuspension probability in S2
            if (tau .eq. -1.0) then
               press2 = 1.0
            else
      !        Compare with critical shear stress
               press2 = max ( 0.0, (tau/tcrrs2 - 1.0) )
            endif

      !     Fraction TIM1 in S2

            frim1s2pup = im1s2/rhosand/(thicks2*(1.-pors2))
            frim2s2pup = im2s2/rhosand/(thicks2*(1.-pors2))
            frim3s2pup = im3s2/rhosand/(thicks2*(1.-pors2))
            tims2      = im1s2+im2s2+im3s2
            frtims2pup = tims2/rhosand/(thicks2*(1.-pors2))
            frpoms2pup = max(0.0,dms2-tims2)/rhosand/(thicks2*(1.-pors2))

      !     No resuspension when depth below min depth
            if ( depth .lt. mindep) then
               flrim1s2 = 0.0
               flrim2s2 = 0.0
               flrim3s2 = 0.0
               flres2   = 0.0
               flrdms2  = 0.0
            else

      !        Resuspension

               rhosandkg = rhosand/1000.
               s         = rhosandkg/rhowater
               dster     = grain50*((s-1.)*grav/(kinviscos*kinviscos))**(1./3.)
               rest      = factrespup*rhosandkg*((s-1.)*grav*grain50)**0.5
               rfdms2    = frtims2pup*rest*(dster**0.3)*(press2**1.5)
         
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
               if (tims2.gt.0.0) then
                  flrdms2 = flres2*dms2/tims2
               else
                  flrdms2 = 0.0
               endif

            endif

            fl( 1 + iflux ) = flrim1s2 / depth
            fl( 2 + iflux ) = flrim2s2 / depth
            fl( 3 + iflux ) = flrim3s2 / depth
            fl( 4 + iflux ) = flrim1s1 / depth
            fl( 5 + iflux ) = flrim2s1 / depth
            fl( 6 + iflux ) = flrim3s1 / depth
      
            pmsa (ipnt (34) ) = flrim1s2
            pmsa (ipnt (35) ) = flrim2s2
            pmsa (ipnt (36) ) = flrim3s2
            pmsa (ipnt (37) ) = flres2
            pmsa (ipnt (38) ) = press2
            pmsa (ipnt (39) ) = frtims2pup
            pmsa (ipnt (40) ) = frpoms2pup
            pmsa (ipnt (41) ) = flrim1s1
            pmsa (ipnt (42) ) = flrim2s1
            pmsa (ipnt (43) ) = flrim3s1
            pmsa (ipnt (44) ) = flrdms1
            pmsa (ipnt (45) ) = flrdms2

          endif
        endif

        iflux = iflux + noflux
        ipnt  = ipnt + increm 
      
      enddo

      end subroutine RESBUF

