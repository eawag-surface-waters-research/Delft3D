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

      subroutine resdm  ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
      use m_dhkmrk

!>\file
!>       Resuspension total bottom material (dry mass)

!
!     Description of the module :
!
! Name    T   L I/O   Description                                    Units
! ----    --- -  -    -------------------                            -----
! DM1     R*4 1 I  amount dry matter in layer S1                     [gDM/m2]
! DM2     R*4 1 I  amount dry matter in layer S2                     [gDM/m2]
! DELT    R*4 1 I  DELWAQ timestep                                   [scu]
! DEPTH   R*4 1 I  depth water column                                  [m]
! FLRES1  R*4 1 O  resuspension flux DM from layer S1           [gDM/m2/d]
! FLRES2  R*4 1 O  resuspension flux DM from layer S2           [gDM/m2/d]
! IAUSYS  R*4 1 I  ratio between auxiliary and system clock unit       [-]
! MRDMS1  R*4 1 L  max. res. flux (contents of layer S1)        [gDM/m2/d]
! MRDMS2  R*4 1 L  max. res. flux (contents of layer S2)        [gDM/m2/d]
! MINDEP  R*4 1 I  minimal depth for resuspension                      [m]
! PRESS1  R*4 1 L  resuspension probability from S1 (0 - endless)      [-]
! PRESS2  R*4 1 L  resuspension probability from S2 (0 - endless)      [-]
! POTRES  R*4 1 L  potential resuspension flux                  [gDM/m2/d]
! FLRES1  R*4 1 L  resuspension flux DM from layer S1           [gDM/m2/d]
! FLRES2  R*4 1 L  resuspension flux DM from layer S2           [gDM/m2/d]
! TAU     R*4 1 I  calculated sheerstress                        [kg/m/s2]
! TAUVEL  R*4 1 I  total velocity calcualted from tau                [m/s]
! TCRRS1  R*4 1 I  critical sheerstress resuspension S1          [kg/m/s2]
! TCRRS2  R*4 1 I  critical sheerstress resuspension S2          [kg/m/s2]
! VCRRS1  R*4 1 I  critical velocity resuspension S1                 [m/s]
! VCRRS2  R*4 1 I  critical velocity resuspension S2                 [m/s]
! VRES    R*4 1 I  first order resuspensionrate constant             [1/d]
! VOLUME  R*4 1 I  volume computed by DELWAQ                          [m3]
! ZRES    R*4 1 I  zeroth order resuspension flux               [gDM/m2/d]

!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------

      IMPLICIT NONE

      real(4) pmsa(*)     !i/o process manager system array, window of routine to process library
      real(4) fl(*)       ! o  array of fluxes made by this process in mass/volume/time
      integer ipoint( 16) ! i  array of pointers in pmsa to get and store the data
      integer increm( 16) ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! i  number of computational elements in the whole model schematisation
      integer noflux      ! i  number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! i  active-inactive, surface-water-bottom, see manual for use
      integer noq1        ! i  nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! i  nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! i  nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt( 16)   !    local work array for the pointering
      integer iseg        !    local loop counter for computational element loop

      integer iflux
      integer ikmrk2
      real(4) dms1
      real(4) dms2
      real(4) zres
      real(4) vres
      real(4) tau
      real(4) tcrrs1
      real(4) tcrrs2
      real(4) depth
      real(4) delt
      real(4) mindep
      real(4) surf
      integer isw_zf
      real(4) press1
      real(4) press2
      real(4) flres1
      real(4) flres2
      real(4) rfdms1
      real(4) mrdms1
      real(4) delts2
      real(4) rfdms2
      real(4) mrdms2

      ipnt  = ipoint

      iflux = 0
      do 9000 iseg = 1 , noseg
      if (btest(iknmrk(iseg),0)) then
      call dhkmrk(2,iknmrk(iseg),ikmrk2)
      if ((ikmrk2.eq.0).or.(ikmrk2.eq.3)) then

      dms1    = pmsa(ipnt (1 ) )
      dms2    = pmsa(ipnt (2 ) )
      zres    = pmsa(ipnt (3 ) )
      vres    = pmsa(ipnt (4 ) )
      tau     = pmsa(ipnt (5 ) )
      tcrrs1  = pmsa(ipnt (6 ) )
      tcrrs2  = pmsa(ipnt (7 ) )
      depth   = pmsa(ipnt (8 ) )
      delt    = pmsa(ipnt (9 ) )
      mindep  = pmsa(ipnt (10) )
      surf    = pmsa(ipnt (11) )
      isw_zf  = nint(pmsa(ipnt (12) ) )

!***********************************************************************
!**** Processes connected to the RESUSENSION
!***********************************************************************

      press1 = 0.0
      press2 = 0.0

!     Calculate resuspension probability in S1
      if (tau .eq. -1.0) then
           press1 = 1.0
      else
!         Compare with critical shear stress
          press1 = max ( 0.0, (tau/tcrrs1 - 1.0) )
      endif

!     Calculate resuspension probability in S2
      if (tau .eq. -1.0) then
         press2 = 1.0
      else
!        Compare with critical shear stress
         press2 = max ( 0.0, (tau/tcrrs2 - 1.0) )
      endif

!     No resuspension when depth below min depth
      if ( depth .lt. mindep) then
         flres1 = 0.0
         flres2 = 0.0
      else
!        Resuspension from S1
         if ( isw_zf .eq. 0 ) then
            ! add zero and first order resuspension
            rfdms1 = zres + ( vres * dms1 )
         else
            ! take the minimum of the first order and second order
            rfdms1 = min(zres,( vres * dms1 ))
         endif

!        Limit resuspension to available material in S1
         mrdms1 = max (0.0, dms1 / delt )
         flres1 = min ( rfdms1 * press1,  mrdms1 )

!        If first layer is exhausted then resuspension from the second layer for the remaining of the timestep (DELTS2)
         if ( rfdms1*press1 .gt. 1e-20 ) then
            delts2 = max(0.0,(1.-flres1/(rfdms1*press1))*delt)
         else
            delts2 = 0.0
         endif

         if ( isw_zf .eq. 0 ) then
            rfdms2 = zres + ( vres * dms2 )
         else
            rfdms2 = min(zres,( vres * dms2 ))
         endif

!        Limit resuspension to available material in S2
         mrdms2 = max (0.0, dms2 / delt )
         flres2 = min ( rfdms2 * press2 * delts2/delt , mrdms2 )
      endif

      pmsa (ipnt (13) ) = flres1
      pmsa (ipnt (14) ) = flres2
      pmsa (ipnt (15) ) = press1
      pmsa (ipnt (16) ) = press2

      endif
      endif
!
      iflux = iflux + noflux
      ipnt  = ipnt  + increm

 9000 continue
!
      return
!
      end
