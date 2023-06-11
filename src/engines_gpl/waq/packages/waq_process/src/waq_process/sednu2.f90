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

      subroutine sednu2 ( pmsa   , fl     , ipoint , increm , noseg  , &
                          noflux , iexpnt , iknmrk , noq1   , noq2   , &
                          noq3   , noq4   )
      use m_dhkmrk



! This version is used for developing the interaction of POX-2 with the sediment buffer model
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint(12)  ! I  Array of pointers in pmsa to get and store the data
      integer increm(12)  ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
!
!*******************************************************************************
!>\file
!>       Sedimentation of nutrients in the organic carbon matrix (GEM)
!
!     Description of the module :
!
! Name    T   L I/O   Description                                    Units
!
!     Logical Units : -

      integer     ipnt(12)
      real        SFL  !   R*4 1 I  sedimention flux organic                      [gX/m2/d]
      real        sfl2
      real        CN   !   R*4 1 I  CN ratio substance                             [gC/gN]
      real        CP   !   R*4 1 I  CP ratio substance                             [gC/gP]
      real        CS   !   R*4 1 I  CS ratio substance                             [gC/gS]
      real        depth
      real        FSEDDN
      real        FSEDDP
      real        FSEDDS
      real        FSEDDN2
      real        FSEDDP2
      real        FSEDDS2
      
      integer    iflux, iseg, ikmrk2

      ipnt = ipoint
!
      iflux = 0
      do iseg = 1 , noseg

          if (btest(iknmrk(iseg),0)) then
          call dhkmrk(2,iknmrk(iseg),ikmrk2)
          if ((ikmrk2.eq.0).or.(ikmrk2.eq.3)) then
    !
          sfl    = pmsa(ipnt(1) )
          sfl2   = pmsa(ipnt(2) )
          cn     = pmsa(ipnt(3) )
          cp     = pmsa(ipnt(4) )
          cs     = pmsa(ipnt(5) )
          depth  = pmsa(ipnt(6) )

    !*******************************************************************************
    !**** Processes connected to the SEDIMENTATION of nutrients in C-Matrix
    !***********************************************************************

    !     Initialisation
          FSEDDN = 0.0
          FSEDDP = 0.0
          FSEDDS = 0.0
          FSEDDN2 = 0.0
          FSEDDP2 = 0.0
          FSEDDS2 = 0.0

    !     Sedimentation

          if ( cn .gt. 0.1 ) FSEDDN = sfl / cn
          if ( cp .gt. 0.1 ) FSEDDP = sfl / cp
          if ( cs .gt. 0.1 ) FSEDDS = sfl / cs
          if ( cn .gt. 0.1 ) FSEDDN2 = sfl2 / cn
          if ( cp .gt. 0.1 ) FSEDDP2 = sfl2 / cp
          if ( cs .gt. 0.1 ) FSEDDS2 = sfl2 / cs
    !
          pmsa ( ipnt(7) ) = FSEDDN
          pmsa ( ipnt(8) ) = FSEDDP
          pmsa ( ipnt(9) ) = FSEDDS
          pmsa ( ipnt(10) ) = FSEDDN2
          pmsa ( ipnt(11) ) = FSEDDP2
          PMSA ( IPNT(12) ) = FSEDDS2
    !
          if ( depth .gt. 0.0 ) then
              fl( 1 + iflux ) =  FSEDDN /depth
              fl( 2 + iflux ) =  FSEDDP /depth
              fl( 3 + iflux ) =  FSEDDS /depth
              fl( 4 + iflux ) =  FSEDDN2 /depth
              fl( 5 + iflux ) =  FSEDDP2 /depth
              fl( 6 + iflux ) =  FSEDDS2 /depth
          else
              fl( 1 + iflux ) =  0.0
              fl( 2 + iflux ) =  0.0
              fl( 3 + iflux ) =  0.0
              fl( 4 + iflux ) =  0.0
              fl( 5 + iflux ) =  0.0
              fl( 6 + iflux ) =  0.0
          endif
    !
          endif
          endif
    !
          iflux = iflux + noflux
          ipnt = ipnt + increm
    !
          end do

!
      return
!
      end
