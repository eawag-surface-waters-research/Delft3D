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

      subroutine VBUPT      ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
      use m_errsys

!
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint( 25) ! I  Array of pointers in pmsa to get and store the data
      integer increm( 25) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt( 25)   !    Local work array for the pointering
      integer iseg        !    Local loop counter for computational element loop
!
!*******************************************************************************
!
!     Type    Name         I/O Description                                        Unit
!
      real(4) fVB         ! I  area scaled flux dVB01                             (gC/m2/d)
      real(4) SwVBGro     ! I  vegetation biomass growth allowed (0=no,1=yes)     (-)
      real(4) F1VB        ! I  allocation factor comp. 1 (stem) VB01              (-)
      real(4) F2VB        ! I  allocation factor comp. 2 (foliage) VB01           (-)
      real(4) F3VB        ! I  allocation factor comp. 3 (branch) VB01            (-)
      real(4) F4VB        ! I  allocation factor comp. 4 (root) VB01              (-)
      real(4) F5VB        ! I  allocation factor comp. 5 (fineroot) VB01          (-)
      real(4) CNf1VB      ! I  carbon-nitrogen ratio in stem VB01                 (gC/gN)
      real(4) CNf2VB      ! I  carbon-nitrogen ratio in foliage VB01              (gC/gN)
      real(4) CNf3VB      ! I  carbon-nitrogen ratio in branch VB01               (gC/gN)
      real(4) CNf4VB      ! I  carbon-nitrogen ratio in root VB01                 (gC/gN)
      real(4) CNf5VB      ! I  carbon-nitrogen ratio in fineroot VB01             (gC/gN)
      real(4) CPf1VB      ! I  carbon-phosporus ratio in stem VB01                (gC/gP)
      real(4) CPf2VB      ! I  carbon-phosporus ratio in foliage VB01             (gC/gP)
      real(4) CPf3VB      ! I  carbon-phosporus ratio in branch VB01              (gC/gP)
      real(4) CPf4VB      ! I  carbon-phosporus ratio in root VB01                (gC/gP)
      real(4) CPf5VB      ! I  carbon-phosporus ratio in fineroot VB01            (gC/gP)
      real(4) CSf1VB      ! I  carbon-sulphur ratio in stem VB01                  (gC/gS)
      real(4) CSf2VB      ! I  carbon-sulphur ratio in foliage VB01               (gC/gS)
      real(4) CSf3VB      ! I  carbon-sulphur ratio in branch VB01                (gC/gS)
      real(4) CSf4VB      ! I  carbon-sulphur ratio in root VB01                  (gC/gS)
      real(4) CSf5VB      ! I  carbon-sulphur ratio in fineroot                   (gC/gS)

      real(4) fNVB01up    ! O  uptake roots VB01                                  (gN/m2/d)
      real(4) fPVB01up    ! O  uptake roots VB01                                  (gP/m2/d)
      real(4) fSVB01up    ! O  uptake roots VB01                                  (gS/m2/d)
      real(4) weighCN
      real(4) weighCP
      real(4) weighCS
!
!*******************************************************************************
!
      ipnt        = ipoint
!
      do 9000 iseg = 1 , noseg
!
         fVB      = pmsa( ipnt(  1) )
         SwVBGro  = pmsa( ipnt(  2) )
         F1VB     = pmsa( ipnt(  3) )
         F2VB     = pmsa( ipnt(  4) )
         F3VB     = pmsa( ipnt(  5) )
         F4VB     = pmsa( ipnt(  6) )
         F5VB     = pmsa( ipnt(  7) )
         CNf1VB   = pmsa( ipnt(  8) )
         CNf2VB   = pmsa( ipnt(  9) )
         CNf3VB   = pmsa( ipnt( 10) )
         CNf4VB   = pmsa( ipnt( 11) )
         CNf5VB   = pmsa( ipnt( 12) )
         CPf1VB   = pmsa( ipnt( 13) )
         CPf2VB   = pmsa( ipnt( 14) )
         CPf3VB   = pmsa( ipnt( 15) )
         CPf4VB   = pmsa( ipnt( 16) )
         CPf5VB   = pmsa( ipnt( 17) )
         CSf1VB   = pmsa( ipnt( 18) )
         CSf2VB   = pmsa( ipnt( 19) )
         CSf3VB   = pmsa( ipnt( 20) )
         CSf4VB   = pmsa( ipnt( 21) )
         CSf5VB   = pmsa( ipnt( 22) )

!
!   *****     Insert your code here  *****
!
!
         fNVB01up  = 0.0
         fPVB01up  = 0.0
         fSVB01up  = 0.0

         if ( Nint(SwVBGro) .eq. 1) then

!            make sure allocation factors for roots > 0

             if ( (F4VB + F5VB) - 1.E-10 .lt. 0.0 ) then
                CALL ERRSYS ('(no valid values for F4VB and F5VB (alloction factors vegetation  roots)', 1 )
             else
!               average Nutrient content of cohort
                weighCN = F1VB*CNf1VB + F2VB*CNf2VB + F3VB*CNf3VB + F4VB*CNf4VB + F5VB*CNf5VB
                weighCP = F1VB*CPf1VB + F2VB*CPf2VB + F3VB*CPf3VB + F4VB*CPf4VB + F5VB*CPf5VB
                weighCS = F1VB*CSf1VB + F2VB*CSf2VB + F3VB*CSf3VB + F4VB*CSf4VB + F5VB*CSf5VB

!               calculate 2D nutrient uptake flux
                fNVB01up  = fVB / weighCN
                fPVB01up  = fVB / weighCP
                fSVB01up  = fVB / weighCS
             endif

!        evaluate SwVBGro
         endif
!
!   *****     End of your code       *****
!
         pmsa( ipnt( 23)   ) = fNVB01up
         pmsa( ipnt( 24)   ) = fPVB01up
         pmsa( ipnt( 25)   ) = fSVB01up
!
         ipnt        = ipnt        + increm
!
 9000 continue
!
      return
      end
