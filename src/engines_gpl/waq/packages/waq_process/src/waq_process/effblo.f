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
      module m_effblo
      use m_get_effi


      implicit none

      contains


      subroutine effblo     ( pmsa   , fl     , ipoint , increm, noseg ,
     &                        noflux , iexpnt , iknmrk , noq1  , noq2  ,
     &                        noq3   , noq4   )
!>\file
!>       Light efficiency function BLOOM algae
!>
!>              Routine is called for all algae species with species specific coefficients

      implicit none

!     Type    Name         I/O Description

      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint( 37) ! I  Array of pointers in pmsa to get and store the data
      integer increm( 37) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt( 37)   !    Local work array for the pointering
      integer iseg        !    Local loop counter for computational element loop

!     arguments

      integer  sweff      ! input , Switch to use classic(1) or direct(2) BLOOM Efficiency calculation
      real     temper     ! input , temperature
      real     radiat     ! input , radiation
      real     ext        ! input , total extinction
      real     depthw     ! input , depth of the layer
      real     daylen     ! input , daylength in hours

!     local decalarations

      integer  nspe       ! number of bloom algae species
      integer  ispe       ! index number of bloom algae species
      real     effi(30)   ! efficiencies per species group

      ipnt        = ipoint

!     we might not have the bloom parameters loaded yet. This could already be done here or in BLOOM? Only the first time step.
!     this is in a module/include, so we might put a flag if it was read of not.
!     this should be a 'proto-proces', and thus needs to be added to the BLOOM.SPE
      do 9000 iseg = 1 , noseg

         if ( btest(iknmrk(iseg),0) ) then

            SWEff  = nint(pmsa( ipnt(  1) ))
            Temper = pmsa( ipnt(  2) )
            Radiat = pmsa( ipnt(  3) ) * 60.48  ! Conversion from W/m2 to J/cm2/7days
            Ext    = pmsa( ipnt(  4) )
            Depthw = pmsa( ipnt(  5) )
            DayLen = pmsa( ipnt(  6) )  * 24.   ! Conversion from days to hours
!     test for extinction and depth to prevent diff by zero!!
            effi = 0.0e0
            if(ext.gt.0.0e0 .and. depthw.gt.0.0e0) then
               call get_effi( SWEff, temper, radiat, ext   , depthw, daylen, nspe  , effi )
            endif
            do ispe = 1, nspe
               pmsa( ipnt(7+ ispe)) = effi(ispe)
            enddo
         endif

         ipnt        = ipnt        + increm

 9000 continue

      return
      end subroutine

      end module m_effblo
