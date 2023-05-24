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

      subroutine npps12     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
      use m_dhkmrk

!
!*******************************************************************************
!
      implicit none
!
!     type    name         i/o description
!
      real(4) pmsa(*)     !i/o process manager system array, window of routine to process library
      real(4) fl(*)       ! o  array of fluxes made by this process in mass/volume/time
      integer ipoint( 15) ! i  array of pointers in pmsa to get and store the data
      integer increm( 15) ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! i  number of computational elements in the whole model schematisation
      integer noflux      ! i  number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! i  active-inactive, surface-water-bottom, see manual for use
      integer noq1        ! i  nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! i  nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! i  nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt( 15)   !    local work array for the pointering
      integer iseg        !    local loop counter for computational element loop
!
!*******************************************************************************
!
!
!     type    name         i/o description                                        unit
!
      real(4) nh4         ! i  depth of segment                                   (g/m3)
      real(4) po4         ! i  total depth water column                           (g/m3)
      real(4) dmn1s1      ! i  first mineralisation flux N in layer 1             (g/m3/d)
      real(4) dmn2s1      ! i  secnd mineralisation flux N in layer 1             (g/m3/d)
      real(4) dmn1s2      ! i  first mineralisation flux N in layer 2             (g/m3/d)
      real(4) dmn2s2      ! i  secnd mineralisation flux N in layer 2             (g/m3/d)
      real(4) dmp1s1      ! i  first mineralisation flux P in layer 1             (g/m3/d)
      real(4) dmp2s1      ! i  secnd mineralisation flux P in layer 1             (g/m3/d)
      real(4) dmp1s2      ! i  first mineralisation flux P in layer 2             (g/m3/d)
      real(4) dmp2s2      ! i  secnd mineralisation flux P in layer 2             (g/m3/d)
      real(4) dlen        ! i  diffusion length                                   (m)
      real(4) dcoef       ! i  diffusion coefficient                              (m2/d)
      real(4) depth       ! i  depth                                              (m)

      real(4) nh4s12      ! o  estimated NH4 in pore water                        (m)
      real(4) po4s12      ! o  estimated PO4 in pore water                        (m)

      ! local

      integer ikmrk1      !    first attribute
      integer ikmrk2      !    second attribute
      real(4) nflux       !    total flux
      real(4) pflux       !    total flux

      ipnt        = ipoint

      do 9000 iseg = 1 , noseg
         call dhkmrk(1,iknmrk(iseg),ikmrk1)
         if (ikmrk1.eq.1) then
         call dhkmrk(2,iknmrk(iseg),ikmrk2)
         if (ikmrk2.eq.0.or.ikmrk2.eq.3) then

         nh4        = max(pmsa( ipnt( 1) ),0.0)
         po4        = max(pmsa( ipnt( 2) ),0.0)
         dmn1s1     = pmsa( ipnt( 3) )
         dmn2s1     = pmsa( ipnt( 4) )
         dmn1s2     = pmsa( ipnt( 5) )
         dmn2s2     = pmsa( ipnt( 6) )
         dmp1s1     = pmsa( ipnt( 7) )
         dmp2s1     = pmsa( ipnt( 8) )
         dmp1s2     = pmsa( ipnt( 9) )
         dmp2s2     = pmsa( ipnt(10) )
         dlen       = pmsa( ipnt(11) )
         dcoef      = pmsa( ipnt(12) )
         depth      = pmsa( ipnt(13) )

         nflux = (dmn1s1+dmn2s1+dmn1s2+dmn2s2)*depth   ! g/m2/d
         pflux = (dmp1s1+dmp2s1+dmp1s2+dmp2s2)*depth
         nh4s12 = nflux*dlen/dcoef + nh4
         po4s12 = pflux*dlen/dcoef + po4

         pmsa( ipnt(14) ) = nh4s12
         pmsa( ipnt(15) ) = po4s12

         endif
         endif
         ipnt        = ipnt        + increm

 9000 continue

      return
      end subroutine
