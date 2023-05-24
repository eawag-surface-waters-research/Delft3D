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

      subroutine mac3du     ( pmsa   , fl     , ipoint , increm, noseg , &
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
      integer ipoint( 29) ! i  array of pointers in pmsa to get and store the data
      integer increm( 29) ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! i  number of computational elements in the whole model schematisation
      integer noflux      ! i  number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! i  active-inactive, surface-water-bottom, see manual for use
      integer noq1        ! i  nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! i  nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! i  nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt( 29)   !    local work array for the pointering
      integer iseg        !    local loop counter for computational element loop
!
!*******************************************************************************
!

!     type    name         i/o description                                        unit
!
      real(4) depth       ! i  depth of segment                                   (m)
      real(4) totaldepth  ! i  total depth water column                           (m)
      real(4) nh4s12      ! i  concentration of NH4 in S12 bottom (pores)         (g/m3)
      real(4) locseddept  ! i  sediment layer depth to bottom of segment          (m)
      integer ibotseg     ! i  bottom segment number                              (-)
      real(4) FrBmLay     ! i  actual height sm                                   (m)
      real(4) rootdesm01  ! i  rooting depth sm01                                 (m)
      real(4) poros       ! i  volumetric porosity                                (-)
      real(4) po4s12      ! i  concentration of PO4 in S12 bottom (pores)         (g/m3)
      real(4) nh4         ! i  ammonium (nh4)                                     (gn/m3)
      real(4) no3         ! i  nitrate (no3)                                      (gn/m3)
      real(4) po4         ! i  ortho-phosphate (po4)                              (gp/m3)
      real(4) disco2      ! i  concentration of dissolved carbon dioxide          (g/m3)
      real(4) dish2co3    ! i  concentration of dissolved true h2co3              (gc/m3)
      real(4) dishco3     ! i  concentration of dissolved hco3(-)                 (gc/m3)
      real(4) prfnh4sm01  ! i  ammonium preferency over nitrate sm01              (-)
      real(4) nh4crsm01   ! i  critical nh4 concentration for uptake sm01         (gn/m3)
      real(4) frootnsm01  ! i  fraction root uptake nitrogen sm01                 (-)
      real(4) frootpsm01  ! i  fraction root uptake phosphorus sm01               (-)
      real(4) cdinsm01w   ! i  average water concentration din for sm01           (m)
      real(4) cpo4sm01w   ! i  average water concentration po4 for sm01           (m)
      real(4) cco2sm01    ! i  average concentration co2+h2co3 for sm01           (m)
      real(4) chco3sm01   ! i  average concentration hco3 for sm01                (m)
      real(4) cdinsm01b   ! i  average sediment concentration din for sm01        (m)
      real(4) cpo4sm01b   ! i  average sediment concentration po4 for sm01        (m)
      real(4) dnupsm01    ! i  uptake flux nitrogen sm01                          (gn/m3/d)
      real(4) dpupsm01    ! i  uptake flux phosphorus sm01                        (gp/m3/d)
      real(4) dsm01oxy    ! i  oxygen production sm01                             (go/m3/d)
      real(4) dsm01co2    ! i  co2 uptake sm01                                    (go/m3/d)
      real(4) dnh4upsm01  ! f  nh4 uptake by sm01                                 (gn/m3/d)
      real(4) dno3upsm01  ! f  no3 uptake by sm01                                 (gn/m3/d)
      real(4) dpo4upsm01  ! f  po4 uptake by sm01                                 (gp/m3/d)
      real(4) dco2upsm01  ! f  co2 uptake by sm01                                 (gc/m3/d)
      real(4) doxyprsm01  ! f  oxy production by sm01                             (go/m3/d)
      integer idnh4upsm01 !    pointer to the nh4 uptake by sm01
      integer idno3upsm01 !    pointer to the no3 uptake by sm01
      integer idpo4upsm01 !    pointer to the po4 uptake by sm01
      integer idco2upsm01 !    pointer to the co2 uptake by sm01
      integer idoxyprsm01 !    pointer to the oxy production by sm01

      ! local

      integer ikmrk1      !    first attribute
      integer ikmrk2      !    second attribute
      real(4) din         ! l  dissolved inorganic nitrogen, corrected for preference
      real(4) z1          ! l  z1
      real(4) fr_avg      ! l  fr_avg
      real(4) fr_nh4      ! l  fr_nh4
      real(4) fr_no3      ! l  fr_no3
      real(4) hroot       ! l  hroot
      real(4) bdepth      ! l  depth of bottom segment

      ipnt        = ipoint
      idnh4upsm01 = 1
      idno3upsm01 = 2
      idpo4upsm01 = 3
      idco2upsm01 = 4
      idoxyprsm01 = 5

      do 9000 iseg = 1 , noseg

         depth      = pmsa( ipnt(  1) )
         totaldepth = pmsa( ipnt(  2) )
         nh4s12     = pmsa( ipnt(  3) )
         locseddept = pmsa( ipnt(  4) )
         ibotseg    = pmsa( ipnt(  5) )
         FrBmLay    = pmsa( ipnt(  6) )
         rootdesm01 = pmsa( ipnt(  7) )
         poros      = pmsa( ipnt(  8) )
         po4s12     = pmsa( ipnt(  9) )
         nh4        = max(pmsa( ipnt( 10) ),0.0)
         no3        = max(pmsa( ipnt( 11) ),0.0)
         po4        = max(pmsa( ipnt( 12) ),0.0)
         disco2     = max(pmsa( ipnt( 13) ),0.0)
         dish2co3   = max(pmsa( ipnt( 14) ),0.0)
         dishco3    = max(pmsa( ipnt( 15) ),0.0)
         prfnh4sm01 = pmsa( ipnt( 16) )
         nh4crsm01  = pmsa( ipnt( 17) )

         ! convert co2 to carbon and add h2co3, calculated din with preference
         ! adjust for porosity

         din        = (nh4 + no3/prfnh4sm01)/poros
         if ( nh4 + no3/prfnh4sm01 .gt. 1e-20 ) then
            fr_nh4     = nh4/(nh4 + no3/prfnh4sm01)
            fr_no3     = no3/prfnh4sm01/(nh4 + no3/prfnh4sm01)
         else
            fr_nh4     = 1.0
            fr_no3     = 0.0
         endif
         po4        =  po4/poros
         disco2     = (disco2*12./44. + dish2co3)/poros
         dishco3    =  dishco3/poros

         ! results of previous modeules are defined in the ibotseg were the plant resides

         frootnsm01 = pmsa(ipoint(18)+(ibotseg-1)*increm(18))
         frootpsm01 = pmsa(ipoint(19)+(ibotseg-1)*increm(19))
         cdinsm01w  = pmsa(ipoint(20)+(ibotseg-1)*increm(20))
         cpo4sm01w  = pmsa(ipoint(21)+(ibotseg-1)*increm(21))
         cco2sm01   = pmsa(ipoint(22)+(ibotseg-1)*increm(22))
         chco3sm01  = pmsa(ipoint(23)+(ibotseg-1)*increm(23))
         cdinsm01b  = pmsa(ipoint(24)+(ibotseg-1)*increm(24))
         cpo4sm01b  = pmsa(ipoint(25)+(ibotseg-1)*increm(25))
         dnupsm01   = pmsa(ipoint(26)+(ibotseg-1)*increm(26))
         dpupsm01   = pmsa(ipoint(27)+(ibotseg-1)*increm(27))
         dsm01oxy   = pmsa(ipoint(28)+(ibotseg-1)*increm(28))
         dsm01co2   = pmsa(ipoint(29)+(ibotseg-1)*increm(29))

         ! scale input fluxes to g/m2/d

         bdepth     = pmsa(ipoint( 1)+(ibotseg-1)*increm( 1))
         dnupsm01   = dnupsm01 * bdepth
         dpupsm01   = dpupsm01 * bdepth
         dsm01oxy   = dsm01oxy * bdepth
         dsm01co2   = dsm01co2 * bdepth

         ! zero output fluxes

         dnh4upsm01 = 0.0
         dno3upsm01 = 0.0
         dpo4upsm01 = 0.0
         dco2upsm01 = 0.0
         doxyprsm01 = 0.0

         call dhkmrk(1,iknmrk(iseg),ikmrk1)
         if (ikmrk1.eq.1) then

            ! active water segment

            fr_avg = FrBmLay

            ! nutrient uptake according to conc gradient, preference, depth and fraction root uptake

            if ( cdinsm01w .gt. 1e-20 ) then
               dnh4upsm01 = (din/cdinsm01w) * fr_nh4 * fr_avg * (1.-frootnsm01) * dnupsm01
               dno3upsm01 = (din/cdinsm01w) * fr_no3 * fr_avg * (1.-frootnsm01) * dnupsm01
            endif
            if ( cpo4sm01w .gt. 1e-20 ) then
               dpo4upsm01 = (po4/cpo4sm01w) * fr_avg * (1.-frootpsm01) * dpupsm01
            endif


            ! we do not know if the macropythe takes disco2 or dishco3
            ! co2 uptake base on total disco2 and dishco3 profile, assuming the speciation in the column is not important
            ! the flux is on tic anyhow

            if ( cco2sm01+chco3sm01 .gt. 1.e-20 ) then
               dco2upsm01 = ((disco2+dishco3)/(cco2sm01+chco3sm01)) * fr_avg * dsm01co2
            endif

            ! oxygen flux according to fr_avg

            doxyprsm01 = fr_avg * dsm01oxy

            ! S12 sediment uptake

            call dhkmrk(2,iknmrk(iseg),ikmrk2)
            if ((ikmrk2.eq.0).or.(ikmrk2.eq.3)) then
              if (nh4s12.gt.0.0) dnh4upsm01 = dnh4upsm01 + frootnsm01 * dnupsm01
              if (po4s12.gt.0.0) dpo4upsm01 = dpo4upsm01 + frootpsm01 * dpupsm01
            endif

         elseif (ikmrk1.eq.3) then

            ! sediment bed segment, distribution of roots in bed

            hroot = min(rootdesm01,totaldepth)
            z1 = locseddept - depth

            if (hroot .gt. locseddept) then
               ! completely in segment:
               fr_avg = min(1.0,depth/hroot)
            elseif (hroot .gt. z1 ) then
               ! partialy in segment:
               fr_avg = (hroot-z1)/hroot
            else
               ! not in segment:
               fr_avg = 0.0
            endif

            ! in bottom only nutrients

            if ( cdinsm01b .gt. 1e-20 ) then
               dnh4upsm01 = (din/cdinsm01b) * fr_nh4 * fr_avg * frootnsm01 * dnupsm01
               dno3upsm01 = (din/cdinsm01b) * fr_no3 * fr_avg * frootnsm01 * dnupsm01
            endif
            if ( cpo4sm01b .gt. 1e-20 ) then
               dpo4upsm01 = (po4/cpo4sm01b) * fr_avg * frootpsm01 * dpupsm01
            endif

         endif

         ! fluxes, scale back to g/m3/d

         if ( depth > 0.0 ) then
            fl  ( idnh4upsm01 ) = dnh4upsm01 / depth
            fl  ( idno3upsm01 ) = dno3upsm01 / depth
            fl  ( idpo4upsm01 ) = dpo4upsm01 / depth
            fl  ( idco2upsm01 ) = dco2upsm01 / depth
            fl  ( idoxyprsm01 ) = doxyprsm01 / depth
         endif

         idnh4upsm01 = idnh4upsm01 + noflux
         idno3upsm01 = idno3upsm01 + noflux
         idpo4upsm01 = idpo4upsm01 + noflux
         idco2upsm01 = idco2upsm01 + noflux
         idoxyprsm01 = idoxyprsm01 + noflux
         ipnt        = ipnt        + increm

 9000 continue

      return
      end subroutine
