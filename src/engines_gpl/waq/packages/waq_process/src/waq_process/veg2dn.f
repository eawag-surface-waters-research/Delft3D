!!  Copyright (C)  Stichting Deltares, 2012-2021.
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

      subroutine veg2dn     ( pmsa   , fl     , ipoint , increm, noseg ,
     +                        noflux , iexpnt , iknmrk , noq1  , noq2  ,
     +                        noq3   , noq4   )

      ! function determine nutrient availability for vegetation

      implicit none

      ! arguments          i/o description

      real(4) pmsa(*)     !i/o process manager system array, window of routine to process library
      real(4) fl(*)       ! o  array of fluxes made by this process in mass/volume/time
      integer ipoint(*)   ! i  array of pointers in pmsa to get and store the data
      integer increm(*)   ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! i  number of computational elements in the whole model schematisation
      integer noflux      ! i  number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! i  active-inactive, surface-water-bottom, see manual for use
      integer noq1        ! i  nr of exchanges in 1st direction, only horizontal dir if irregular mesh
      integer noq2        ! i  nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! i  nr of exchanges in the bottom (bottom layers, specialist use only)

      ! from pmsa array

      real(4) depth       ! i  depth of segment                               (m)
      real(4) totaldepth  ! i  total depth water column                       (m)
      real(4) localdepth  ! i  depth from water surface to bottom of segment  (m)
      real(4) volume      ! i  volume                                        (m3)
      real(4) surf        ! i  surf                                          (m2)
      real(4) hmax        ! i  maxmimum length roots                          (m)
      real(4) nh4         ! i  nh4                                         (g/m3)
      real(4) aap         ! i  aap                                         (g/m3)
      real(4) so4         ! i  so4                                         (g/m3)
      real(4) no3         ! i  no3                                         (g/m3)
      real(4) po4         ! i  po4                                         (g/m3)
      real(4) sud         ! i  sud                                         (g/m3)
      real(4) SWRoot      ! I  RootShootModel(y=1,n=0) for F2VB F4VB       (-)
      real(4) Vmax        ! I  maximun rate in Michelis/Menten             (-)
      real(4) Km          ! I  TIN conc. at half of Vmax                  (gN/m3)
      real(4) Vini        ! I  initial rate in Michelis/Menten             (-)
      real(4) Poros       ! I  Porosity                                    (-)

      real(4) vbxxnavail  ! o  available nitrogen                          (g/m2)
      real(4) vbxxpavail  ! o  available p                                 (g/m2)
      real(4) vbxxsavail  ! o  available s                                 (g/m2)
      real(4) porevol     ! o  pore water volume                           (m3)
      real(4) F1VB        ! o  allocation factor comp. 1 (stem)             (-)
      real(4) F2VB        ! o  allocation factor comp. 2 (foliage)          (-)
      real(4) F3VB        ! o  allocation factor comp. 3 (branch)           (-)
      real(4) F4VB        ! o  allocation factor comp. 4 (root)             (-)
      real(4) F5VB        ! o  allocation factor comp. 5 (fine root)        (-)

      ! local declarations

      integer iseg        !    local loop counter for computational element loop
      real(4) z2          !    height bottom segment from bottom              (m)
      real(4) z1          !    height top segment from bottom                 (m)
      integer ikmrk1
      integer ikmrk2
      real(4) zm          !    watersurface to top macropyte                  (-)
      real(4) frlay       !    fraction witin layer                           (-)
      integer iq          !    loop counter
      integer ifrom       !    from segment
      integer ito         !    from segment
      integer iflux       !    index in the fl array

      integer, parameter           :: npnt = 27           ! number of pointers
      integer                      :: ipnt(npnt)          ! local work array for the pointering
      integer                      :: ibotseg             ! bottom segment for macrophyte

      real(4) TIN         !    nh4+no2 conc.                                 (g/m3)
      real(4) porewater   !    pore water volume                            (m3)
      ! zero the pool for all segments

      ipnt  = ipoint(1:npnt)
      do iseg = 1 , noseg
         pmsa(ipnt(19)) = 0.0
         pmsa(ipnt(20)) = 0.0
         pmsa(ipnt(21)) = 0.0
         pmsa(ipnt(22)) = 0.0
         pmsa(ipnt(23)) = 0.0
         pmsa(ipnt(24)) = 0.0  ! constant value for no vegetation cells
         pmsa(ipnt(25)) = 0.0
         pmsa(ipnt(26)) = 0.0
         pmsa(ipnt(27)) = 0.0  ! constant value for no vegetation cells
         ipnt  = ipnt  + increm(1:npnt)
      enddo

      ! accumulate mass in the rooting zone in the pool of the bottom segment

      !allocate(poreVol(noseg),stat=ierr_alloc)

      ipnt  = ipoint(1:npnt)
      do iseg = 1 , noseg

         depth       = pmsa(ipnt(1))
         totaldepth  = pmsa(ipnt(2))
         localdepth  = pmsa(ipnt(3))
         volume      = pmsa(ipnt(4))
         surf        = pmsa(ipnt(5))
         ibotseg     = NINT(pmsa(ipnt(6)))
         hmax        = pmsa(ipnt(7))
         nh4         = pmsa(ipnt(8))
         no3         = pmsa(ipnt(9))
         aap         = pmsa(ipnt(10))
         po4         = pmsa(ipnt(11))
         so4         = pmsa(ipnt(12))
         sud         = pmsa(ipnt(13))
         SWRoot      = pmsa(ipnt(14))
         Vmax        = pmsa(ipnt(15))
         Km          = pmsa(ipnt(16))
         Vini        = pmsa(ipnt(17))
         Poros       = pmsa(ipnt(18))

         call dhkmrk(1,iknmrk(iseg),ikmrk1)
         if (ikmrk1.lt.3) then ! also when dry!

            ! active water segment

            if ( hmax .gt. 0.0 ) then

               ! in, partly in or out of the active zone

               hmax = min(hmax,totaldepth)
               zm = totaldepth - hmax
               z1 = localdepth - depth
               z2 = localdepth

               if (zm .gt. z2) then
                  ! not in segment:

               elseif (zm . lt. z1 ) then
                  ! partialy in segment:
                  frlay = (z2-zm)/depth
                  pmsa(ipoint(19)+(ibotseg-1)*increm(19)) = pmsa(ipoint(19)+(ibotseg-1)*increm(19)) + (nh4+no3)*volume*frlay
                  pmsa(ipoint(20)+(ibotseg-1)*increm(20)) = pmsa(ipoint(20)+(ibotseg-1)*increm(20)) + (aap+po4)*volume*frlay
                  pmsa(ipoint(21)+(ibotseg-1)*increm(21)) = pmsa(ipoint(21)+(ibotseg-1)*increm(21)) + (so4+sud)*volume*frlay
                  pmsa(ipoint(22)+(ibotseg-1)*increm(22)) = pmsa(ipoint(22)+(ibotseg-1)*increm(22)) + volume*frlay*Poros
               else
                  ! completely in segment:
                  pmsa(ipoint(19)+(ibotseg-1)*increm(19)) = pmsa(ipoint(19)+(ibotseg-1)*increm(19)) + (nh4+no3)*volume
                  pmsa(ipoint(20)+(ibotseg-1)*increm(20)) = pmsa(ipoint(20)+(ibotseg-1)*increm(20)) + (aap+po4)*volume
                  pmsa(ipoint(21)+(ibotseg-1)*increm(21)) = pmsa(ipoint(21)+(ibotseg-1)*increm(21)) + (so4+sud)*volume
                  pmsa(ipoint(22)+(ibotseg-1)*increm(22)) = pmsa(ipoint(22)+(ibotseg-1)*increm(22)) + volume*Poros
              endif

            endif

         elseif (ikmrk1.eq.3) then

            ! delwaq-g segment

            if ( hmax .lt. 0.0 ) then

               ! distribution over the bottom segments

               hmax = -hmax
               hmax = min(hmax,totaldepth)
               z1 = localdepth - depth

               if (hmax .gt. localdepth) then
                  ! completely in segment:
                  pmsa(ipoint(19)+(ibotseg-1)*increm(19)) = pmsa(ipoint(19)+(ibotseg-1)*increm(19)) + (nh4+no3)*volume
                  pmsa(ipoint(20)+(ibotseg-1)*increm(20)) = pmsa(ipoint(20)+(ibotseg-1)*increm(20)) + (aap+po4)*volume
                  pmsa(ipoint(21)+(ibotseg-1)*increm(21)) = pmsa(ipoint(21)+(ibotseg-1)*increm(21)) + (so4+sud)*volume
                  pmsa(ipoint(22)+(ibotseg-1)*increm(22)) = pmsa(ipoint(22)+(ibotseg-1)*increm(22)) + volume*Poros
               elseif (hmax .gt. z1 ) then
                  ! partialy in segment:
                  frlay = (hmax-z1)/depth
                  pmsa(ipoint(19)+(ibotseg-1)*increm(19)) = pmsa(ipoint(19)+(ibotseg-1)*increm(19)) + (nh4+no3)*volume*frlay
                  pmsa(ipoint(20)+(ibotseg-1)*increm(20)) = pmsa(ipoint(20)+(ibotseg-1)*increm(20)) + (aap+po4)*volume*frlay
                  pmsa(ipoint(21)+(ibotseg-1)*increm(21)) = pmsa(ipoint(21)+(ibotseg-1)*increm(21)) + (so4+sud)*volume*frlay
                  pmsa(ipoint(22)+(ibotseg-1)*increm(22)) = pmsa(ipoint(22)+(ibotseg-1)*increm(22)) + volume*frlay*Poros
              else
                  ! not in segment:
              endif

            endif

         endif

         ipnt  = ipnt  + increm(1:npnt)

      enddo

      ! express the availeble pool as g/m2

      ipnt  = ipoint(1:npnt)
      do iseg = 1 , noseg
         ibotseg     = NINT(pmsa(ipnt(6)))
         if ( ibotseg .eq. iseg ) then
            surf           = pmsa(ipnt(5))
            pmsa(ipnt(19)) = pmsa(ipnt(19))/surf
            pmsa(ipnt(20)) = pmsa(ipnt(20))/surf
            pmsa(ipnt(21)) = pmsa(ipnt(21))/surf
            ! RootShoot Model using the Michelis-Menten eq.
            if ( Nint(SWRoot) .eq. 1) then
                porewater      = pmsa(ipnt(22))
                ! express the availeble nitrogen conc in sediment as g/m3
                if (porewater .gt. 1.0e-10) then
                    TIN = pmsa(ipnt(19))*surf/porewater
                else
                    TIN = 0.0
                endif

!                if (porewater .le. 1.0e-10 .or. TIN .le. 1.0e-10) then
!                    TIN = 0.0
!                endif

                pmsa(ipnt(23)) = 0.0
                pmsa(ipnt(24)) = Vini + (Vmax*TIN)/(Km + TIN)
                pmsa(ipnt(25)) = 0.0
                pmsa(ipnt(26)) = 0.0
                pmsa(ipnt(27)) = 1.0 - pmsa(ipnt(24))
            endif
         else
             ! Fill all bottom sediment colume
             if ( Nint(SWRoot) .eq. 1) then
                 pmsa(ipnt(23)) = 0.0
                 pmsa(ipnt(24)) = pmsa(ipoint(24)+(ibotseg-1)*increm(24))
                 pmsa(ipnt(25)) = 0.0
                 pmsa(ipnt(26)) = 0.0
                 pmsa(ipnt(27)) = pmsa(ipoint(27)+(ibotseg-1)*increm(27))
             endif
         endif
         ipnt  = ipnt  + increm(1:npnt)
      enddo

      ! in order to avoid the error message from vbupt
      ! the switches should be checked
      if ( Nint(SWRoot) .eq. 1) then
          ipnt  = ipoint(1:npnt)
          do iseg = 1 , noseg
              if ((pmsa(ipnt(24))+pmsa(ipnt(27)))- 1.E-10 .lt. 0.0) then
                  pmsa(ipnt(23)) = 0.0
                  pmsa(ipnt(24)) = 0.5
                  pmsa(ipnt(25)) = 0.0
                  pmsa(ipnt(26)) = 0.0
                  pmsa(ipnt(27)) = 0.5
              endif
              ipnt  = ipnt  + increm(1:npnt)
          enddo
      endif

      return
      end
