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

      subroutine veloc      ( pmsa   , fl     , ipoint , increm, noseg , &
     &                        noflux , iexpnt , iknmrk , noq1  , noq2  , &
     &                        noq3   , noq4   )
      use m_dhkmrk

!>\file
!>       Horizontal stream velocity in a segment based on flows at interfaces

      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint( 20) ! I  Array of pointers in pmsa to get and store the data
      integer increm( 20) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt( 20)   ! L  Local work array for the pointering
      integer i           ! L  Local general loop counter
      integer iseg        ! L  Local loop counter for computational element loop
      integer iq          ! L  Local loop counter for exchanges loop
      integer ifrom       ! L  Local help variable 'from' comp. elem.
      integer ito         ! L  Local help variable 'to'   comp. elem.
      integer ispnt       ! L  Local help variable segment of an exchange
      integer ikmrkv      ! L  Local help variable feature 'from' comp. elem.
      integer ikmrkt      ! L  Local help variable feature 'to'   comp. elem.
      integer ikmrk1      ! L  Local help variable feature
      real(4) pi, radcf   ! L  pi and  its conversion to radians
      real(4) x, y        ! L  helpvariables for the direction of velocities
      real(4) u, u2       ! L  helpvariables for summation of velocities
      integer icalsw      ! L  computational switch
      integer iavgsw      ! L  averaging switch
      integer ip1, ip2, ip3, ip4, ip8, ip10, ip11
      integer in1, in2, in3, in4, in8, in10, in11
!
      parameter ( pi = 3.1415926,  radcf = pi / 180.)
!
!*******************************************************************************
!
!     Type    Name         I/O Description                                        Unit
!
      real(4) MaxVeloc    ! I  maximum horizontal flow velocity                   (m/s)
      real(4) Orient_1    ! I  orientation of main positive flow direction        (degrees)
      real(4) Orient_2    ! I  orientation of secondary positive flow direct      (degrees)
      real(4) SWCalcVelo  ! I  switch (1=lin avg, 2=Flow avg, 3=Area avg)         (-)
      real(4) SWAvgVelo   ! I  switch (1=Pythagoras, 2=Min, 3=Max)                (-)
      real(4) Area        ! I  exchange area                                      (m2)
      real(4) Flow        ! I  flow rate                                          (m3/s)
      real(4) Velocity    ! O  horizontal flow velocity                           (m/s)
      real(4) FlowDir     ! O  flow direction relative to North                   (degrees)
      real(4) Veloc1      ! O  horizontal flow velocity first direction           (m/s)
      real(4) Veloc2      ! O  horizontal flow velocity second direction          (m/s)
      real(4) FlowSeg     ! O  horizontal flow averaged over two directions       (m3/s)
      real(4) FlowSeg1    ! O  horizontal flow first direction                    (m3/s)
      real(4) FlowSeg2    ! O  horizontal flow second direction                   (m3/s)
      !
!*******************************************************************************
!
      ipnt        = ipoint

!.....Zero the work arrays
      pmsa( ipnt(1) : ipnt(1)+increm(1)*(noseg-1) : increm(1) ) = 0.0
      pmsa( ipnt(2) : ipnt(2)+increm(2)*(noseg-1) : increm(2) ) = 0.0
      pmsa( ipnt(3) : ipnt(3)+increm(3)*(noseg-1) : increm(3) ) = 0.0
      pmsa( ipnt(4) : ipnt(4)+increm(4)*(noseg-1) : increm(4) ) = 0.0

!.....Pointering invoer op segment niveau
      ip1  = ipoint(1)
      ip2  = ipoint(2)
      ip3  = ipoint(3)
      ip4  = ipoint(4)
      ip8  = ipoint(8)

!.....Pointering invoer op exchange niveau
      ip10 = ipoint(10)
      ip11 = ipoint(11)

!.....Increment invoer op segment niveau
      in1  = increm(1)
      in2  = increm(2)
      in3  = increm(3)
      in4  = increm(4)
      in8  = increm(8)

!.....Increment invoer op exchange niveau
      in10 = increm(10)
      in11 = increm(11)

       FlowSeg1 = 0.
       FlowSeg2 = 0.
!.....Berekening gemiddelde stroomsnelheid horizontale richting
      do 100 iq = 1, noq1+noq2

         if ( iq .eq. noq1+1 ) then
            ip1 = ip3           !   now point to the work arrays
            ip2 = ip4           !   for the second direction
            in1 = in3
            in2 = in4
         endif

         area = pmsa(ip10)      !   the area of this exchange
         flow = pmsa(ip11)      !   the flow of this exchange
         ip10 = ip10 + in10     !   set pointers to next
         ip11 = ip11 + in11     !   area and flow

         ifrom = iexpnt(1,iq)   !   the 'from' computational element
         ito   = iexpnt(2,iq)   !   the 'to'   computational element
         if ( ifrom .eq. 0 .or.  ito .eq. 0 )  cycle  !  closed boundary
         if ( ifrom .lt. 0 .and. ito .lt. 0 )  cycle  !  both open bnd

         ikmrkv = 0             !   see whether at least one of the
         ikmrkt = 0             !   segments (ifrom,ito) is interior to
                                !   the domain of this processor
         if ( ifrom  .gt. 0 ) call dhkmrk(1,iknmrk(ifrom),ikmrkv)
         if ( ito    .gt. 0 ) call dhkmrk(1,iknmrk(ito  ),ikmrkt)
                                !   if not, then skip the exchange
         if ( ikmrkv .eq. 0 .and. ikmrkt .eq. 0 ) cycle

         ikmrkv = 1             !   now check whether the segments
         ikmrkt = 1             !   are both active in the global domain
         if ( ifrom  .gt. 0 ) call dhkmrk(3,iknmrk(ifrom),ikmrkv)
         if ( ito    .gt. 0 ) call dhkmrk(3,iknmrk(ito  ),ikmrkt)
                                !   if not, then skip the exchange
         if ( ikmrkv .eq. 0 .or.  ikmrkt .eq. 0 ) cycle

         if ( abs(flow) .lt. 10.0E-25 ) flow = 0.0
         if ( area      .lt. 10.0E-25 ) then
            area = 1.0
            flow = 0.0
         endif

!           loop over from and to pointers
         do i = 1, 2
            ispnt = iexpnt(i,iq)
            if ( ispnt .gt. 0 ) then
               SWCalcVelo = pmsa( ip8 + (ispnt-1)*in8 )
               icalsw = int ( SWCalcVelo + 0.5 )
               select case ( icalsw )
                  case ( 1 )  !  lineaire middeling velocs
                     u  = flow/area
                     u2 = 1.0
                  case ( 2 )  !  debiet gewogen middeling
                     u  = flow*flow/area
                     u2 = flow
                  case ( 3 )  !  area gewogen middeling
                     u  = flow
                     u2 = area
                  case ( 4 )  !  maximale waarde snelheid
                     u  = flow/area
                     pmsa(ip1+(ispnt-1)*in1) =                           &
     &                        max( u, pmsa(ip1+(ispnt-1)*in1) )
                     pmsa(ip2+(ispnt-1)*in2) = 1.0
                     u  = 0.0
                     u2 = 0.0
                  case default
                     u  = 0.0
                     u2 = 0.0
               end select
               pmsa(ip1+(ispnt-1)*in1) = pmsa(ip1+(ispnt-1)*in1) + u
               pmsa(ip2+(ispnt-1)*in2) = pmsa(ip2+(ispnt-1)*in2) + u2
            endif
         enddo

  100 continue
!
      do 200 iseg = 1 , noseg
!
         call dhkmrk(1,iknmrk(iseg),ikmrk1)
         if ( ikmrk1 .eq. 1 ) then

            Veloc1 = pmsa(ipnt(1)) / max( pmsa(ipnt(2)), 1.0 )
            Veloc2 = pmsa(ipnt(3)) / max( pmsa(ipnt(4)), 1.0 )

            SWCalcVelo = pmsa( ipnt(8) )
            icalsw = int ( SWCalcVelo + 0.5 )

            if (icalsw .eq. 3) then
                FlowSeg1 = pmsa(ipnt(1))
                FlowSeg2 = pmsa(ipnt(3))
            endif

!           switch (1=Pythagoras, 2=Min, 3=Max)            (-)

            SWAvgVelo  = pmsa( ipnt(  9) )
            iavgsw = int ( SWAvgVelo  + 0.5 )
            select case ( iavgsw )
               case ( 1 )
                  Orient_1 = pmsa( ipnt(  6) ) * radcf
                  Orient_2 = pmsa( ipnt(  7) ) * radcf
                  if( Orient_1 .ge. 0.0 ) then
                     if( Orient_2 .lt. 0. ) Orient_2 = Orient_1 + 0.5 * pi
                     x = Veloc1 * sin(Orient_1) + Veloc2 * sin(Orient_2)
                     y = Veloc1 * cos(Orient_1) + Veloc2 * cos(Orient_2)
                     Velocity = sqrt(x*x + y*y)
                     if( x .ne. 0. ) then
                        FlowDir = atan2(x,y) / radcf
                        if( FlowDir .lt. 0. ) then
                           FlowDir = FlowDir + 360.
                        endif
                     else
                        if ( y .lt. 0. ) then
                           FlowDir = 180.
                        else
                           FlowDir = 0.
                        endif
                     endif
                  else
!                    orient1 negatif, assume perpendicular flowdir not defined
                     Velocity = sqrt ( Veloc1*Veloc1 + Veloc2*Veloc2 )
                     FlowSeg = sqrt ( FlowSeg1*FlowSeg1 + FlowSeg2*FlowSeg2 )
                     FlowDir = -1.0
                  endif
               case ( 2 )
                  Velocity = max(abs(Veloc1),abs(Veloc2))
                  FlowSeg = max(abs(FlowSeg1),abs(FlowSeg2))
                  FlowDir = -1.0
               case ( 3 )
                  Velocity = min(abs(Veloc1),abs(Veloc2))
                  FlowSeg = min(abs(FlowSeg1),abs(FlowSeg2))
                  FlowDir = -1.0
               case default
            end select

!           Maximize velocity if MaxVeloc > 0
            MaxVeloc   = pmsa( ipnt(  5) )
            if( MaxVeloc .gt. 0.0 .and. Velocity .gt. MaxVeloc ) then
               Velocity = MaxVeloc
            endif
!
            pmsa( ipnt( 12)   ) = Velocity
             pmsa( ipnt( 13)   ) = FlowDir
            if (icalsw .eq. 3) then
                pmsa( ipnt( 14)   ) = FlowSeg
            else
                pmsa( ipnt( 14)   ) = -999.9
            endif
            pmsa( ipnt( 15)   ) = Veloc1
            pmsa( ipnt( 16)   ) = Veloc2

         endif
!
         ipnt        = ipnt        + increm
!
  200 continue
!
      return
      end
