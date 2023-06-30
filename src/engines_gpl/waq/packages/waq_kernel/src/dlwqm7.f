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
      module m_dlwqm7

      implicit none

      contains


      subroutine dlwqm7 ( noq    , noq1   , noq2   , area   , flow   ,
     &                    aleng  , ilflag , iopt   , ipoint , mixlen ,
     &                    iknmrk )

!     Deltares Software Centre

!>/File
!>              prepares a mixing length array (area/length) once for all substances
!>
!>              This subroutine does once and for all the logics of
!>              either variable from and to lengthes or fixed lengthes.
!>              It also does the logics (if corresponding options are set)
!>              - no dispersion at zero flow
!>              - no dispersion accross open boundaries
!>              - no dispersion to dry compuational volumes

!     Created   : November 2009 by Leo Postma

!     Modified  :

      use timers
      implicit none

      integer(4), intent(in   ) :: noq                 !< number of exchanges
      integer(4), intent(in   ) :: noq1                !< number of exchanges in first direction
      integer(4), intent(in   ) :: noq2                !< number of exchanges in second direction
      real   (4), intent(in   ) :: area(noq)           !< exchange surface areas (dim: noq)
      real   (4), intent(in   ) :: flow(noq)           !< flows accross exchange surfs (dim: noq)
      real   (4), intent(in   ) :: aleng(2,noq)        !< from- and to lengths (dim: 2*noq)
      integer(4), intent(in   ) :: ilflag              !< if 0 then 3 length values (equidistant grid)
      integer(4), intent(in   ) :: iopt                !< optoons for e.g. treatment of boundaries
      integer(4), intent(in   ) :: ipoint(4,noq)       !< exchange pointers (dim: 4 x noq)
      real   (4), intent(  out) :: mixlen(noq)         !< exchange surface areas (dim: noq)
      integer(4), intent(in   ) :: iknmrk( * )         !< feature array, bit zero indicates wet or not

!     Local declarations

      integer(4)                :: ifrom, ito          ! from- and to segments
      integer(4)                :: iq                  ! current edge

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqm7", ithandl )

      mixlen = 0.0
      if ( ilflag .eq. 0 ) then     ! deals with spatially constant lengthes
         do iq = 1, noq1
            mixlen(iq) = area(iq)/aleng(1,1)
         enddo
         do iq = noq1+1, noq1+noq2
            mixlen(iq) = area(iq)/aleng(2,1)
         enddo
         do iq = noq1+noq2+1, noq
            mixlen(iq) = area(iq)/aleng(1,2)
         enddo
      else                               ! deals with spatially varying  lengthes
         do iq = 1, noq
            if ( aleng(1,iq) + aleng(2,iq) .gt. 1.0E-25 ) then
               mixlen(iq) = area(iq)/(aleng(1,iq) + aleng(2,iq))
            endif
         enddo
      endif

      if ( btest(iopt,0) .and.           ! deals with no horizontal dispersion through the boundary
     &     btest(iopt,1)       ) then    ! thin dam option, no dispersion at zero flow
         do iq = 1,noq1+noq2
            ifrom = ipoint(1,iq)
            ito   = ipoint(2,iq)
            if ( ifrom .le. 0 .or. ito .le. 0 ) mixlen(iq) = 0.0
            if ( abs(flow(iq)) .lt. 10.0e-25 )  mixlen(iq) = 0.0
         enddo
      else if ( btest(iopt,1) ) then
         do iq = 1,noq1+noq2
            ifrom = ipoint(1,iq)
            ito   = ipoint(2,iq)
            if ( ifrom .le. 0 .or. ito .le. 0 ) mixlen(iq) = 0.0
         enddo
      else if ( btest(iopt,0) ) then
         do iq = 1,noq1+noq2
            ifrom = ipoint(1,iq)
            ito   = ipoint(2,iq)
            if ( abs(flow(iq)) .lt. 10.0e-25 )  mixlen(iq) = 0.0
         enddo
      endif

      do iq = 1,noq                                                   ! drying and flooding
            ifrom = ipoint(1,iq)
            ito   = ipoint(2,iq)
            if ( ifrom .gt. 0 ) then
               if ( .not. btest(iknmrk(ifrom),0) ) mixlen(iq) = 0.0   ! identified dry at start and end of timestep
            endif                                                     ! aggregated time step can be wet in between
            if ( ito   .gt. 0 ) then                                  ! start and end, that is why a check on 1 cm3/s
               if ( .not. btest(iknmrk(ito  ),0) ) mixlen(iq) = 0.0   ! life is not easy
            endif
      enddo

      if ( timon ) call timstop ( ithandl )
      return
      end


      end module m_dlwqm7
