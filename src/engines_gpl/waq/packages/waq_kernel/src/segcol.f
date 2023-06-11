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

      subroutine segcol(nosss , noq1  , noq2   , noq3  , noq4  ,
     &                  ipoint, iknmrk, isegcol)

      ! function : sets the top of the column for every segment

      use m_dhkmrk
      use timers
      implicit none

      integer, intent(in   ) :: nosss          ! total number of segments
      integer, intent(in   ) :: noq1           ! number of exchange pointers in first direction
      integer, intent(in   ) :: noq2           ! number of exchange pointers in first direction
      integer, intent(in   ) :: noq3           ! number of exchange pointers in first direction
      integer, intent(in   ) :: noq4           ! number of exchange pointers in first direction
      integer, intent(in   ) :: ipoint(4,*)    ! exchange pointers
      integer, intent(in   ) :: iknmrk(*)      ! segment attributes
      integer, intent(  out) :: isegcol(*)     ! pointer from segment to top of column

      ! local declarations

      integer                :: iseg           ! segment index
      integer                :: iq             ! exchange index
      integer                :: ifrom          ! from segment in pointer
      integer                :: ito            ! to segment in pointer
      integer                :: ikmrkv         ! first attribute from segment

      do iseg = 1, nosss
         isegcol(iseg) = iseg
      enddo

      do iq = noq1 + noq2 + 1, noq1 + noq2 + noq3
         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)
         if ( ifrom .gt. 0 .and. ito .gt. 0 ) then
            isegcol(ito) = isegcol(ifrom)
         endif
      enddo

      do iq = noq1+noq2+noq3+1 , noq1+noq2+noq3+noq4

         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)

         ! only positive segments

         if ( ifrom .le. 0 .or. ito .le. 0 ) cycle

         ! only if from segment is not a water segment

         call dhkmrk(1,iknmrk(ifrom),ikmrkv)
         if ( ikmrkv.ne.3 ) cycle

         isegcol(ito) = isegcol(ifrom)

      enddo

      end
