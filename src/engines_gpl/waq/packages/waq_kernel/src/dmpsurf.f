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
      module m_dmpsurf

      implicit none

      contains


      subroutine dmpsurf(nosss, ndmpar, ipdmp , isegcol, surf  , dmp_surf)

      ! function            : sums surf for sub-area's no double counting over the layers

      use timers
      implicit none

      integer, intent(in   ) :: nosss          ! total number of segments
      integer, intent(in   ) :: ndmpar         ! Number of dump areas
      integer, intent(in   ) :: ipdmp(*)       ! pointer structure dump area's
      integer, intent(in   ) :: isegcol(*)     ! pointer from segment to top of column
      real   , intent(in   ) :: surf(*)        ! horizontal surface per segment
      real   , intent(  out) :: dmp_surf(*)    ! horizontal surface per dump area

      ! local declarations

      integer                :: itel           ! index counter
      integer                :: idump          ! dump area number
      integer                :: nsc            ! number of segment contributions
      integer                :: isc            ! index of segment contributions
      integer                :: iseg           ! segment number
      integer                :: icol           ! segment number top of column
      integer, allocatable   :: i_surf(:)      ! indication if column is already in area
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dmpsurf", ithandl )

      ! loop over the dump area's, sum value

      allocate(i_surf(nosss))
      dmp_surf(1:ndmpar) = 0.0
      itel               = 0
      do idump = 1 , ndmpar
         i_surf = 0
         nsc = ipdmp(idump)
         do isc = 1 , nsc
            itel  = itel + 1
            iseg  = ipdmp(ndmpar+itel)
            if ( iseg .gt. 0 ) then
               icol = isegcol(iseg)
               if ( i_surf(icol) .eq. 0 ) then
                  dmp_surf(idump) = dmp_surf(idump) + surf  (iseg)
                  i_surf(icol) = 1
               endif
            endif
         enddo
      enddo
      deallocate(i_surf)

      if ( timon ) call timstop ( ithandl )
      return
      end

      end module m_dmpsurf
