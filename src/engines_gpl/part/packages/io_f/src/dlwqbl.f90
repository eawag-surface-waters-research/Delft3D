!!  Copyright(C) Stichting Deltares, 2012-2013.
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

!!  Note: The "part" engine is not yet Open Source, but still under
!!  development. This package serves as a temporary dummy interface for
!!  the references in the "waq" engine to the "part" engine.

      subroutine dlwqbl ( lunin  , lunout , itime  , idtime , itime1 ,    &
     &                    itime2 , ihdel  , nftot  , nrtot  , array1 ,    &
     &                    result , ipnt   , luntxt , isflag , ifflag ,    &
     &                    update )

      use precision

      implicit none

      integer  (ip), intent(in   ) :: lunin
      integer  (ip), intent(in   ) :: lunout
      integer  (ip), intent(in   ) :: itime
      integer  (ip), intent(inout) :: idtime
      integer  (ip), intent(inout) :: itime1
      integer  (ip), intent(inout) :: itime2
      integer  (ip), intent(in   ) :: ihdel
      integer  (ip), intent(in   ) :: nftot
      integer  (ip), intent(in   ) :: nrtot
      real     (sp), intent(inout) :: array1(nftot)
      real     (sp), intent(inout) :: result(nrtot)
      integer  (ip), intent(in   ) :: ipnt  (nftot)
      character( *), intent(in   ) :: luntxt
      integer  (ip), intent(in   ) :: isflag
      integer  (ip), intent(in   ) :: ifflag
      logical      , intent(  out) :: update

      return

      end subroutine
