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

      subroutine part08 ( lun    , nodye  , nocont , ictmax , amassd ,  &
                          ictime , amassc , aconc  , tmass  , tmassc ,  &
                          nosubs , ndprt  , tmassu , ftime  , linear ,  &
                          substi , nmdyer , nmconr )

      use precision

      implicit none

      integer (ip), intent(in   ) :: lun
      integer (ip), intent(in   ) :: nodye
      integer (ip), intent(in   ) :: nocont
      integer (ip), intent(in   ) :: nosubs
      integer (ip), intent(in   ) :: ictmax(nocont)
      real    (rp), intent(in   ) :: amassd(nosubs,nodye)
      integer (ip), intent(in   ) :: ictime(nocont, * )
      real    (rp), intent(in   ) :: amassc(nocont,nosubs,*)
      real    (rp), intent(  out) :: aconc (nodye+nocont,nosubs)
      real    (rp), intent(  out) :: tmass (nosubs)
      real    (rp), intent(  out) :: tmassc(nocont,nosubs)
      integer (ip), intent(in   ) :: ndprt (nodye+nocont)
      real    (rp), intent(  out) :: tmassu(nocont)
      real    (rp), intent(in   ) :: ftime (nocont,*)
      integer (ip), intent(in   ) :: linear(nocont)
      character(*), intent(in   ) :: substi(nosubs)
      character(*), intent(in   ) :: nmdyer(nodye )
      character(*), intent(in   ) :: nmconr(nocont)
      return

      end subroutine
