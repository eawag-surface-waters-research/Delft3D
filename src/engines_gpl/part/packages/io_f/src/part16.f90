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

module part16_mod

use precision

implicit none

contains
      subroutine part16(lun2  , lgrid , conc  , mnmaxk, npart ,    &
                        mpart , wpart , nopart, itime , iptime,    &
                        npwndw, atotal, itstrt, iddtim, itstop,    &
                        nodye , nocont, idelt , kpart , npwndn,    &
                        nosubs, nolay , mnmax2, isfile, nosubc,    &
                        modtyp                        )

      integer(ip), dimension(:)        :: iptime
      integer(ip), dimension(:)        :: isfile
      integer(ip), dimension(:)        :: npart , mpart , kpart
      integer(ip), dimension(:,:)      :: lgrid

      real   (sp), dimension(:)        :: atotal
      real   (sp), dimension(:,:)      :: conc
      real   (sp), dimension(:,:)      :: wpart

      integer(ip) :: iddtim , idelt
      integer(ip) :: itime  , itstop , itstrt , lun2   , mnmax2
      integer(ip) :: mnmaxk , modtyp , nocont , nodye  , nolay
      integer(ip) :: nopart , nosubc , nosubs , npwndn , npwndw

      return

      end subroutine
end module

