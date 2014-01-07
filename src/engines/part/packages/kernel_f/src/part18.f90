!!  Copyright(C) Stichting Deltares, 2012-2014.
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

module part18_mod

use precision

implicit none

contains
      subroutine part18 ( lgrid  , velo   , conc   , flres  , volume , &
                          area   , mnmaxk , npart  , mpart  , wpart  , &
                          zpart  , nopart , idelt  , nolay  , npwndw , &
                          vdiff  , pblay  , ptlay  , const  , nocons , &
                          lun2   , nosubs , layt   , kpart  , icvdf  , &
                          wvelo  , alpha  , nosubc , icvdf2    )

      integer(ip),dimension(:)    :: npart , mpart , kpart
      integer(ip),dimension(:,:)  :: lgrid

      real   (sp),dimension(:)    :: vdiff , velo  , volume , area , const , zpart
      real   (dp),dimension(:)    :: wvelo
      real   (sp),dimension(:,:)  :: conc
      real   (sp),dimension(:,:)  :: flres
      real   (sp),dimension(:,:)  :: wpart

      integer(ip) ::  icvdf  , icvdf2 , idelt , layt  , lun2
      integer(ip) ::  mnmaxk, nocons, nolay , nopart
      integer(ip) ::  nosubc, nosubs, npwndw
      real   (sp) ::  alpha , pblay  , ptlay

      return

      end subroutine
end module

