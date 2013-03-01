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

module partwq_mod

use precision

implicit none

contains
      subroutine partwq ( lgrid  , nmax   , conc   , volume , area   , &
                          npart  , mpart  , wpart  , radius , nodye  , &
                          npwndw , nopart , idelt  , velo   , wvelo  , &
                          const  , nocons , ptlay  , lun2   , nosubs , &
                          nolay  , lgrid2 , mmax   , xb     , yb     , &
                          t0cf   , acf    , nwaste , mwaste , kpart  , &
                          mapsub , layt   , mnmaxk       )

      integer(ip), pointer, dimension(:)     :: npart , mpart
      integer(ip), pointer, dimension(:,:)   :: lgrid , lgrid2
      real   (sp), pointer, dimension(:)     :: volume, area  , velo  , radius , const , xb , yb
      real   (dp), pointer, dimension(:)     :: wvelo
      real   (sp), pointer, dimension(:,:)   :: conc
      real   (sp), pointer, dimension(:,:)   :: wpart

      integer(ip), pointer, dimension(:)     :: kpart    , mapsub
      integer(ip), pointer, dimension(:)     :: nwaste   , mwaste
      real   (sp), pointer, dimension(:)     :: t0cf     , acf

      integer(ip), parameter        :: maxld = 50

      integer(ip) ::  idelt
      integer(ip) ::  layt
      integer(ip) ::  lun2
      integer(ip) ::  nodye , mmax   , mnmaxk
      integer(ip) ::  nocons , nolay  , nmax   , nosubs , nopart
      integer(ip) ::  npwndw

      real   (sp) ::  ptlay

      return

      end subroutine
end module

