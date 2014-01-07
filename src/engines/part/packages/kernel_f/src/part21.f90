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

module part21_mod

use precision

use typos

implicit none

contains
      subroutine part21 ( lun2   , lgrid  , lgrid2 , xb     , yb     , &
                          area   , volume , nmax   , mmax   , nolay  , &
                          nosubs , nopart , npart  , mpart  , kpart  , &
                          xpart  , ypart  , zpart  , wpart  , npwndw , &
                          pg     , amap   , xa     , ya     , za     , &
                          atotal , apeak  , adepth , imap   , nplay  , &
                          wsettl , irfac  , anfac  , lsettl , locdep , &
                          tcktot , dps   )

      type(PlotGrid)                   pg
      logical :: lsettl

      integer(ip),dimension(:)        :: npart , mpart , kpart
      integer(ip),dimension(:)        :: nplay
      integer(ip),dimension(:,:)      :: imap
      integer(ip),dimension(:,:)      :: lgrid , lgrid2

      real   (sp),dimension(:)        :: dps
      real   (sp),dimension(:)        :: tcktot
      real   (sp),dimension(:)        :: volume
      real   (sp),dimension(:)        :: wsettl
      real   (sp),dimension(:)        :: xa    , ya    , za
      real   (sp),dimension(:)        :: xb    , yb    , area
      real   (sp),dimension(:)        :: xpart , ypart , zpart
      real   (sp),dimension(:,:)      :: adepth, apeak
      real   (sp),dimension(:,:)      :: atotal
      real   (sp),dimension(:,:)      :: locdep
      real   (sp),dimension(:,:)      :: wpart
      real   (sp),dimension(:,:,:,:)  :: amap

      integer(ip) ::  lun2
      integer(ip) ::  irfac , mmax
      integer(ip) ::  npwndw , nmax   , nolay  , nopart , nosubs

      real   (sp) ::  anfac
      return

      end subroutine
end module
