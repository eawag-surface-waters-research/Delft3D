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

module partwr_mod

use precision

implicit none

contains
      subroutine partwr ( lgrid  , nmax   , conc   , volume , area   , &
                          npart  , mpart  , wpart  , zpart  , npwndw , &
                          nopart , idelt  , const  , nocons , buffer , &
                          lun2   , nolay  , mmax   , itime  , kpart  , &
                          mapsub , isfile , mnmaxk , mnmax2 , finnh4 , &
                          finno3         )

      character(len=256) :: finnh4, finno3

      integer(ip), dimension(:)   :: isfile
      integer(ip), dimension(:)   :: kpart , mapsub
      integer(ip), dimension(:)   :: npart , mpart
      integer(ip), dimension(:,:) :: lgrid

      real   (sp), dimension(:)   :: buffer
      real   (sp), dimension(:)   :: volume, area  , const , zpart
      real   (sp), dimension(:,:) :: conc
      real   (sp), dimension(:,:) :: wpart

      integer(ip) ::  idelt
      integer(ip) ::  mnmax2 , nopart, npwndw
      integer(ip) ::  itime  , lun2  , nocons
      integer(ip) ::  mnmaxk , nolay
      integer(ip) ::  mmax   , nmax

      return
      end subroutine
end module
