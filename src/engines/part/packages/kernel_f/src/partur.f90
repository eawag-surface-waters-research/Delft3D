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

module partur_mod

use precision

implicit none

contains
      subroutine partur ( itime  , noudef , iutime  , mpart   , npart  , &
                          kpart  , xpart  , ypart   , zpart   , wpart  , &
                          iptime , nopart , lgrid   , nmax    , mmax   , &
                          amasud , ipnt   , sname   , nosubs  , nolay  , &
                          nocont , ndprt  , nodye   , lun     , buffer , &
                          volume , aconud , uscal   , isub    , finam  , &
                          iftime , ifopt  , nosyss  , isfil   , nosubud, &
                          snamud   )

      integer(ip),dimension(:)    :: iftime, ifopt , nosyss, ipnt
      integer(ip),dimension(:)    :: iptime , kpart
      integer(ip),dimension(:)    :: iutime
      integer(ip),dimension(:)    :: ndprt
      integer(ip),dimension(:)    :: npart , mpart
      integer(ip),dimension(:,:)  :: lgrid

      real   (sp),dimension(:)    :: buffer, volume, uscal
      real   (sp),dimension(:)    :: xpart , ypart , zpart
      real   (sp),dimension(:,:)  :: aconud
      real   (sp),dimension(:,:)  :: amasud
      real   (sp),dimension(:,:)  :: wpart

      integer(ip),dimension(:)    :: isub  , isfil

      character(len=256), dimension(:)  :: finam
      character(len= 20), dimension(:)  :: sname
      character(len= 20), dimension(:)  :: snamud

      integer(ip) ::  nmax
      integer(ip) ::  itime
      integer(ip) ::  lun    , mmax    , modtyp
      integer(ip) ::  nocont , nodye , nolay   , nopart
      integer(ip) ::  noudef
      integer(ip) ::  nosubs , nosubud, npmax
      return

      end subroutine
end module

