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

      subroutine part11( lgrid  , xp     , yp     , nmax   , npart  ,        &
                         mpart  , xpart  , ypart  , xa     , ya     ,        &
                         nopart , npwndw , lgrid2 , kpart  , zpart  ,        &
                         za     , locdep , dps    , nolay  , mmax   ,        &
                         tcktot )

      use precision

      implicit none

      integer  ( ip), intent(in   ) :: nmax
      integer  ( ip), intent(in   ) :: mmax
      integer  ( ip), intent(in   ) :: nolay
      integer  ( ip), intent(in   ) :: npwndw
      integer  ( ip), intent(in   ) :: nopart
      integer  ( ip), intent(in   ) :: lgrid (nmax,mmax)
      integer  ( ip), intent(in   ) :: lgrid2(nmax,mmax)
      real     ( rp), intent(in   ) :: xp    (nmax*mmax)
      real     ( rp), intent(in   ) :: yp    (nmax*mmax)
      real     ( rp), intent(in   ) :: locdep(nmax*mmax,nolay)
      real     ( rp), intent(in   ) :: tcktot(nolay )
      real     ( rp), intent(in   ) :: dps   (nmax*mmax)
      integer  ( ip), intent(in   ) :: npart (nopart)
      integer  ( ip), intent(in   ) :: mpart (nopart)
      integer  ( ip), intent(in   ) :: kpart (nopart)
      real     ( rp), intent(in   ) :: xpart (nopart)
      real     ( rp), intent(in   ) :: ypart (nopart)
      real     ( rp), intent(in   ) :: zpart (nopart)
      real     ( rp), intent(  out) :: xa    (nopart)
      real     ( rp), intent(  out) :: ya    (nopart)
      real     ( rp), intent(  out) :: za    (nopart)

      xa = 0.0
      ya = 0.0
      za = 0.0

      return

      end subroutine part11

