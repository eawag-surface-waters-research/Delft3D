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

      subroutine part03 ( lgrid  , volume , flow   , dx     , dy     ,   &
                          nmax   , mmax   , mnmaxk , lgrid2 , velo   ,   &
                          layt   , area   , depth  , dps    , locdep ,   &
                          zlevel , tcktot , ltrack)

      use precision

      implicit none

      integer(ip), intent(in   ) :: nmax
      integer(ip), intent(in   ) :: mmax
      integer(ip), intent(in   ) :: mnmaxk
      integer(ip), intent(in   ) :: layt
      integer(ip), intent(in   ) :: lgrid (nmax,mmax)
      integer(ip), intent(in   ) :: lgrid2(nmax,mmax)
      real   (rp), intent(in   ) :: dx    (nmax*mmax)
      real   (rp), intent(in   ) :: dy    (nmax*mmax)
      real   (rp), intent(in   ) :: volume(mnmaxk)
      real   (rp), intent(in   ) :: flow  (*)
      real   (rp), intent(in   ) :: area  (nmax*mmax)
      real   (rp), intent(  out) :: depth (nmax*mmax)
      real   (rp), intent(  out) :: velo  (mnmaxk)
      real   (rp), intent(in   ) :: dps   (nmax*mmax)
      real   (rp), intent(  out) :: locdep(nmax*mmax,layt)
      real   (rp), intent(  out) :: zlevel(nmax*mmax)
      real   (rp), intent(in   ) :: tcktot(layt)
      logical    , intent(in   ) :: ltrack

      depth = 0.0
      velo = 0.0
      locdep = 0.0
      zlevel = 0.0

      return

      end subroutine
