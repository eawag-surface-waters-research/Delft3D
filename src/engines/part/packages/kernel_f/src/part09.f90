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

module part09_mod

contains
      subroutine part09 ( lun2   , itime  , nodye  , nwaste , mwaste ,  &
                          xwaste , ywaste , iwtime , amassd , aconc  ,  &
                          npart  , mpart  , xpart  , ypart  , zpart  ,  &
                          wpart  , iptime , nopart , radius , lgrid  ,  &
                          dx     , dy     , ndprt  , nosubs , kpart  ,  &
                          layt   , tcktot , nplay  , kwaste , nolay  ,  &
                          modtyp , zwaste , track  , nmdyer , substi )

      use precision

      implicit none

      integer  ( ip), intent(in   ) :: nodye
      integer  ( ip), intent(in   ) :: nosubs
      integer  ( ip), intent(in   ) :: layt
      integer  ( ip), intent(in   ) :: itime
      integer  ( ip), intent(inout) :: iwtime (nodye)
      integer  ( ip), intent(in   ) :: nwaste (nodye)
      integer  ( ip), intent(in   ) :: mwaste (nodye)
      real     ( rp), intent(in   ) :: xwaste (nodye)
      real     ( rp), intent(in   ) :: ywaste (nodye)
      real     ( rp), intent(in   ) :: zwaste (nodye)
      real     ( rp), intent(in   ) :: amassd (nosubs,nodye)
      real     ( rp), pointer       :: aconc  (:,:)
      integer  ( ip), intent(  out) :: npart  (*)
      integer  ( ip), intent(in   ) :: ndprt  (nodye)
      integer  ( ip), intent(  out) :: mpart  (*)
      real     ( rp), intent(  out) :: xpart  (*)
      real     ( rp), intent(  out) :: ypart  (*)
      real     ( rp), intent(  out) :: zpart  (*)
      real     ( rp), intent(  out) :: wpart  (nosubs,*)
      integer  ( ip), intent(  out) :: iptime (*)
      integer  ( ip), intent(inout) :: nopart
      real     ( rp), intent(in   ) :: radius (nodye)
      integer  ( ip), pointer       :: lgrid  (:,:)
      real     ( rp), pointer       :: dx     (:)
      real     ( rp), pointer       :: dy     (:)
      integer  ( ip), intent(in   ) :: modtyp
      integer  ( ip), intent(in   ) :: lun2
      integer  ( ip), intent(  out) :: kpart  (*)
      real     ( rp), intent(in   ) :: tcktot (layt)
      integer  ( ip)                :: nplay  (layt)
      integer  ( ip), intent(inout) :: kwaste (nodye)
      integer  ( ip), intent(in   ) :: nolay
      real     ( rp), intent(inout) :: track  (8,*)
      character( 20), intent(in   ) :: nmdyer (nodye)
      character( 20), intent(in   ) :: substi (nosubs)

      return

      end subroutine
end module
