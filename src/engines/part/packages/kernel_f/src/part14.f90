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

module part14_mod

   contains
      subroutine part14 ( itime  , idelt  , nodye  , nocont , ictime ,    &
                          ictmax , nwaste , mwaste , xwaste , ywaste ,    &
                          zwaste , aconc  , rem    , npart  , ndprt  ,    &
                          mpart  , xpart  , ypart  , zpart  , wpart  ,    &
                          iptime , nopart , pblay  , radius , lgrid  ,    &
                          dx     , dy     , ftime  , tmassu , nosubs ,    &
                          ncheck , t0buoy , modtyp , abuoy  , t0cf   ,    &
                          acf    , lun2   , kpart  , layt   , tcktot ,    &
                          nplay  , kwaste , nolay  , linear , track  ,    &
                          nmconr  )

      use precision

      implicit none

      integer  ( ip), intent(in   ) :: nocont
      integer  ( ip), intent(in   ) :: nodye
      integer  ( ip), intent(in   ) :: nosubs
      integer  ( ip), intent(in   ) :: layt
      integer  ( ip), intent(in   ) :: itime
      integer  ( ip), intent(in   ) :: idelt
      integer  ( ip), intent(in   ) :: ictime (nocont,*)
      integer  ( ip), intent(in   ) :: ictmax (nocont)
      integer  ( ip), intent(in   ) :: nwaste (nodye+nocont)
      integer  ( ip), intent(in   ) :: mwaste (nodye+nocont)
      real     ( rp), intent(in   ) :: xwaste (nodye+nocont)
      real     ( rp), intent(in   ) :: ywaste (nodye+nocont)
      real     ( rp), intent(in   ) :: zwaste (nodye+nocont)
      real     ( rp), intent(in   ) :: aconc  (nocont+nodye,nosubs)
      real     ( rp), intent(inout) :: rem    (nocont)
      integer  ( ip), intent(  out) :: npart  (*)
      integer  ( ip), intent(in   ) :: ndprt  (nodye+nocont)
      integer  ( ip), intent(  out) :: mpart  (*)
      real     ( rp), intent(  out) :: xpart  (*)
      real     ( rp), intent(  out) :: ypart  (*)
      real     ( rp), intent(  out) :: zpart  (*)
      real     ( rp), intent(  out) :: wpart  (nosubs,*)
      integer  ( ip), intent(  out) :: iptime (*)
      integer  ( ip), intent(inout) :: nopart
      real     ( rp), intent(in   ) :: pblay
      real     ( rp), intent(in   ) :: radius (nodye+nocont)
      integer  ( ip), pointer       :: lgrid  (:,:)
      real     ( rp), pointer       :: dx     (:)
      real     ( rp), pointer       :: dy     (:)
      real     ( rp), intent(in   ) :: ftime  (nocont,*)
      real     ( rp), intent(in   ) :: tmassu (nocont)
      integer  ( ip), intent(  out) :: ncheck (nocont)
      real     ( rp), intent(  out) :: t0buoy (*)
      integer  ( ip), intent(in   ) :: modtyp
      real     ( rp), intent(  out) :: abuoy  (*)
      real     ( rp), intent(in   ) :: t0cf   (nocont)
      real     ( rp), intent(in   ) :: acf    (nocont)
      integer  ( ip), intent(in   ) :: lun2
      integer  ( ip), intent(  out) :: kpart  (*)
      real     ( rp), intent(in   ) :: tcktot (layt)
      integer  ( ip)                :: nplay  (layt)
      integer  ( ip), intent(in   ) :: kwaste (nodye+nocont)
      integer  ( ip), intent(in   ) :: nolay
      integer  ( ip), intent(in   ) :: linear (nocont)
      real     ( rp), intent(inout) :: track  (8,*)
      character( 20), intent(in   ) :: nmconr (nocont)

      npart  (1) = 0
      mpart  (1) = 0
      iptime (1) = 0
      ncheck (1) = 0
      kpart  (1) = 0

      xpart  (1) = 0.0
      ypart  (1) = 0.0
      zpart  (1) = 0.0
      wpart  (1,1) = 0.0
      t0buoy (1) = 0.0
      abuoy  (1) = 0.0

      return

      end subroutine
end module
