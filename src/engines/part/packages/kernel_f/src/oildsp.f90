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

module oildsp_mod

   contains
        subroutine oildsp ( lgrid   , nmax    , conc    , volume  , area    ,    &
                            npart   , mpart   , wpart   , radius  , nodye   ,    &
                            npwndw  , nopart  , itime   , idelt   , wvelo   ,    &
                            const   , lun2    , nosubs  , nolay   , lgrid2  ,    &
                            mmax    , xb      , yb      , kpart   , mapsub  ,    &
                            isfile  , nfract  , mstick  , nstick  , fstick  ,    &
                            xa      , ya      , pg      , lsettl  , xpart   ,    &
                            ypart   , zpart   , za      , locdep  , dps     ,    &
                            tcktot  , substi  , hmin    , npmax   , rhow    ,    &
                            amassd  , ioptrad , ndisapp , idisset , tydisp  ,    &
                            efdisp  , xpoldis , ypoldis , nrowsdis )

      use precision
      use typos

      implicit none

      integer   (ip), intent(in   ) :: itime
      integer  ( ip), intent(in   ) :: idelt
      integer  ( ip), intent(in   ) :: lun2
      integer  ( ip), intent(in   ) :: nfract
      integer  ( ip), intent(in   ) :: npmax
      integer  ( ip), intent(in   ) :: npwndw
      integer  ( ip), intent(in   ) :: nopart
      integer  ( ip), intent(in   ) :: nodye
      integer  ( ip), intent(in   ) :: nmax
      integer  ( ip), intent(in   ) :: mmax
      integer  ( ip), intent(in   ) :: nolay
      integer  ( ip), intent(in   ) :: nosubs
      integer  ( ip), intent(in   ) :: nstick
      logical       , intent(in   ) :: lsettl
      real     ( rp), pointer       :: const  (:)
      real     ( rp), intent(  out) :: fstick (nfract)
      real     ( rp), intent(in   ) :: rhow
      real     ( rp), intent(in   ) :: hmin
      real     ( rp), intent(  out) :: radius (nodye)
      real     ( dp), intent(in   ) :: wvelo  (*)
      integer  ( ip), pointer       :: lgrid (:,:)
      integer  ( ip), pointer       :: lgrid2(:,:)
      real     ( rp), pointer       :: xb     (:)
      real     ( rp), pointer       :: yb     (:)
      integer  ( ip), pointer       :: npart  (:)
      integer  ( ip), pointer       :: mpart  (:)
      integer  ( ip), pointer       :: kpart  (:)
      real     ( rp), pointer       :: xpart  (:)
      real     ( rp), pointer       :: ypart  (:)
      real     ( rp), pointer       :: zpart  (:)
      real     ( rp), pointer       :: xa     (:)
      real     ( rp), pointer       :: ya     (:)
      type(PlotGrid)                :: pg
      real     ( rp), pointer       :: za     (:)
      real     ( rp), pointer       :: locdep(:,:)
      real     ( rp), pointer       :: dps    (:)
      real     ( rp), pointer       :: tcktot (:)
      real     ( rp), pointer       :: conc  (:,:)
      real     ( rp), pointer       :: volume (:)
      real     ( rp), pointer       :: area   (:)
      integer  ( ip), intent(in   ) :: ioptrad(nodye)
      character( 20), intent(in   ) :: substi (nosubs)
      integer  ( ip), intent(in   ) :: isfile (nosubs)
      integer  ( ip), intent(in   ) :: mapsub (nosubs)
      integer  ( ip), intent(in   ) :: mstick (nosubs)
      real     ( rp), pointer       :: amassd  (:,:)
      real     ( rp), pointer       :: wpart   (:,:)
      integer  ( ip), intent(in   ) :: ndisapp
      integer  ( ip), pointer       :: idisset(:)
      integer  ( ip), pointer       :: tydisp (:)
      real     ( rp), pointer       :: efdisp (:)
      real     ( sp), pointer       :: xpoldis (:,:)
      real     ( sp), pointer       :: ypoldis (:,:)
      integer  ( ip), pointer       :: nrowsdis (:)

      fstick = 0.0
      radius = 0.0

      return

      end subroutine
end module
