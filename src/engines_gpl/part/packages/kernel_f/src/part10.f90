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

module part10_mod

contains
      subroutine part10 ( lgrid  , volume , flow   , dx     , dy     ,      &
                          area   , angle  , nmax   , mnmaxk , idelt  ,      &
                          nopart , npart  , mpart  , xpart  , ypart  ,      &
                          zpart  , iptime , rough  , drand  , lgrid2 ,      &
                          wvelo  , wdir   , decays , wpart  , pblay  ,      &
                          npwndw , vdiff  , nosubs , dfact  , modtyp ,      &
                          t0buoy , abuoy  , kpart  , mmax   , layt   ,      &
                          wsettl , depth  , ldiffz , ldiffh , lcorr  ,      &
                          acomp  , ipc    , accur  , xcor   , ycor   ,      &
                          tcktot , lun2   , alpha  , mapsub , nfract ,      &
                          taucs  , tauce  , chezy  , rhow   , lsettl ,      &
                          mstick , nstick , ioptdv , cdisp  , dminim ,      &
                          fstick , defang , floil  , xpart0 , ypart0 ,      &
                          xa0    , ya0    , xa     , ya     , npart0 ,      &
                          mpart0 , za     , locdep , dps    , nolay  ,      &
                          vrtdsp , stickdf, subst  , nbmax  , nconn  ,      &
                          conn   , tau    , caltau )

      use precision
      use typos

      implicit none

      real   (sp), external      :: rnd

      integer(ip), intent(in)    :: layt
      integer(ip), intent(in)    :: mmax
      integer(ip), intent(in)    :: mnmaxk
      integer(ip), intent(in)    :: nmax
      integer(ip), intent(in)    :: nopart
      integer(ip), intent(in)    :: nosubs

      integer(ip), intent(in)    :: idelt
      integer(ip), intent(in)    :: ioptdv

      integer(ip), intent(in)    :: ipc

      integer(ip), pointer    :: lgrid ( : , : )
      integer(ip), pointer    :: lgrid2( :, : )
      integer(ip), intent(in)    :: lun2
      integer(ip), pointer    :: mapsub( : )
      integer(ip), intent(in)    :: modtyp

      integer(ip), intent(in)    :: nfract

      integer(ip), intent(in)    :: npwndw
      integer(ip), intent(in)    :: nstick
      integer(ip), pointer :: iptime( : )
      integer(ip), pointer :: kpart ( : )
      integer(ip), pointer :: mpart ( : )
      integer(ip), pointer :: mpart0( : )
      integer(ip), pointer :: mstick( : )

      integer(ip), intent(inout) :: nolay
      integer(ip), pointer :: npart ( : )
      integer(ip), pointer :: npart0( : )
      integer(ip), pointer :: floil ( : )

      logical    , intent(in)    :: acomp
      logical    , intent(in)    :: lcorr
      logical    , intent(in)    :: ldiffh
      logical    , intent(in)    :: ldiffz
      logical    , intent(in)    :: lsettl

      real   (sp), pointer    :: abuoy ( : )
      real   (sp), intent(in)    :: accur
      real   (sp), intent(in)    :: alpha
      real   (sp), pointer    :: angle ( : )
      real   (sp), pointer    :: area  ( :  )
      real   (sp), intent(in)    :: cdisp
      real   (sp), intent(in)    :: dminim
      real   (sp), pointer    :: decays( :  )
      real   (sp), pointer    :: depth ( :  )
      real   (sp), intent(in)    :: drand ( 3 )

      real   (sp), pointer    :: flow  ( :    )
      real   (sp), pointer    :: fstick( : )
      real   (sp), intent(in)    :: pblay
      real   (sp), intent(in)    :: rhow
      real   (sp), intent(in)    :: rough
      real   (sp), pointer    :: t0buoy( : )
      real   (sp), intent(in)    :: tauce
      real   (sp), intent(in)    :: taucs
      real   (sp), pointer    :: volume( : )
      real   (dp), intent(in)    :: wdir (:)
      real   (dp), intent(in)    :: wvelo(:)
      real   (sp), pointer    :: xcor  ( : )
      real   (sp), pointer    :: ycor  ( : )
      real   (sp), intent(inout) :: chezy
      real   (sp), intent(inout) :: defang

      real   (sp), pointer :: dfact ( : )
      real   (sp), pointer :: dps   ( : )
      real   (sp), pointer :: dx    ( : )
      real   (sp), pointer :: dy    ( : )
      real   (sp), pointer :: locdep( :, : )
      real   (sp), pointer :: tcktot( : )
      real   (sp), pointer :: wpart ( :, :)
      real   (sp), pointer :: wsettl( : )
      real   (sp), pointer :: xa    ( : )
      real   (sp), pointer :: xa0   ( : )
      real   (sp), pointer :: xpart ( : )
      real   (sp), pointer :: xpart0( : )
      real   (sp), pointer :: ya    ( : )
      real   (sp), pointer :: ya0   ( : )
      real   (sp), pointer :: ypart ( : )
      real   (sp), pointer :: ypart0( : )
      real   (sp), pointer :: za    ( : )
      real   (sp), pointer :: zpart ( : )
      real   (sp), pointer :: vdiff ( : )
      real   (sp), pointer :: vrtdsp( :,: )
      character(len=*), pointer  :: subst ( : )

      integer(ip), intent(in   ) :: stickdf
      integer(ip), intent(in   ) :: nbmax
      integer(ip), intent(in   ) :: nconn
      type( pnt ), intent(in   ) :: conn (nconn)
      real   (sp), pointer       :: tau   ( : )
      logical    , intent(in   ) :: caltau

      return

      end subroutine part10

 end module
