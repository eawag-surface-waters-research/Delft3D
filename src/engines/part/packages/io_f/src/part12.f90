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

module part12_mod
contains
      subroutine part12 ( lun1     , lname    , lun2     , title    , subst    ,    &
                          lgrid    , lgrid2   , lgrid3   , nmax     , mmax     ,    &
                          conc     , volume   , npart    , mpart    , wpart    ,    &
                          nopart   , itime    , idelt    , icwsta   , icwsto   ,    &
                          icwste   , atotal   , npwndw   , kpart    , pblay    ,    &
                          iptime   , npwndn   , modtyp   , nosubs   , nolay    ,    &
                          iyear    , imonth   , iofset   , pg       , rbuffr   ,    &
                          nosta    , mnmax2   , nosegl   , isfile   , mapsub   ,    &
                          layt     , area     , nfract   , lsettl   , mstick   ,    &
                          elt_names, elt_types, elt_dims , elt_bytes, locdep   ,    &
                          nosub_max, bufsize  )

      use precision
      use typos

      implicit none

      integer  ( ip), intent(in   ) :: lun1
      character( * ), intent(in   ) :: lname
      integer  ( ip), intent(in   ) :: lun2
      character( 40), intent(in   ) :: title (4)
      integer  ( ip), intent(in   ) :: nmax
      integer  ( ip), intent(in   ) :: mmax
      integer  ( ip), intent(in   ) :: nolay
      integer  ( ip), intent(in   ) :: nosubs
      integer  ( ip), intent(in   ) :: nopart
      character( 20), intent(in   ) :: subst (nosubs+1)
      integer  ( ip), intent(in   ) :: lgrid (nmax,mmax)
      integer  ( ip), intent(in   ) :: lgrid2(nmax,mmax)
      integer  ( ip), intent(in   ) :: lgrid3(nmax,mmax)
      integer  ( ip), intent(in   ) :: nosub_max
      real     ( rp), intent(  out) :: conc  (nosub_max,nmax*mmax*nolay)
      real     ( sp), intent(in   ) :: volume( * )
      integer  ( ip), intent(inout) :: npart ( nopart )
      integer  ( ip), intent(inout) :: mpart ( nopart )
      integer  ( ip), intent(inout) :: kpart ( nopart )
      real     ( sp), intent(inout) :: wpart (nosubs,nopart)
      integer  ( ip), intent(in   ) :: itime
      integer  ( ip), intent(in   ) :: idelt
      integer  ( ip), intent(in   ) :: icwsta
      integer  ( ip), intent(in   ) :: icwsto
      integer  ( ip), intent(in   ) :: icwste
      real     ( rp), intent(  out) :: atotal(nolay,nosubs)
      integer  ( ip), intent(inout) :: npwndw
      real     ( sp), intent(in   ) :: pblay
      integer  ( ip), intent(inout) :: iptime( nopart )
      integer  ( ip), intent(in   ) :: npwndn
      integer  ( ip), intent(in   ) :: modtyp
      integer  ( ip), intent(in   ) :: iyear
      integer  ( ip), intent(in   ) :: imonth
      integer  ( ip), intent(in   ) :: iofset
      type(PlotGrid)                   pg
      integer  ( ip), intent(in   ) :: bufsize
      real     ( rp)                :: rbuffr(bufsize)
      integer  ( ip), intent(in   ) :: nosta
      integer  ( ip), intent(in   ) :: mnmax2
      integer  ( ip), intent(in   ) :: nosegl
      integer  ( ip), intent(in   ) :: isfile(nosub_max)
      integer  ( ip), intent(in   ) :: mapsub(nosub_max)
      integer  ( ip), intent(in   ) :: layt
      real     ( sp), intent(in   ) :: area  (mnmax2)
      integer  ( ip), intent(in   ) :: nfract
      logical       , intent(in   ) :: lsettl
      integer  ( ip), intent(in   ) :: mstick(nosub_max)
      character( * ), pointer       :: elt_names(:)
      character( * ), pointer       :: elt_types(:)
      integer  ( ip), pointer       :: elt_dims (:,:)
      integer  ( ip), pointer       :: elt_bytes(:)
      real     ( rp)                :: locdep (nmax*mmax,nolay)

      conc  (1,1) = 0.0
      atotal(1,1) = 0.0

      return

      end subroutine
end module part12_mod
