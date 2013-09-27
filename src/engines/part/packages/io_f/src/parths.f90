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

module parths_mod

use precision
use typos

implicit none

contains
      subroutine parths(lun1     , lun2     , title    , subst    , mmax     ,  &
                        lgrid2   , nmax     , volume   , area     , npart    ,  &
                        mpart    , xpart    , ypart    , wpart    , nopart   ,  &
                        itime    , idelt    , xa       , npwndw   , lgrid    ,  &
                        ya       , xb       , yb       , pg       , pblay    ,  &
                        modtyp   , nolay    , nosubs   , conc     , chismp   ,  &
                        chispl   , nosta    , nmstat   , xstat    , ystat    ,  &
                        nstat    , mstat    , nplsta   , mplsta   , ihstrt   ,  &
                        ihstop   , ihstep   , ihplot   , finam    , kpart    ,  &
                        mnmax2   , noseglp  , nfract   , lsettl   , mstick   ,  &
                        elt_names, elt_types, elt_dims , elt_bytes, rbuffr   ,  &
                        zpart    , za       , locdep   , dps      , tcktot   ,  &
                        lgrid3   )

      type(PlotGrid)                   pg
      character(len=*), pointer, dimension(:) :: nmstat
      character(len=*), pointer, dimension(:) :: subst
      character( 40), intent(in   ) :: title (4)
      character(len=256)                      :: finam
      logical                                 :: lsettl

      character(len=16), pointer, dimension(:) ::  elt_names, elt_types

      integer(ip), pointer, dimension(:)       :: ihplot
      integer(ip), pointer, dimension(:)       :: nplsta, mplsta
      integer(ip), pointer, dimension(:)       :: nstat , mstat
      integer(ip), pointer, dimension(:,:)     :: elt_dims
      integer(ip), pointer, dimension(:)       :: elt_bytes
      integer(ip), pointer, dimension(:)       :: mstick
      integer(ip), pointer, dimension(:)       :: npart , mpart , kpart
      integer(ip), pointer, dimension(:,:)     :: lgrid , lgrid2, lgrid3
      real   (sp), pointer, dimension(:)       :: xa    , ya    , xb     , yb
      real   (sp), pointer, dimension(:)       :: xpart , ypart , volume , area
      real   (sp), pointer, dimension(:)       :: xstat , ystat
      real   (sp), pointer, dimension(:,:,:)   :: chismp
      real   (sp), pointer, dimension(:,:,:)   :: chispl
      real   (sp), pointer, dimension(:)       :: zpart , za
      real   (sp), pointer, dimension(:,:)     :: conc
      real   (sp), pointer, dimension(:,:)     :: wpart
      real   (sp), pointer, dimension(:,:)     :: locdep
      real   (sp), pointer, dimension(:)       :: tcktot
      real   (sp), pointer, dimension(:)       :: dps
      real   (sp), pointer, dimension(:)       :: rbuffr

      integer(ip) :: idelt     ,ihstep    ,ihstop
      integer(ip) :: lun1      ,lun2      ,mmax
      integer(ip) :: mnmax2    ,noseglp   ,modtyp    ,nfract    ,ihstrt
      integer(ip) :: nmax      ,nolay     ,nopart
      integer(ip) :: nosubs    ,npwndw    ,itime     ,nosta
      real   (sp) :: pblay
      return

      end subroutine
end module
