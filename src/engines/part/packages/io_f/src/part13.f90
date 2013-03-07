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

module part13_mod
contains
      subroutine part13 ( lun1     , lname    , lun2     , title    , subst    ,   &
                          lgrid2   , nmax     , volume   , area     , npart    ,   &
                          mpart    , xpart    , ypart    , wpart    , nopart   ,   &
                          itime    , idelt    , ipset    , iptmax   , xa       ,   &
                          ya       , xb       , yb       , pg       , recovr   ,   &
                          atotal   , iyear    , imonth   , iofset   , npwndw   ,   &
                          lgrid    , pblay    , modtyp   , apeak    , adepth   ,   &
                          nolay    , nosubs   , rbuffr   , kpart    , itrack   ,   &
                          nplot    , mapsub   , ntrack   , isfile   , mmax     ,   &
                          nfract   , lsettl   , mstick   , elt_names, elt_types,   &
                          elt_dims , elt_bytes, locdep   , zpart    , za       ,   &
                          dps      , tcktot   , nosub_max, bufsize  )

      use precision
      use typos

      implicit none

      integer  ( ip), intent(in   ) :: lun1
      character( * ), intent(in   ) :: lname
      integer  ( ip), intent(in   ) :: lun2
      character( 40), intent(in   ) :: title (4)
      integer  ( ip), intent(in   ) :: nosubs
      integer  ( ip), intent(in   ) :: nosub_max
      character( * ), intent(in   ) :: subst (nosub_max)
      integer  ( ip), intent(in   ) :: nmax
      integer  ( ip), intent(in   ) :: mmax
      integer  ( ip), intent(in   ) :: lgrid2(nmax,mmax)
      integer  ( ip), intent(in   ) :: nolay
      real     ( rp), intent(in   ) :: volume(nmax*mmax*nolay)
      real     ( rp), intent(in   ) :: area  (nmax*mmax*nolay)
      integer  ( ip), intent(in   ) :: nopart
      integer  ( ip), intent(in   ) :: npart (nopart)
      integer  ( ip), intent(in   ) :: mpart (nopart)
      real     ( rp), intent(in   ) :: xpart (nopart)
      real     ( rp), intent(in   ) :: ypart (nopart)
      real     ( rp), intent(in   ) :: wpart (nosubs,nopart)
      integer  ( ip), intent(in   ) :: itime
      integer  ( ip), intent(in   ) :: idelt
      integer  ( ip), intent(in   ) :: iptmax
      integer  ( ip), intent(in   ) :: ipset (iptmax)
      real     ( rp), intent(  out) :: xa    (nopart)
      real     ( rp), intent(  out) :: ya    (nopart)
      real     ( rp), intent(in   ) :: xb    (nmax*mmax)
      real     ( rp), intent(in   ) :: yb    (nmax*mmax)
      type(PlotGrid), intent(in   ) :: pg
      real     ( rp), intent(in   ) :: recovr(iptmax)
      real     ( rp), intent(  out) :: atotal(nolay,nosubs)
      integer  ( ip), intent(in   ) :: iyear
      integer  ( ip), intent(in   ) :: imonth
      integer  ( ip), intent(in   ) :: iofset
      integer  ( ip), intent(in   ) :: npwndw
      integer  ( ip), intent(in   ) :: lgrid (nmax,mmax)
      real     ( rp), intent(in   ) :: pblay
      integer  ( ip), intent(in   ) :: modtyp
      real     ( rp), intent(  out) :: apeak (nosubs,nolay)
      real     ( rp), intent(  out) :: adepth(nosubs,nolay)
      integer  ( ip), intent(in   ) :: bufsize
      real     ( rp)                :: rbuffr(bufsize)
      integer  ( ip), intent(in   ) :: kpart (nopart)
      integer  ( ip), intent(in   ) :: itrack
      integer  ( ip), intent(in   ) :: ntrack
      integer  ( ip), intent(in   ) :: nplot (ntrack)
      integer  ( ip), intent(in   ) :: mapsub(nosubs)
      integer  ( ip), intent(in   ) :: isfile(nosubs)
      integer  ( ip), intent(in   ) :: nfract
      logical       , intent(in   ) :: lsettl
      integer  ( ip), intent(in   ) :: mstick(nosubs)
      character( * ), pointer       :: elt_names(:)
      character( * ), pointer       :: elt_types(:)
      integer  ( ip), pointer       :: elt_dims (:,:)
      integer  ( ip), pointer       :: elt_bytes(:)
      real     ( rp)                :: locdep(nmax*mmax,nolay)
      real     ( rp), intent(in   ) :: zpart (nopart)
      real     ( rp), intent(  out) :: za    (nopart)
      real     ( rp), intent(in   ) :: dps   (nmax*mmax)
      real     ( rp), intent(in   ) :: tcktot(nolay+1)

      xa = 0.0
      ya = 0.0
      za = 0.0
      atotal = 0.0
      apeak = 0.0
      adepth = 0.0
      
      return

      end subroutine
end module part13_mod
