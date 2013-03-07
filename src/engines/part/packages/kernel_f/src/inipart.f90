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

      subroutine inipart( lgrid   , lgrid2  , nmax    , mmax    , xcor    ,     &
                          ycor    , nopart  , nosubs  , subst   , ini_file,     &
                          xpol    , ypol    , npol    , wpart   , xpart   ,     &
                          ypart   , zpart   , npart   , mpart   , kpart   ,     &
                          iptime  , npmax   , nrowsmax, lunpr   )

      use precision

      implicit none

      integer  ( ip), intent(in   ) :: nmax
      integer  ( ip), intent(in   ) :: mmax
      integer  ( ip), intent(in   ) :: npmax
      integer  ( ip), intent(inout) :: nopart
      integer  ( ip), intent(in   ) :: lgrid (nmax,mmax)
      integer  ( ip), intent(in   ) :: lgrid2(nmax,mmax)
      real     ( rp), intent(in   ) :: xcor  (nmax*mmax)
      real     ( rp), intent(in   ) :: ycor  (nmax*mmax)
      integer  ( ip), intent(inout) :: nosubs
      character( * ), intent(in   ) :: subst (*)
      character( * ), intent(in   ) :: ini_file
      integer  ( ip), intent(in   ) :: npol
      integer  ( ip), intent(in   ) :: nrowsmax
      real     ( rp), intent(  out) :: xpol  (nrowsmax)
      real     ( rp), intent(  out) :: ypol  (nrowsmax)
      real     ( rp), intent(  out) :: wpart (nosubs,npmax)
      real     ( rp), intent(  out) :: xpart (npmax)
      real     ( rp), intent(  out) :: ypart (npmax)
      real     ( rp), intent(  out) :: zpart (npmax)
      integer  ( ip), intent(  out) :: npart (npmax)
      integer  ( ip), intent(  out) :: mpart (npmax)
      integer  ( ip), intent(  out) :: kpart (npmax)
      integer  ( ip), intent(  out) :: iptime(npmax)
      integer  ( ip), intent(in   ) :: lunpr

      xpol = 0.0 
      ypol = 0.0
      wpart = 0.0
      xpart = 0.0
      ypart = 0.0
      zpart = 0.0
      
      npart = 0
      mpart = 0
      kpart = 0
      iptime = 0

      return

      end subroutine inipart
