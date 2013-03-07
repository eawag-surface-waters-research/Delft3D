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

      subroutine part15 ( lunpr  , itime  , spawnd , noseg  , nowind ,          &
                          iwndtm , wveloa , wdira  , wvelo  , wdir   )

      use precision

      implicit none

      integer       , intent(in   ) :: lunpr
      integer       , intent(in   ) :: itime
      logical       , intent(in   ) :: spawnd
      integer       , intent(in   ) :: noseg
      integer       , intent(in   ) :: nowind
      integer       , intent(in   ) :: iwndtm(nowind)
      real     ( 4 ), intent(in   ) :: wveloa(nowind)
      real     ( 4 ), intent(in   ) :: wdira (nowind)
      real     ( 8 ), intent(  out) :: wvelo (noseg )
      real     ( 8 ), intent(  out) :: wdir  (noseg )

      wvelo (1 ) = 0.0
      wdir  (1 ) = 0.0

      return

      end subroutine
