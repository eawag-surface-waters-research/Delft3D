!!  Copyright (C)  Stichting Deltares, 2012-2019.
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

!  *********************************************************************
!  *  SUBROUTINE TO CONVERT UNITS BETWEEN BLOOM II AND DELWAQ          *
!  *********************************************************************
!
!  This module converts some of the inputs of BLOOM II to enable the
!  program to use g/m3 rather than mg/m3 as basic concentration unit.
!  Notes:
!  1. No checks are made that all units are indeed correct! Thus it
!     is the user's responsibility to provide a consistent data set.
!  2. The output formats of BLOOM II are not changed. Thus some
!     numbers of some variables are printed in a rather akward format.
!     It is assumed, however, that ECOLUMN itself will handle all
!     essential BLOOM II outputs in the future.

      subroutine cvrblm

      use bloom_data_dim
      use bloom_data_size 
      use bloom_data_caldynam
      use bloom_data_phyt    

      implicit none

      integer :: i

! Assuming that concentration units in the calling program are g/m3,
! where as BLOOM II uses mg/m3, it is necessary to convert
! 1.  the specific extinction coefficients
! 2.  the carbon to chlorophyll ratio (and hence the dry weight to
!     chlorophyll ratio)
! of all phytoplankton types.
! The specific extinction coefficient of detritus.
! The base and top levels of the growth and mortality constraints.
      do i = 1, nuspec
         chltoc(i) = chltoc(i) * 1.0d-3
         chlr(i)   = chltoc(i) * ctodry(i)
         ekx(i)    = ekx(i)    * 1000.0d0
      enddo
      biobas = biobas * 1.0d-3
      toplev = toplev * 1.0d-3
      return
      end
