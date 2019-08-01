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
!  *          SUBROUTINE TO SET I/O UNIT NUMBERS FOR BLOOM II          *
!  *********************************************************************

!  This module determines ALL I/O units for BLOOM II.
!  For historic reasons this subroutine
!  is configured such that BLOOM II units will be number 51 and up,
!  with the exception of units 5 and 6, which are the default console
!  units.

      subroutine setuni

      use bloom_data_dim
      use bloom_data_io  

      implicit none

      integer :: i, ioux

! In ECOLUMN/NZBLOOM version:
      ioux = 50
      do i = 1, 30
         iou (i) = i + ioux
      end do
      iou(5) = 5
      iou(6) = 6

! NZBLOOM: change IOU(10) to 70.
      iou(10) = 70

! In ECOLUMN/NZBLOOM version:
      ioux = 40
      do i = 41, 45
         iou (i) = i + ioux
      end do

! In ECOLUMN/NZBLOOM version:
      ioux = 30
      do i = 61, 69
         iou (i) = i + ioux
      end do

!  Initialize (old) unit names previously set in various other
!  subroutines of BLOOM II.
      inuni  = iou(9)
      ouuni  = iou(10)
      ipl1 = iou(41)
      ipl2 = iou(42)

! In PC version: use the standard BLOOM II file OUUNI also for EKOBLM
! BLOOM II and DLWQWQ, which use IOU(61), IOU(62), IOU(6) and
! IOU(3).
      iou(61) = ouuni
      iou(62) = ouuni
      iou(6)  = ouuni
      iou(3)  = ouuni
      return
      end
