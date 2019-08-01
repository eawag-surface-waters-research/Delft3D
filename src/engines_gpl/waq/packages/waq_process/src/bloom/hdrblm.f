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

!  This module writes the headers for BLOOM II output files if
!  LPRINT = 1. Otherwise no BLOOM II specific output should be
!  produced.

      subroutine hdrblm

      use bloom_data_dim
      use bloom_data_io  
      use bloom_data_phyt    
      use bloom_data_putin   
      use bloom_data_sumou  

      implicit none

!  Write heading for output on units IOU(6), OUUNI, IOU(15) and optionally IOU(21).
      call formfe (ouuni)
      write (ouuni,99999) iyear,case
      write (ouuni,99990) com
      call formfe (iou(14))
      write (iou(14),99999) iyear,case
      write (iou(14),99990) com
      if ( ioflag .ne. 0) then
         call formfe (iou(21))
         write (iou(21),99999) iyear,case
         write (iou(21),99990) com
      end if
      if ( idump .ne. 0) then
         call formfe (iou(6))
         write (iou(6),99999) iyear,case
         write (iou(6),99990) com
      end if

! Formats this subroutine.
99999 format (1X,'YEAR',1X,I4,3X,13A8)
99990 format (3X,9A8)
      return
      end
