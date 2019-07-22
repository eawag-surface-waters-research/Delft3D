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

      subroutine blfile (runnam, lunrep)

      use bloom_data_io  

      implicit none

      character(12)  runnam       ! Filename consisting of runid (no ext)
      integer        lunrep       ! Report file for error messages

      integer        iost         ! I/O-status

!  Call subroutine SETUNI to set I/O unit numbers for BLOOM II.
      call setuni

!  Open statement for BLOOM II input files.
      write (runnam(10:12),'(''frm'')')
      open (iou(12),file=runnam,iostat = iost)
      if (iost .ne. 0) then
         write (*,*) 'blfile: error opening .frm file'
         write (lunrep,*) 'blfile: error opening .frm file'
         call srstop(1)
      endif

      write (runnam(10:12),'(''d09'')')
      open (iou( 9),file=runnam,iostat = iost)
      if (iost .ne. 0) then
         write (*,*) 'blfile: error opening .d09 file'
         write (lunrep,*) 'blfile: error opening .d09 file'
         call srstop(1)
      endif

! Open statement for BLOOM II debug file.
      write (runnam(10:12),'(''dbg'')')
      open (iou(10),file=runnam,iostat = iost)
      if (iost .ne. 0) then
         write (*,*) 'blfile: error opening .dbg file'
         write (lunrep,*) 'blfile: error opening .dbg file'
         call srstop(1)
      endif

      return
      end
