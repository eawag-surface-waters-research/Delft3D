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

      SUBROUTINE EDIT (FNAME,PROFIL,IRC)

      use bloom_data_io

      implicit none

      character*8 fname,profil
      integer cms, irc

!
! Call EDITBLM.BAT to invoke a user-specified editor.
!
      LINE(1) = 'EDITBLM '
      LINE(2) = FNAME
      CLOSE (IOU(29))
      IRC = CMS(LINE,16)
      OPEN (IOU(29), FILE = FNAME)
      RETURN
      END
