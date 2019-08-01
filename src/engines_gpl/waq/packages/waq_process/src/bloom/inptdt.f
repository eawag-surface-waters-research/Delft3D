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
!  *  INTEGER FUNCTION INPTDT TO READ CHARACTER STRINGS USING          *
!  *          INTERACTIVE ROUTINES IN "INTERACT TXTLIB"                *
!  *********************************************************************

      integer function inptdt(prmpt,token,len)

      use bloom_data_io  

      implicit none

      character*8 token
      integer prmpt,gets,len,uprcas,irc
!
      inptdt = 0
    1 continue
      if (gets(line,posit,80,8,token,len) .eq. 0) return
    4 read (inuni,10,end=2) line
   10 format (10a8)
      irc = uprcas (line,line,80)
      posit=1
      go to 1
    2 continue
      if (inuni.ne.5) go to 999
      rewind (inuni, err = 999)
      go to 4
 999  write (iou(6),1004) inuni
1004  format (' Hit end of file on unit ',I5)
      call srstop(6)
      end
