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
!  *  INTEGER FUNCTION INPTNM TO READ VARIABLES USING INTERACTIVE      *
!  *              ROUTINES IN "INTERACT TXTLIB"                        *
!  *********************************************************************

      integer function inptnm(prmpt,dnum,inum,type)

      use bloom_data_io  

      implicit none

      real(8)  :: dnum, rnum
      integer  :: prmpt,type,stoi,stor,stod,uprcas, inum, irc

!  Type indicates type of variable:
!    type = 1: double precission real
!    type = 2: integer
!    type = 3: single precission real
      inptnm = 0
    1 continue
      go to (10,20,30), type
      call srstop(6)
   10 if (stod(line,posit,80,dnum) .eq. 0) return
      go to 2
   20 if (stoi(line,posit,80,inum) .eq. 0) return
      go to 2
   30 if (stor(line,posit,80,rnum) .eq. 0) return
    2 continue
    5 read(inuni,100,end=3) line
  100 format (10a8)
      irc = uprcas (line,line,80)
      posit=1
      go to 1
    3 continue
      if (inuni.ne.5) go to 999
      rewind (inuni, err = 999)
      go to 5
 999  write (iou(6),1004) inuni
1004  format (' Hit end of file on unit ',I5)
      call srstop(6)
      end
