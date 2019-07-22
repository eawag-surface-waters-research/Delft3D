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

!
! Integer function to obtain the length of a character string.
! The non-significant characters are initially set to a space and
! a comma, but can be reset by function SETDLM.
!
      integer function lenstr (source,maxlen)

      implicit none

      character*1 source(*)
      character*255 result
      integer gets, pos, i, lenout, maxlen
!
      pos = 1
      do 10 i = 1,maxlen
      if (gets(source,pos,maxlen,255,result,lenout) .ne. 0) go to 20
10    continue
20    continue
      lenstr = pos - 1
      return
      end
