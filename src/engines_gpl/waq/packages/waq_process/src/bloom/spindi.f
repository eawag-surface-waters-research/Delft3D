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
!  *********************************************************************
!  *         SUBROUTINE SPINDI TO DETERMINE TYPE INDICES               *
!  *********************************************************************

      subroutine spindi(lspind)

      use bloom_data_dim
      use bloom_data_io  
      use bloom_data_phyt  

      implicit none

      integer  :: i, j, lspind, ngr

!  Determine the number of the first and last type of each species.
!  Check whether the number of different names agrees with the total
!  number of species which has previously been read by the program.
      lspind = 0
      ngr = 0
      j = 0
   5  i = j + 1
  10  j = j + 1
      if (j .gt. nuspec) go to 15
      if (spname(j) .eq. spname(i)) go to 10
  15  ngr = ngr + 1
      j=j-1
      it2(ngr,1)=i
      it2(ngr,2)=j
      grname (ngr) = spname (i)
      if (j .eq. nuspec) go to 20
      go to 5

!  Check the number of species.
  20  if (ngr .eq. nuecog) return
      lspind = 1
      write (iou(10),30) ngr,nuecog
  30  format (' The number of different species names ',I2,/,' is not',
     1        ' consistent with the number of species ',I2,/,' Execution terminates.')
      return
      end
