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
!   ******************************************************************
!   *    SUBROUTINE FOR DETERMING THE EUPHOPHIC DAYLENGTH AND THE    *
!   *                     EUPHOTIC ZONE                              *
!   ******************************************************************
!
      subroutine dayeu(day, dayeuf, exttot, dep, depeuf, dsol, euligh, idump)

      use bloom_data_io  

      implicit none
      
      real(8)    :: dsol
      real(8)    :: euligh
      real(8)    :: depeuf
      real(8)    :: exttot
      real(8)    :: dep
      real(8)    :: dayeuf
      real(8)    :: day
      integer    :: idump

! Compute the euphotic depth.
      if (euligh .gt. dsol) euligh = dsol
      depeuf=dlog(dsol/euligh)
      depeuf=depeuf/exttot

! Check whether the euphotic depth exceeds the physical depth. If it
! does, set depeuf = dep.
      if (depeuf .gt. dep) depeuf=dep

! Compute the euphotic day length.
      dayeuf=day * (depeuf/dep)

! Print the euphotic day length and depth.
      if (idump .eq. 1) write(ouuni,10) day, dayeuf, dep, depeuf
   10 format (' Day length = ',F5.2,' Euphotic day length = ',F5.2,' Depth = ',F5.2,' Euphotic depth = ',F5.2)
      return
      end
