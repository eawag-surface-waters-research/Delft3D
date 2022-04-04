!!  Copyright (C)  Stichting Deltares, 2012-2022.
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

      real function t_forcing(itime,iseg)

!     read and evaluates temperatures forcing from file

      implicit none

!     Arguments

      integer                      :: itime
      integer                      :: iseg

      save

!     Locals

      integer                      :: ifirst = 1
      integer                      :: notime
      integer                      :: noseg
      integer                      :: it, it1, it2
      integer, allocatable         :: t_time(:)
      real   , allocatable         :: temperature(:)
      character(len=256)           :: t_file
      integer                      :: io_err

      if ( ifirst .eq. 1 ) then
         ifirst = 0
         open(849,file='t_forcing.dat')
         read(849,*) notime
         if ( notime .eq. -2 ) then
            ! binary temperature file
            read(849,*) noseg
            read(849,*) t_file
            allocate(temperature(noseg))
            open(848,file=t_file,form='binary')
            read(848) it1,temperature
            read(848) it2
         else
            allocate(t_time(notime))
            allocate(temperature(notime))
            do it = 1, notime
               read(849,*) t_time(it),temperature(it)
            enddo
            it = 1
         endif
      endif

      if ( notime .eq. -2 ) then
         do
            if ( itime .ge. it2 ) then
               it1 = it2
               read(848) temperature
               read(848,iostat=io_err) it2
            else
               exit
            endif
         enddo
         t_forcing = temperature(iseg)
      else
         if ( it .ne. notime ) then
            do
               if ( itime .lt. t_time(it+1) ) exit
               it = it + 1
            enddo
         endif
         t_forcing = temperature(it)
      endif

      return
      end
