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

      real function s_forcing(itime,iseg,s_prev)

!     read and evaluates salinity forcing from file

      implicit none

!     Arguments

      integer                      :: itime
      integer                      :: iseg
      real                         :: s_prev

      save

!     Locals

      integer, save                :: ifirst = 1
      integer, save                :: lun_forcing, lun_binforcing
      integer                      :: notime
      integer                      :: noseg
      integer                      :: it
      integer, save                :: it0, it1, it2
      integer, allocatable, save   :: t_time(:)
      real   , allocatable, save   :: salinity(:)
      real   , allocatable, save   :: salprev(:)
      character(len=256)           :: t_file
      integer                      :: io_err

      if ( ifirst .eq. 1 ) then
         ifirst = 0
         open(newunit=lun_forcing,file='s_forcing.dat')
         read(lun_forcing,*) notime
         if ( notime .eq. -2 ) then
            ! binary salinity file
            read(lun_forcing,*) noseg
            read(lun_forcing,*) t_file
            allocate(salinity(noseg))
            allocate(salprev(noseg))
            open(newunit=lun_binforcing,file=t_file, access='stream', form='unformatted')
            read(lun_binforcing) it1,salinity
            read(lun_binforcing) it2
         else
            allocate(t_time(notime))
            allocate(salinity(notime))
            do it = 1, notime
               read(lun_forcing,*) t_time(it),salinity(it)
            enddo
            salprev = salinity
            it = 1
         endif
      endif

      if ( notime .eq. -2 ) then
         do
            if ( itime .ge. it2 ) then
               it1 = it2
               salprev = salinity
               read(lun_binforcing) salinity
               read(lun_binforcing,iostat=io_err) it2
            else
               exit
            endif
         enddo
         s_prev    = salprev(iseg)
         s_forcing = salinity(iseg)
      else
         if ( it .ne. notime ) then
            do
               if ( itime .lt. t_time(it+1) ) exit
               it = it + 1
            enddo
         endif
         it0 = max(1,it-1)
         s_prev    = salinity(it0)
         s_forcing = salinity(it)
      endif

      return
      end
