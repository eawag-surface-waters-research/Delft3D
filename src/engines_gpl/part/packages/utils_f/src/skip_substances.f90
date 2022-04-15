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

module skipsb_mod
!
!  module declarations
!
!
!  data definition module(s)
!
use precision             ! single and double precision
      use timers
!
!  module procedure(s)
!
use exception_part         ! explicit interface for subroutine calls
use m_part_modeltypes
!
implicit none             ! force explicit typing
!
contains
      subroutine skipsb (lun1  ,substi , nosubs, modtyp, lunlog)
!
!                        d e l p a r    v3.60
!
!     created               : november 2000, by antoon koster
!
!     function              : skip substances in general input file
!
!     logical unit numbers  : lun1   - stripped user input file
!
!     declarations
!
      character(len= 20),dimension(:)  :: substi
      character(len=256)               :: finnh4, finno3
!
!     local scalars
!
      integer(4) :: i , lun1, nosubs, modtyp, lunlog
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "skipsb", ithndl )
!
!     skip substance names
!
      read (lun1 , *,err=1090) (substi(i), i = 1, nosubs)
!
!     skip file names for red tide model (modtyp=model_red_tide)
!
      if(modtyp==model_red_tide) then
        do 71 i = 1, nosubs
             if(substi(i)(1:8)=='nh4_file') then
                read(lun1,*,err=1110)  finnh4
             endif
             if(substi(i)(1:8)=='no3_file') then
                read(lun1,*,err=1120)  finno3
             endif
71      continue
      endif
!
!     formats
!
      if ( timon ) call timstop ( ithndl )
      return
1090  write(lunlog,*) 'Error: names of substances not read correctly'
      call stop_exit(1)
1110  write(*,*) 'Error: filename of nh4-file not read correctly'
      call stop_exit(1)
1120  write(*,*) 'Error: filename of no3-file not read correctly'
      call stop_exit(1)
      return
      end subroutine
end module
