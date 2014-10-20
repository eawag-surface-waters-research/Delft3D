!!  Copyright (C)  Stichting Deltares, 2012-2014.
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

module noextspaces_mod
use precision
      use timers
!
!  module declarations
!
implicit none  ! force explicit typing
!
contains
subroutine noextspaces(name      ,length    )
!!--copyright-------------------------------------------------------------------
! Copyright (c) 2003, Deltares. All rights reserved.
!!--disclaimer------------------------------------------------------------------
! This code is part of the Delft3D software system. Deltares has
! developed c.q. manufactured this code to its best ability and according to the
! state of the art. Nevertheless, there is no express or implied warranty as to
! this software whether tangible or intangible. In particular, there is no
! express or implied warranty as to the fitness for a particular purpose of this
! software, whether tangible or intangible. The intellectual property rights
! related to this software code remain with Deltares at all times.
! For details on the licensing agreement, we refer to the Delft3D software
! license and any modifications to this license, if applicable. These documents
! are available upon request.
!!--version information---------------------------------------------------------
! $Author$
! $Date$
! $Revision$
!!--description-----------------------------------------------------------------
!
!    Function: Removes the spaces at the front and the
!              end of the name. Also returns the length of the
!              cleaned name.
!              WARNING: do not call this subroutine with a
!              constant character string
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer          :: length
                                   !!  Length of cleaned name
    character(len=*) :: name
                                   !!  Name to be cleaned
!
!
! Local variables
!
    integer(ip)                    :: begpos
    integer(ip)                    :: endpos
    integer(ip)                    :: pos
    character(len=1)               :: achar
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "noextspaces", ithndl )
!
!! executable statements -------------------------------------------------------
!
    !
    !-----initialization
    !
    begpos = 1
    endpos = len(name)
    if (endpos==0) then
       length = 0
       goto 999
    endif
    !
    !-----skip backside spaces
    !
  100 continue
    achar = name(endpos:endpos)
    if (achar==' ') then
       endpos = endpos - 1
       if (endpos>0) goto 100
       length = 0
       goto 999
    endif
    !
    !-----skip frontside spaces
    !
  200 continue
    achar = name(begpos:begpos)
    if ((achar==' ') .and. (begpos<endpos)) then
       begpos = begpos + 1
       goto 200
    endif
    !
    !-----adjust output
    !
    length = endpos - begpos + 1
    name(1:length) = name(begpos:endpos)
    do pos = length + 1, len(name)
       name(pos:pos) = ' '
    enddo
    !
  999 continue
      if ( timon ) call timstop ( ithndl )
end subroutine noextspaces
end module
