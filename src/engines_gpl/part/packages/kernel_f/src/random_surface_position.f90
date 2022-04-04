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

module random_surface_position_mod
!
!  data definition module(s)
!
use precision_part          ! single/double precision
use timers
!
!  module procedure(s)
!
!

implicit none

contains
    subroutine random_surface_position ( lunrep, idelt, ztop, zdepth, vzact, buoy, vz )

        ! function  : Set a random surface position when jellyfish are within 12 meter of the surface.
        !              
        !             
        
        ! arguments :
        integer(ip), intent(in)    :: lunrep                  ! report file
        integer(ip)                :: idelt                   ! timestep in seconds
        real   (sp)                :: vz                      ! vz
        real   (sp)                :: ztop                    ! ztop
        real   (sp)                :: zdepth                  ! z relative to water surface
        real   (sp)                :: vzact                   ! vzact
        real   (sp)                :: buoy                    ! buoy
        
        real   (sp)                :: newdepth                ! z relative to water surface
        real   (sp)                :: move                    ! movement in z 
        real   (sp)                :: randomnbr               ! random number between 0.-1.
        
        !Check if the jellyfish is within 12 meters depth
        if(zdepth .le. 12.0) then
            !distribute jellyfish randomly in 12 meter layer
            call RANDOM_NUMBER(randomnbr)
            newdepth = 12.0 * randomnbr
            
            !make sure newdepth is never 0.0
            if(newdepth .lt. 0.1) then
                newdepth = 0.1
            endif
            
            !move jellyfish to the new depth or let it swim towards
            move = newdepth - zdepth                               !positive move is in the downward direction
            ! make sure that the random movement can't exceed the vertical travel distance
            ! else let the particle swim towards the position
            if(move .gt. ((buoy - vzact)*idelt)) then        !if moving down and more than traveldistance
                vz = buoy - vzact
            else if(move .lt. ((buoy + vzact)*idelt)) then   !if moving up and more than traveldistance
                vz = buoy - vzact
            else                                             !if moving is less than traveldistance
                vz = move/idelt                             
            endif
        endif
               
    return                                                                     	   !Return from the subroutine
    end subroutine
end module

