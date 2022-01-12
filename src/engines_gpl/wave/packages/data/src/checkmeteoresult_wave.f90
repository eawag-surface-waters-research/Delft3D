subroutine checkmeteoresult_wave(success)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2022.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id$
!  $HeadURL$
!!--description-----------------------------------------------------------------
!
!    Function: Handle the occurence of an error generated in the meteo module
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use meteo
    !
    implicit none
    !
!
! Global variables
!
    logical        :: success
!
! Local variables
!
    character(500) :: message
!
!! executable statements -------------------------------------------------------
!
    message = getmeteomessage()
    if (.not. success) then
       !
       ! Error occurred: write error and stop
       !
       call wavestop(1, 'Error in meteo module: ' // trim(message))
    elseif (message /= '') then
       !
       ! Message or warning issued by meteo module: write message
       ! No error: continue
       !
       write(*, '(2a)') 'Message generated by meteo module: ', trim(message)
    endif
end subroutine checkmeteoresult_wave
