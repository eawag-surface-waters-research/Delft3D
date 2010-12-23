function newlun_nogdp( )
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation; either                 
!  version 2.1 of the License.                                                  
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!!--description-----------------------------------------------------------------
!
!    Function: This routine gets an available unit specifier. It
!              returns an error if it didn't succeed.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
! Global variables
!
    integer         :: newlun_nogdp
                                   !!  Ineteger function used to attach
                                   !!  a new unit number to a var. LUN...
!
!
! Local variables
!
    integer                        :: iexit                ! Exit return value 
    integer                        :: lunit                ! Help var. 
    logical                        :: opened               ! Logical flag = TRUE if the test file is already opened 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    call psemlun
    lunit = 31
    opened = .true.
    !
    !-----get unit specifier
    !
    !-->
    !
   10 continue
    if (opened .and. lunit<999) then
       lunit = lunit + 1
       inquire (unit = lunit, opened = opened)
       goto 10
    !
    ! <--
    !
    endif
    !
    !-----test if unit number is available
    !
    if (opened) then
       newlun_nogdp = 0
       write (*, *) ' *** FATAL ERROR - New unit number not available'
       write (*, *) ' Abnormal end'
       !
       !--------stop routine for DELFT3D
       !
       iexit = 2
    else
       newlun_nogdp = lunit
    endif
    call vsemlun
end function newlun_nogdp
