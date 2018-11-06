function openexistingfile_meteo(minp, filename, meteotype) result(success)
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2018.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
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
!  $Id$
!  $HeadURL$
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use meteo_data
    implicit none
!
! Global variables
!
    integer         :: minp
    integer         :: meteotype
    logical         :: success
    character(*)    :: filename
!
! Local variables
!
    integer :: i
    logical :: unitused
!
!! executable statements -------------------------------------------------------
!
    if (len_trim(filename) == 0) then
       write (meteomessage, '(a,i0)') 'While opening meteo file: name is empty, for meteotype = ', meteotype
       success = .false.
       return
    endif
    inquire (file = trim(filename), exist = success)
    if (.not. success) then
       write(meteomessage,'(3a)') 'Meteo file ',trim(filename),' does not exist'
       success = .false.
       return
    endif
    do i = 32, 200
       inquire (unit = i, opened = unitused) 
       if (.not. unitused) exit
    enddo
    if (unitused) then
       meteomessage = 'No free unit number available for opening file'
       success = .false.
       return
    endif
    minp = i
    open (minp, file = trim(filename), action = 'READ')
    success = .true.
end function openexistingfile_meteo
