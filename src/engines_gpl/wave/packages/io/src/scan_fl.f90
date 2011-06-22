subroutine scan_fl(checkVersionNumber, versionNumberOK)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
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
! SCANFL scans the file "hiswa.out" for a string "SEVERE" or
! or the Swan file "PRINT" for a string "ERROR"
! If found, the program will be stopped.
! Errors related to the grid are converted into a warning.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    logical, intent(in) :: checkVersionNumber
    character(*)        :: versionNumberOK
!
! Local variables
!
    integer           :: k
    integer           :: k2
    integer           :: gridwarn
    integer           :: verLen
    integer           :: uh
    character(20)     :: versionNumber
    character(80)     :: line
!
! Functions called
!
    integer, external :: new_lun
!
!! executable statements -------------------------------------------------------
!
    verLen = len_trim(versionNumberOK)
    uh = new_lun()
    open (uh, file = 'PRINT', form = 'FORMATTED')
    gridwarn = 0
100 continue
       read (uh, '(A)', end = 200) line
       call small(line, 80)
       k   = index(line, 'severe')
       if (k > 0) then
          write (*, '(a)') '*** ERROR: SWAN file PRINT contains SEVERE errors'
          close (uh)
          stop
       endif
       k = index(line, 'error')
       if (k > 0) then
          k2 = index(line, 'grid')
          if (k2 > 0) then
             gridwarn = gridwarn + 1
          else
             write (*, '(a)') '*** ERROR: SWAN file PRINT contains ERRORS'
             close (uh)
             stop
          endif
       endif
       if (checkVersionNumber) then
          k = index(line, 'version number')
          if (k > 0) then
             versionNumber = line(k+15:k+14+verLen)
             if (versionNumber < versionNumberOK) then
                write (*, '(3a)') '*** ERROR: use SWAN version ',trim(versionNumberOK), ' or newer.'
                close (uh)
                stop
             endif
          endif
       endif
    goto 100
200 continue
    if (gridwarn > 0) then
       write (*, '(a,i4,a)') 'Warning: found ', gridwarn, &
          ' times a error about the grid. See PRINT file for details'
    endif
    close (uh)
end subroutine scan_fl
