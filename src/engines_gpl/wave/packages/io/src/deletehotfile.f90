function deletehotfile(wavedata) result (dodelete)
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
!  $Id:  $
!  $HeadURL:  $
!!--description-----------------------------------------------------------------
!
!    Function: Return true if the hotfile with time hotfiletime can be deleted
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use precision
use wave_data
use swan_input
!
implicit none
!
! Global variables
!
logical        :: dodelete
type(wave_data_type) :: wavedata
!
! Local variables
!
integer  :: idate
integer  :: iday
integer  :: ihour
integer  :: imin
integer  :: imon
integer  :: isec
integer  :: itime
integer  :: iyear
integer  :: julday
real(hp) :: rkeeptime
real(hp) :: rusetime
!
!! executable statements -------------------------------------------------------
!
   ! Always delete the hotfile when
   ! - running standalone
   ! or
   ! - no "int2keephotfile" specified
   !
   if (wavedata%mode==stand_alone .or. comparereal(swan_run%int2keephotfile,0.0)<=0) then
      dodelete = .true.
      return
   endif
   read (swan_run%usehottime ,*) rusetime
   read (swan_run%keephottime,*) rkeeptime
   !
   ! increase rkeeptime until it is equal to or greater than rusetime
   !
   do while (comparereal(rkeeptime,rusetime) < 0)
      wavedata%output%timseckeephot = wavedata%output%timseckeephot + swan_run%int2keephotfile*60.0
      call juldat(wavedata%time%refdate, julday)
      call timdat(julday, wavedata%output%timseckeephot, idate, itime)
      iyear = idate/10000
      imon  = (idate - iyear*10000)/100
      iday  = idate - iyear*10000 - imon*100
      ihour = itime/10000
      imin  = (itime - ihour*10000)/100
      isec  = itime - ihour*10000 - imin*100
      write (swan_run%keephottime, '(I4,2I2.2,A1,3I2.2)') iyear, imon, iday, '.', ihour, imin, isec
      read (swan_run%keephottime,*) rkeeptime
   enddo
   if (comparereal(rkeeptime,rusetime) == 0) then
      dodelete = .false.
   else
      dodelete = .true.
   endif
end function deletehotfile
