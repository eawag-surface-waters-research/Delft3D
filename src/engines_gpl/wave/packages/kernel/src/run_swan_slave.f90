subroutine run_swan_slave (command, retval)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2021.                                
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
!     *** Run swan; produce output file swanout with values on     ***
!     *** swan computational grid                                  ***
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use wave_mpi
   implicit none
!
! Global variables
!
   integer, intent(out) :: command
   integer, intent(out) :: retval
!
! Local variables
!
   integer :: ierr
!
!! executable statements -----------------------------------------------
!
   call wave_mpi_bcast(command, ierr)
   if ( ierr /= MPI_SUCCESS ) then
      write (*,'(a,i5)') 'MPI produces some internal error - return code is ',ierr
      retval = -1
   else
      if (command == SWAN_GO) then
         call swan(engine_comm_world)
      endif
      retval = 0
   endif
end subroutine run_swan_slave