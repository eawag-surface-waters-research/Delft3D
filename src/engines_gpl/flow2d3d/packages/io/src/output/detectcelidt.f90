subroutine detectcelidt(fds       ,grpnam    ,elmnam    ,itc       ,celidt    , &
                      & ierror    ,gdp       )
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
! Writes the time varying Dredge and Dump group to the NEFIS HIS-DAT file
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                          , intent(in)   :: fds          ! NEFIS file handle
    character(*)                     , intent(in)   :: grpnam       ! Data-group name for the NEFIS-file
    character(*)                     , intent(in)   :: elmnam       ! Element name for the NEFIS-file
    integer                          , intent(in)   :: itc          ! Time step to be written
    integer                          , intent(out)  :: ierror       ! Error flag for NEFIS files
    integer                          , intent(inout):: celidt       ! IN:  Last cell number
                                                                    ! OUT: Cell index to be used
!
! Local variables
!
    integer                           , external    :: getelt
!
    integer         , dimension(1)                  :: idummy       ! Help array to read/write Nefis files 
    integer                                         :: lastcl
    integer         , dimension(3,5)                :: uindex
!
!! executable statements -------------------------------------------------------
!
    !
    ! initialize group index time dependent data
    !
    ierror = 0
    uindex (1,1) = celidt ! start index
    uindex (2,1) = celidt ! end index
    uindex (3,1) = 1      ! increment in time
    !
    do while (celidt > 1)
       idummy(1)   = -1
       lastcl      = celidt - 1
       uindex(1,1) = lastcl
       uindex(2,1) = lastcl
       ierror     = getelt(fds, grpnam, elmnam, uindex, 1, 4, idummy)
       if (ierror/=0) return
       if (idummy(1)<itc) return
       celidt = lastcl
    enddo
    celidt = 1
end subroutine detectcelidt
