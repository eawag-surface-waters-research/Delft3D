subroutine nefisio_dealloc(gdp       )
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
!!--description-----------------------------------------------------------------
!
! NONE
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
!
! Global variables
!
!
! Local variables
!
    integer                        :: i
    type (nefiselement) , pointer  ::nefiselem
!
!! executable statements -------------------------------------------------------
!
    do i = 1, nrnefisfiles
       nefiselem => gdp%nefisio%nefiselem(i)
       if (associated(nefiselem%elmdms)) deallocate (nefiselem%elmdms)
       if (associated(nefiselem%nbytsg)) deallocate (nefiselem%nbytsg)
       if (associated(nefiselem%elmunt)) deallocate (nefiselem%elmunt)
       if (associated(nefiselem%elmnms)) deallocate (nefiselem%elmnms)
       if (associated(nefiselem%elmqty)) deallocate (nefiselem%elmqty)
       if (associated(nefiselem%elmtps)) deallocate (nefiselem%elmtps)
       if (associated(nefiselem%elmdes)) deallocate (nefiselem%elmdes)
    enddo
    deallocate (gdp%nefisio)
end subroutine nefisio_dealloc
