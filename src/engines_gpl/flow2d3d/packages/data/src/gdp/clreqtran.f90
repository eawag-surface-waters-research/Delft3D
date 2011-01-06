subroutine clreqtran(istat     ,gdp       )
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer,        dimension(:)   , pointer :: dll_handle_settle
    integer,        dimension(:)   , pointer :: dll_handle
!
! Global variables
!
    integer,intent(out) :: istat
!
! Local variables
!
    integer           :: i
    integer, external :: close_shared_library
    integer           :: error
!
!! executable statements -------------------------------------------------------
!
    dll_handle_settle    => gdp%gdeqtran%dll_handle_settle
    dll_handle    => gdp%gdeqtran%dll_handle
    !
    do i = 1,size(dll_handle)
       error = close_shared_library(dll_handle_settle(i))
       error = close_shared_library(dll_handle(i))
    enddo
    !
    if (associated(gdp%gdeqtran%dll_function_settle)) deallocate(gdp%gdeqtran%dll_function_settle, STAT = istat)
    if (associated(gdp%gdeqtran%dll_name_settle    )) deallocate(gdp%gdeqtran%dll_name_settle    , STAT = istat)
    if (associated(gdp%gdeqtran%dll_handle_settle  )) deallocate(gdp%gdeqtran%dll_handle_settle  , STAT = istat)
    if (associated(gdp%gdeqtran%dll_integers_settle)) deallocate(gdp%gdeqtran%dll_integers_settle, STAT = istat)
    if (associated(gdp%gdeqtran%dll_reals_settle   )) deallocate(gdp%gdeqtran%dll_reals_settle   , STAT = istat)
    if (associated(gdp%gdeqtran%dll_strings_settle )) deallocate(gdp%gdeqtran%dll_strings_settle , STAT = istat)
    if (associated(gdp%gdeqtran%dll_usrfil_settle  )) deallocate(gdp%gdeqtran%dll_usrfil_settle  , STAT = istat)
    !
    if (associated(gdp%gdeqtran%dll_function)) deallocate(gdp%gdeqtran%dll_function, STAT = istat)
    if (associated(gdp%gdeqtran%dll_handle  )) deallocate(gdp%gdeqtran%dll_handle  , STAT = istat)
    if (associated(gdp%gdeqtran%dll_integers)) deallocate(gdp%gdeqtran%dll_integers, STAT = istat)
    if (associated(gdp%gdeqtran%dll_reals   )) deallocate(gdp%gdeqtran%dll_reals   , STAT = istat)
    if (associated(gdp%gdeqtran%dll_strings )) deallocate(gdp%gdeqtran%dll_strings , STAT = istat)
    if (associated(gdp%gdeqtran%dll_usrfil  )) deallocate(gdp%gdeqtran%dll_usrfil  , STAT = istat)
    if (associated(gdp%gdeqtran%flstrn      )) deallocate(gdp%gdeqtran%flstrn      , STAT = istat)
    if (associated(gdp%gdeqtran%iform       )) deallocate(gdp%gdeqtran%iform       , STAT = istat)
    if (associated(gdp%gdeqtran%name        )) deallocate(gdp%gdeqtran%name        , STAT = istat)
    if (associated(gdp%gdeqtran%par         )) deallocate(gdp%gdeqtran%par         , STAT = istat)
end subroutine clreqtran
