subroutine clrwaqpar(istat, gdp)
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
    type (gd_waqpar)  , pointer :: gdwaqpar
!
! Global variables
!
    integer,intent(out) :: istat
!
!! executable statements -------------------------------------------------------
!
    gdwaqpar  => gdp%gdwaqpar
    !
    if (associated(gdwaqpar%quwaq))      deallocate(gdwaqpar%quwaq     , stat = istat)
    if (associated(gdwaqpar%qvwaq))      deallocate(gdwaqpar%qvwaq     , stat = istat)
    if (associated(gdwaqpar%qwwaq))      deallocate(gdwaqpar%qwwaq     , stat = istat)
    if (associated(gdwaqpar%discumwaq))  deallocate(gdwaqpar%discumwaq , stat = istat)
    if (associated(gdwaqpar%ifrmto))     deallocate(gdwaqpar%ifrmto    , stat = istat)
    if (associated(gdwaqpar%isaggr))     deallocate(gdwaqpar%isaggr    , stat = istat)
    if (associated(gdwaqpar%iqaggr))     deallocate(gdwaqpar%iqaggr    , stat = istat)
    if (associated(gdwaqpar%ilaggr))     deallocate(gdwaqpar%ilaggr    , stat = istat)
    if (associated(gdwaqpar%ifsmax))     deallocate(gdwaqpar%ifsmax    , stat = istat)
    if (associated(gdwaqpar%vol))        deallocate(gdwaqpar%vol       , stat = istat)
    if (associated(gdwaqpar%sag))        deallocate(gdwaqpar%sag       , stat = istat)
    if (associated(gdwaqpar%vol2))       deallocate(gdwaqpar%vol2      , stat = istat)
    if (associated(gdwaqpar%sag2))       deallocate(gdwaqpar%sag2      , stat = istat)
    if (associated(gdwaqpar%qag))        deallocate(gdwaqpar%qag       , stat = istat)
    if (associated(gdwaqpar%horsurf))    deallocate(gdwaqpar%horsurf   , stat = istat)
    if (associated(gdwaqpar%kmk))        deallocate(gdwaqpar%kmk       , stat = istat)
    if (associated(gdwaqpar%loads))      deallocate(gdwaqpar%loads     , stat = istat)
    if (associated(gdwaqpar%iwlk))       deallocate(gdwaqpar%iwlk      , stat = istat) 
    !
end subroutine clrwaqpar
