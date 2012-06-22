subroutine clrtrachy(istat     ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
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
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , dimension(:,:)   , pointer :: ittaru
    integer , dimension(:,:)   , pointer :: ittarv
    integer , dimension(:,:)   , pointer :: ittdef
    real(fp), dimension(:,:)   , pointer :: rgcalu
    real(fp), dimension(:,:)   , pointer :: rgcalv
    real(fp), dimension(:)     , pointer :: rttaru
    real(fp), dimension(:)     , pointer :: rttarv
    real(fp), dimension(:,:)   , pointer :: rttdef
    real(fp), dimension(:,:,:) , pointer :: rttfu
    real(fp), dimension(:,:,:) , pointer :: rttfv
    real(fp), dimension(:,:)   , pointer :: vegh2d
    real(fp), dimension(:,:)   , pointer :: vden2d 
    type (gd_trachy)           , pointer :: gdtrachy
!
! Global variables
!
    integer,intent(out) :: istat
!
!! executable statements -------------------------------------------------------
!
    ittaru        => gdp%gdtrachy%ittaru
    ittarv        => gdp%gdtrachy%ittarv
    ittdef        => gdp%gdtrachy%ittdef
    rgcalu        => gdp%gdtrachy%rgcalu
    rgcalv        => gdp%gdtrachy%rgcalv
    rttaru        => gdp%gdtrachy%rttaru
    rttarv        => gdp%gdtrachy%rttarv
    rttdef        => gdp%gdtrachy%rttdef
    rttfu         => gdp%gdtrachy%rttfu
    rttfv         => gdp%gdtrachy%rttfv
    vegh2d        => gdp%gdtrachy%vegh2d
    vden2d        => gdp%gdtrachy%vden2d
    gdtrachy      => gdp%gdtrachy
    !
    if (associated(gdtrachy%ittaru)) deallocate (gdtrachy%ittaru, STAT = istat)
    if (associated(gdtrachy%ittarv)) deallocate (gdtrachy%ittarv, STAT = istat)
    if (associated(gdtrachy%ittdef)) deallocate (gdtrachy%ittdef, STAT = istat)
    !
    if (associated(gdtrachy%vegh2d)) deallocate (gdtrachy%vegh2d, STAT = istat)
    if (associated(gdtrachy%vden2d)) deallocate (gdtrachy%vden2d, STAT = istat)
    !
    if (associated(gdtrachy%rgcalu)) deallocate (gdtrachy%rgcalu, STAT = istat)
    if (associated(gdtrachy%rgcalv)) deallocate (gdtrachy%rgcalv, STAT = istat)
    if (associated(gdtrachy%rttaru)) deallocate (gdtrachy%rttaru, STAT = istat)
    if (associated(gdtrachy%rttarv)) deallocate (gdtrachy%rttarv, STAT = istat)
    if (associated(gdtrachy%rttdef)) deallocate (gdtrachy%rttdef, STAT = istat)
    if (associated(gdtrachy%rttfu))  deallocate (gdtrachy%rttfu , STAT = istat)
    if (associated(gdtrachy%rttfv))  deallocate (gdtrachy%rttfv , STAT = istat)
end subroutine clrtrachy
