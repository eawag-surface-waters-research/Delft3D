subroutine inittrachy(gdp       )
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
    logical                    , pointer :: flsedprop_rqrd
    type (gd_trachy)           , pointer :: gdtrachy
!
! Global variables
!
!! executable statements -------------------------------------------------------
!
    ! Initialize statics for trachy
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
    flsedprop_rqrd=> gdp%gdtrachy%flsedprop_rqrd
    gdtrachy      => gdp%gdtrachy
    !
    nullify(gdp%gdtrachy%ittaru)
    nullify(gdp%gdtrachy%ittarv)
    nullify(gdp%gdtrachy%ittdef)
    !
    nullify(gdp%gdtrachy%rgcalu)
    nullify(gdp%gdtrachy%rgcalv)
    nullify(gdp%gdtrachy%rttaru)
    nullify(gdp%gdtrachy%rttarv)
    nullify(gdp%gdtrachy%rttdef)
    nullify(gdp%gdtrachy%rttfu)
    nullify(gdp%gdtrachy%rttfv)
    !
    flsedprop_rqrd = .false. 
end subroutine inittrachy
