subroutine initveg3d(gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                
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
    integer                              , pointer :: itplant
    integer                              , pointer :: nveg
    real(fp)                             , pointer :: clplant
    character(256)                       , pointer :: filvg3d
!
!! executable statements -------------------------------------------------------
!
    itplant    => gdp%gdveg3d%itplant
    nveg       => gdp%gdveg3d%nveg
    clplant    => gdp%gdveg3d%clplant
    filvg3d    => gdp%gdveg3d%filvg3d
    !
    itplant   = 0
    nveg      = 0
    nullify(gdp%gdveg3d%planttype)
    nullify(gdp%gdveg3d%nplants)
    clplant   = 0.0
    filvg3d   = ' '
    nullify(gdp%gdveg3d%vegs)
end subroutine initveg3d
