subroutine initscour(gdp       )
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
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
    integer                , pointer :: nof
    integer, dimension(:)  , pointer :: nmapp
    integer, dimension(:)  , pointer :: nmref
    logical                , pointer :: scour
    real(fp), dimension(:) , pointer :: factor
    real(fp)               , pointer :: slope
    real(fp), dimension(:) , pointer :: tauv
    type (gd_scour)        , pointer :: gdscour
!
!! executable statements -------------------------------------------------------
!
    nof        => gdp%gdscour%nof
    nmapp      => gdp%gdscour%nmapp
    nmref      => gdp%gdscour%nmref
    scour      => gdp%gdscour%scour
    factor     => gdp%gdscour%factor
    slope      => gdp%gdscour%slope
    tauv       => gdp%gdscour%tauv
    gdscour    => gdp%gdscour
    !
    nof     = 0
    nullify (gdp%gdscour%nmapp)
    nullify (gdp%gdscour%nmref)
    scour   = .false.
    nullify (gdp%gdscour%factor)
    slope   = 0.0
    nullify (gdp%gdscour%tauv)
end subroutine initscour
