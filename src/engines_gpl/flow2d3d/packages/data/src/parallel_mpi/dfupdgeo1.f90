subroutine dfupdgeo1 ( guu, gvv, gdp )
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
!   exchange geometrical information, e.g. guu, gvu, etc. with neighbours
!
!!--pseudo code and references--------------------------------------------------
!
!   Marcel.Zijlema@wldelft.nl
!   01 mar 07
!
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use globaldata
    !
    implicit none
    !
    type(globdat), target    :: gdp
!
! Global variables
!
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(inout) :: guu    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(inout) :: gvv    !  Description and declaration in rjdim.f90
!
!! executable statements -------------------------------------------------------
!
    ! if not parallel, return
    !
    if (.not.parll) return
    !
    ! exchange geometrical information (obtained with inigeo) with neighbours
    !
    call dfexchg ( guu   , 1, 1, dfloat, gdp )
    call dfexchg ( gvv   , 1, 1, dfloat, gdp )
    !
end subroutine dfupdgeo1
