subroutine initheat(gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2023.                                
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
!  
!  
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
!! executable statements -------------------------------------------------------
!
    nullify(gdp%gdheat%secchi)
    nullify(gdp%gdheat%rhumarr)
    nullify(gdp%gdheat%tairarr)
    nullify(gdp%gdheat%clouarr)
    nullify(gdp%gdheat%swrfarr)
    nullify(gdp%gdheat%qeva_out)
    nullify(gdp%gdheat%qco_out)
    nullify(gdp%gdheat%qbl_out)
    nullify(gdp%gdheat%qin_out)
    nullify(gdp%gdheat%qnet_out)
    nullify(gdp%gdheat%hlc_out)
    nullify(gdp%gdheat%hfree_out)
    nullify(gdp%gdheat%efree_out)
    nullify(gdp%gdheat%qmis_out)
    nullify(gdp%gdheat%flbcktemp)
end subroutine initheat
