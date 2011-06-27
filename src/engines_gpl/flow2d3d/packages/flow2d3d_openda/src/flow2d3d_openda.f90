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
!
!-------------------------------------------------------------------------------
!
! Running:
!  SE_SetElementSet
!  SE_GetCurrentTime
!  SE_GetValuesElementCount
!  SE_GetValues
!  SE_SetDiscreteInputTimes
!  SE_GetInputTimeCount
!  SE_GetInputTimes
!  SE_SetValuesAtTime
!  SE_PerformTimeStep 
!
! Finishing:
!  SE_Finalize
!
!!--pseudo code and references--------------------------------------------------
! NONE
!
! The DLL can only be used for single domain calculations
! On this top level, module gdp_entry is used to:
! - define the (unique) gdp pointer (with SAVE attribute)
! - define the (unique) odfData pointer corresponding with the unique 
!   meteodata%odfData (with SAVE attribute)
! - allocate/deallocate subroutines, to be called from the first/last 
!   wrapper-called DLL-subroutines 
! - getOdfData, assuming that the gdp has been defined and containing the runid
!
!
!======================================================================
!
function Initialize_openda(componentID, schemID) result(retVal)
    !
    implicit none
    !
    ! result
    integer :: retVal              ! retVal == 0 : success
    !
    ! externals
    integer, external :: Initialize
    !
    ! arguments
    character(*), intent(in) :: componentID  ! RR, RTC, etc.
    character(*), intent(in) :: schemID      ! schem. file (*.fnm)
    !
    ! local variables
    character(256) :: version_full ! by calling getfullversionstring_flow2d3d_openda, the version number is visible with the what command
    !
    ! body
    !
    call getfullversionstring_flow2d3d_openda(version_full)
    !
    ! initialize the FLOW2D3D part
    !
    retval = Initialize(componentID, schemID)
    !
    call allocate_d3d_states(1)
    !
end function Initialize_openda
!
!==============================================================================
!
function Finalize_openda(componentID, schemID) result(retVal)
    use m_d3dstate_2_openda
    !    
    implicit none
    !
    integer, external :: Finalize
    !
    ! result
    integer     :: retVal           ! retVal == 0 : success
    !
    ! arguments
    character(Len=*), intent(in) :: componentID  ! RR, RTC, etc.
    character(Len=*), intent(in) :: schemID      ! schem. file (*.fnm)
    !
    ! body
    !
    ! finalize the FLOW2D3D part
    !
    retval = Finalize(componentID, schemID)
    !
    ! deallocate the d3d_state. Should be somewhere in model_free.
    !
    call deallocate_d3d_states(1)
    !
    ! close alle netcdf-corestatefiles
    !
    call d3da_close_cta_state_files(retVal)
    !
    ! reset number of instances etc
    call d3da_reset_all()
    !                  
    ! TODO: costa finalize?
    !
end function Finalize_openda
