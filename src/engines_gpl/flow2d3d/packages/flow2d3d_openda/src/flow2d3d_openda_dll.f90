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
!  $Id: $
!  $HeadURL: $
!
!======================================================================
!
subroutine SE_Set_Max_Instances_In_Memory(max_instances)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Set_Max_Instances_In_Memory
    !
    use m_d3dstate_2_openda
    !
    implicit none
    !
    ! result
    integer :: max_instances  ! max #instances to be kept in memory
    !
    max_instances_in_memory = max_instances
    !
end subroutine SE_Set_Max_Instances_In_Memory
!
!======================================================================
!
function SE_Create_Instance() result(instance_id)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Create_Instance
    !
    use m_d3dstate_2_openda
    !
    implicit none
    !
    ! result
    integer :: instance_id  ! instance identifier (instanceId >= 0 : success)
    !
    instance_id = d3da_create_instance()
    !
end function SE_Create_Instance
!
!==============================================================================
!
subroutine SE_Select_Instance(instance_id)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Select_Instance
    !
    use m_d3dstate_2_openda
    !
    implicit none
    !
    ! arguments
    integer, intent(in) :: instance_id  ! instance identifier
    !
    call d3da_select_new_instance(instance_id)
    !
end subroutine SE_Select_Instance
!
!==============================================================================
!
subroutine SE_Select_Instance_from_Restartfile(instance_id, filename)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Select_Instance_from_Restartfile
    !
    use m_d3dstate_2_openda
    !
    implicit none
    !
    ! arguments
    integer         , intent(in) :: instance_id  ! instance identifier
    character(Len=*), intent(in) :: filename
    !
    call d3da_select_instance_from_restartfile(instance_id, filename)
    !
end subroutine SE_Select_Instance_from_Restartfile
!
!==============================================================================
!
function SE_Store_Current_Instance(storage_level) result (retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Store_Current_Instance
    !
    use m_d3dstate_2_openda
    !
    implicit none
    !
    ! argument
    integer :: storage_level 
    !    
    ! result
    integer :: retVal
    !
    call d3da_store_current_instance(storage_level,'default')
    retVal = 0
    !
end function SE_Store_Current_Instance
!
!==============================================================================
!
function SE_Store_Current_Instance_Restartfile(filename) result (retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Store_Current_Instance_Restartfile
    !
    use m_d3dstate_2_openda
    !
    implicit none
    !
    ! arguments
    character(Len=*), intent(in) :: filename    
    ! result
    integer :: retVal
    !
    call d3da_store_current_instance_restartfile(filename)
    retVal = 0
    !
end function SE_Store_Current_Instance_Restartfile
!
!==============================================================================
!
function SE_Get_Instance_Core_State(corestate, size_corestate) result (retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Get_Instance_Core_State
    !
    use m_d3dstate_2_openda
    !
    implicit none
    !
    ! result
    integer :: retVal
    !
    integer                                       :: size_corestate
    double precision, dimension(size_corestate)   :: corestate
    !
    call d3da_getcorestate(corestate,size_corestate,retval)
    !
end function  SE_Get_Instance_Core_State
!
!==============================================================================
!
function SE_Set_Instance_Core_State(corestate,size_corestate) result (retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Set_Instance_Core_State
    !
    use m_d3dstate_2_openda
    !
    implicit none
    !
    ! result
    integer :: retVal
    !
    integer                                                  :: size_corestate
    double precision, dimension(size_corestate), intent(in)  :: corestate
    !
    call d3da_setcorestate(corestate,size_corestate,retVal)
    !
end function  SE_Set_Instance_Core_State
!
!==============================================================================
!
function SE_Get_Instance_Size() result (inst_size)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Get_Instance_Size
    !
    use m_d3dstate_2_openda
    !
    implicit none
    !
    ! result
    integer :: inst_size
    !
    call d3da_getinstancesize(inst_size)
    !
end function  SE_Get_Instance_Size
!
!==============================================================================
!
subroutine SE_Export_Current_Instance(doappend)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Export_Current_Instance
    ! 
    use m_d3dstate_2_openda
    ! 
    integer :: ierr
    logical :: doappend
    !
    ! to do: export in append mode
    !
    call d3da_ctastate_to_netcdf(ierr)
    !
end subroutine SE_Export_Current_Instance
!
!==============================================================================
!
function SE_Initialize_openda(componentID, schemID) result(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Initialize_openda
    !
    implicit none
    !
    ! result
    integer :: retVal              ! retVal == 0 : success
    !
    ! externals
    integer, external :: Initialize_openda
    !
    ! arguments
    character(*), intent(in) :: componentID  ! RR, RTC, etc.
    character(*), intent(in) :: schemID      ! schem. file (*.fnm)
    !
    ! body
    !
    retval = Initialize_openda(componentID, schemID)
    !
end function SE_Initialize_openda
!
!==============================================================================
!
function SE_Finalize_openda(componentID, schemID) result(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Finalize_openda
    !
    use m_d3dstate_2_openda
    !    
    implicit none
    !
    ! result
    integer     :: retVal           ! retVal == 0 : success
    !
    ! externals
    integer, external :: Finalize_openda
    !
    ! arguments
    character(Len=*), intent(in) :: componentID  ! RR, RTC, etc.
    character(Len=*), intent(in) :: schemID      ! schem. file (*.fnm)
    !
    ! body
    !
    retval = Finalize_openda(componentID, schemID)
    !
end function SE_Finalize_openda
!
!==============================================================================
!
subroutine define_ordinary_state(imode)
    !DEC$ ATTRIBUTES DLLEXPORT :: define_ordinary_state
    !
    implicit none
    !    
    integer :: imode
    !
    call d3da_define_ordinary_state(imode)
    !
end subroutine define_ordinary_state
