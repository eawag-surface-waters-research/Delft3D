!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2020.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! $Id$
! $HeadURL$

!> Module for storing the optional hydrology state variables
module m_hydrology_data
   
   implicit none
   
   !
   ! Constants
   !
   integer, parameter :: DFM_HYD_NOINFILT      = 0 !< No infiltration active.
   ! NOTE: UNST-3763:        infiltrationmodel = 1 !< will soon be refactored, is actually interception.
   integer, parameter :: DFM_HYD_INFILT_CONST  = 2 !< Maximum (constant) infiltration capacity prescribed.
   integer, parameter :: DFM_HYD_INFILT_DARCY  = 3 !< Function of pressure.
   integer, parameter :: DFM_HYD_INFILT_HORTON = 4 !< Horton's infiltration equation.

   integer :: jadhyd !< Whether or not (1/0) external hydrology processes are enabled.

   ! Some hydrology state vars maintained in FM:
   !
   ! Precipitation
   !
   double precision, allocatable, target :: Precipitation(:) 
   integer                               :: precipitationTarget

   !
   ! Evaporation
   !
   double precision, allocatable, target :: PotEvap(:)     !< [m/s] Potential evaporation {"location": "face", "shape": ["ndx"]}
   double precision, allocatable, target :: ActEvap(:)     !< [m/s] Actual evaporation {"location": "face", "shape": ["ndx"]}
   integer                               :: potEvapTarget

   !
   ! Infiltration
   !
   integer                               :: infiltrationmodel      !< Infiltration formula, one of DFM_HYD_NOINFLT, DFM_HYD_INFILT_(CONST|DARCY|HORTON).

   double precision                      :: infiltcapuni           !< [m s-1] Uniform infiltration capacity. Only used if infiltrationmodel == 2 (DFM_HYD_INFILT_CONST).
   double precision, allocatable, target :: infilt(:)              !< [m3 s-1] Actual infiltration flux at current time {"location": "face", "shape": ["ndx"]}
   double precision, allocatable, target :: infiltcap(:)           !< [m s-1] Maximum infiltration capacity on each cell {"location": "face", "shape": ["ndx"]}
   double precision, allocatable         :: infiltcaproofs(:)      !< temporary of the same

   double precision, allocatable, target :: HortonMinInfCap(:)     !< [mm/hr] Minimum infiltration capacity in Horton's equation {"location": "face", "shape": ["ndx"]}
   double precision, allocatable, target :: HortonMaxInfCap(:)     !< [mm/hr] Maximum infiltration capacity in Horton's equation {"location": "face", "shape": ["ndx"]}
   double precision, allocatable, target :: HortonDecreaseRate(:)  !< [1/hr]  Decrease rate in Horton's equation {"location": "face", "shape": ["ndx"]}
   double precision, allocatable, target :: HortonRecoveryRate(:)  !< [1/hr]  Recovery rate in Horton's equation {"location": "face", "shape": ["ndx"]}

   !
   ! dhydrology state
   !
   double precision, allocatable, target :: CanopyGapFraction(:) 
   double precision, allocatable, target :: Cmax(:) 
   double precision, allocatable, target :: CanopyStorage(:) 
   double precision, allocatable, target :: NetInterception(:) 
   double precision, allocatable, target :: ThroughFall(:)    
   double precision, allocatable, target :: StemFlow(:) 
   double precision, allocatable, target :: LeftOver(:) 
   double precision, allocatable, target :: Interception(:) 

contains

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_hydrology_data() instead.
subroutine default_hydrology_data()
   jadhyd            = 0  !< Whether or not (1/0) external hydrology processes are enabled.

   infiltrationmodel = DFM_HYD_NOINFILT  !< Infiltration formula, one of DFM_HYD_NOINFLT, DFM_HYD_INFILT_(CONST|DARCY|HORTON).
   infiltcapuni      = 0d0               !< [m s-1] Uniform infiltration capacity. Only used if infiltrationmodel == 2 (DFM_HYD_INFILT_CONST).

   call reset_hydrology_data()
end subroutine default_hydrology_data


!> Resets only hydrology_data variables intended for a restart of an existing flow simulation (same MDU).
!! Upon loading of new model/MDU, call default_hydrology_data() instead.
subroutine reset_hydrology_data()
end subroutine reset_hydrology_data

end module m_hydrology_data
