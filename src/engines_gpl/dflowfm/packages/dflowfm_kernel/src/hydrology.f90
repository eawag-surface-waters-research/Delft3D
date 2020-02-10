!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017.                                     
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
!-------------------------------------------------------------------------------------------------------
!  Origin: 
!     URL: https://svn.oss.deltares.nl/repos/delft3d/trunk/src/engines_gpl/flow2d3d/packages/data/include/fourier.igs
!     Revision: 5108

!> Module fokor storing the optional hydrology state variables
module m_hydrology
   
   implicit none
   
   integer :: jadhyd !< Whether or not (1/0) external hydrology processes are enabled.

   ! Some hydrology state vars maintained in FM:
   double precision, allocatable, target :: Precipitation(:) 
   integer                               :: precipitationTarget
    
   double precision, allocatable, target :: PotEvap(:)    
   integer                               :: potEvapTarget
   
   double precision, allocatable, target :: CanopyGapFraction(:) 
   double precision, allocatable, target :: Cmax(:) 
   double precision, allocatable, target :: CanopyStorage(:) 
   double precision, allocatable, target :: NetInterception(:) 
   double precision, allocatable, target :: ThroughFall(:)    
   double precision, allocatable, target :: StemFlow(:) 
   double precision, allocatable, target :: LeftOver(:) 
   double precision, allocatable, target :: Interception(:) 

end module m_hydrology