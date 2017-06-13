!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2017.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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

! $Id$
! $HeadURL$

!> \file
!! Basic API for gridgeom routines.

module gridgeom_api

use gridgeom
use iso_c_binding

implicit none

!-------------------------------------------------------------------------------
   contains
!-------------------------------------------------------------------------------

!> Gets the x,y coordinates from UGRID
function ggeo_get_xy_coordinates_dll(c_branchids, c_branchoffsets, c_geopointsX, c_geopointsY, c_nbranchgeometrynodes, c_branchlengths, c_meshXCoords, c_meshYCoords, nbranches, ngeopoints, nmeshnodes) result(ierr) bind(C, name="ggeo_get_xy_coordinates")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_get_xy_coordinates_dll

   integer(kind=c_int), intent(in)   :: nbranches, ngeopoints, nmeshnodes
   type(c_ptr), intent(in)           :: c_branchids
   type(c_ptr), intent(in)           :: c_branchoffsets, c_geopointsX, c_geopointsY, c_nbranchgeometrynodes, c_branchlengths
   type(c_ptr), intent(inout)        :: c_meshXCoords, c_meshYCoords
   
   !fortran pointers
   integer, pointer                  :: branchids(:), nbranchgeometrynodes(:)
   double precision, pointer         :: branchoffsets(:), geopointsX(:), geopointsY(:), branchlengths(:)
   double precision, pointer         :: meshXCoords(:), meshYCoords(:)
   
   integer                           :: ierr

   call c_f_pointer(c_branchids, branchids, (/ nmeshnodes /))
   call c_f_pointer(c_branchoffsets, branchoffsets, (/ nmeshnodes /))
   call c_f_pointer(c_geopointsX, geopointsX, (/ ngeopoints /))
   call c_f_pointer(c_geopointsY, geopointsY, (/ ngeopoints /))
   call c_f_pointer(c_nbranchgeometrynodes, nbranchgeometrynodes, (/ nbranches /))
   call c_f_pointer(c_branchlengths, branchlengths, (/ nbranches /))
   call c_f_pointer(c_meshXCoords, meshXCoords, (/ nmeshnodes /))
   call c_f_pointer(c_meshYCoords, meshYCoords, (/ nmeshnodes /))

   ierr = ggeo_get_xy_coordinates(branchids, branchoffsets, geopointsX, geopointsY, nbranchgeometrynodes, branchlengths, meshXCoords, meshYCoords)
   
end function ggeo_get_xy_coordinates_dll

end module gridgeom_api
