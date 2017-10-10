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

use iso_c_binding

implicit none

!-------------------------------------------------------------------------------
   contains
!-------------------------------------------------------------------------------

!> Gets the x,y coordinates from UGRID
function ggeo_get_xy_coordinates_dll(c_branchids, c_branchoffsets, c_geopointsX, c_geopointsY, c_nbranchgeometrynodes, c_branchlengths, c_meshXCoords, c_meshYCoords, nbranches, ngeopoints, nmeshnodes) result(ierr) bind(C, name="ggeo_get_xy_coordinates")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_get_xy_coordinates_dll
   
   use gridgeom
   use meshdata
   
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

function ggeo_convert_dll(c_meshgeom, c_meshgeomdim) result(ierr) bind(C, name="ggeo_convert")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_convert_dll

   use gridoperations
   use gridgeom
   use meshdata
   
   type(c_t_ug_meshgeom), intent(in)      :: c_meshgeom
   type(c_t_ug_meshgeomdim), intent(in)   :: c_meshgeomdim
   type(t_ug_meshgeom)                    :: meshgeom
   integer                                :: ierr
   
   ierr = convert_cptr_to_meshgeom(c_meshgeom, c_meshgeomdim, meshgeom)
   ierr = ggeo_convert(meshgeom)
   
end function ggeo_convert_dll

function ggeo_make1D2Dinternalnetlinks_dll() result(ierr) bind(C, name="ggeo_make1D2Dinternalnetlinks")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_make1D2Dinternalnetlinks_dll
   use gridgeom
   integer :: ierr
   ierr = ggeo_make1D2Dinternalnetlinks()
   
end function ggeo_make1D2Dinternalnetlinks_dll


function ggeo_get_links_count_dll(nlinks) result(ierr) bind(C, name="ggeo_get_links_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_get_links_count_dll
   
   use gridoperations
   
   integer(kind=c_int), intent(inout):: nlinks
   integer :: ierr
   
   ierr =  ggeo_get_links_count(nlinks)
   
end function ggeo_get_links_count_dll


function ggeo_get_links_dll(c_arrayfrom, c_arrayto, nlinks) result(ierr) bind(C, name="ggeo_get_links")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_get_links_dll
   use gridoperations
   
   type(c_ptr), intent(in)                  :: c_arrayfrom, c_arrayto
   integer(kind=c_int), intent(in)          :: nlinks
   integer, pointer                         :: arrayfrom(:), arrayto(:)
   integer                                  :: ierr
   
   call c_f_pointer(c_arrayfrom, arrayfrom, (/ nlinks /))
   call c_f_pointer(c_arrayto, arrayto, (/ nlinks /))
   
   ierr = ggeo_get_links(arrayfrom, arrayto)
   
end function ggeo_get_links_dll

function ggeo_convert_1d_arrays_dll(c_nodex, c_nodey, c_branchoffset, c_branchlength, c_branchid, c_sourceNodeId, c_targetNodeId, nbranches, nmeshnodes) result(ierr) bind(C, name="ggeo_convert_1d_arrays")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_convert_1d_arrays_dll

   use gridoperations
   use gridgeom
   use meshdata
   
   type(c_ptr), intent(in)                :: c_nodex, c_nodey, c_branchoffset, c_branchlength, c_branchid, c_sourceNodeId, c_targetNodeId   
   integer(kind=c_int), intent(in)        :: nmeshnodes, nBranches
   !fortran pointers
   double precision, pointer              :: nodex(:), nodey(:), branchoffset(:), branchlength(:)
   integer, pointer                       :: branchid(:), sourceNodeId(:), targetNodeId(:)
   integer                                :: ierr
   type(t_ug_meshgeom)                    :: meshgeom
   
   call c_f_pointer(c_nodex, nodex, (/ nmeshnodes /))
   call c_f_pointer(c_nodey, nodey, (/ nmeshnodes /))
   call c_f_pointer(c_branchoffset, branchoffset, (/ nmeshnodes /))
   call c_f_pointer(c_branchid, branchid, (/ nmeshnodes /))
   
   call c_f_pointer(c_branchlength, branchlength, (/ nBranches /))
   call c_f_pointer(c_sourceNodeId, sourceNodeId, (/ nBranches /))
   call c_f_pointer(c_targetNodeId, targetNodeId, (/ nBranches /))
      
   ierr =  ggeo_convert_1d_arrays(nodex, nodey, branchoffset, branchlength, branchid, sourcenodeid, targetnodeid, meshgeom)
    
   ierr = ggeo_convert(meshgeom)

end function ggeo_convert_1d_arrays_dll

function ggeo_create_edge_nodes_dll(c_branchoffset, c_branchlength, c_branchids, c_sourceNodeId, c_targetNodeId, c_edgenodes, nBranches, nNodes, nEdgeNodes) result(ierr) bind(C, name="ggeo_create_edge_nodes")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_create_edge_nodes_dll

   use gridoperations
   
   type(c_ptr), intent(in)   :: c_branchoffset, c_branchids, c_edgenodes, c_sourceNodeId, c_targetNodeId, c_branchlength    
   integer, intent(in)       :: nBranches, nNodes, nEdgeNodes
   double precision, pointer :: branchoffset(:), branchlength(:)
   integer, pointer          :: branchids(:), edgenodes(:,:), sourceNodeId(:), targetNodeId(:) 
   integer                   :: ierr

   

   call c_f_pointer(c_branchlength, branchlength, (/ nBranches /))
   
   call c_f_pointer(c_branchids, branchids, (/ nNodes /))
   call c_f_pointer(c_sourceNodeId, sourceNodeId, (/ nBranches /))
   call c_f_pointer(c_targetNodeId, targetNodeId, (/ nBranches /))
   call c_f_pointer(c_edgenodes, edgenodes, (/ 2, nEdgeNodes /))
   call c_f_pointer(c_branchoffset, branchoffset, (/ nNodes /))
   
   ierr = ggeo_create_edge_nodes(branchids, branchoffset, sourcenodeid, targetnodeid, edgenodes, branchlength)

end function ggeo_create_edge_nodes_dll

end module gridgeom_api
