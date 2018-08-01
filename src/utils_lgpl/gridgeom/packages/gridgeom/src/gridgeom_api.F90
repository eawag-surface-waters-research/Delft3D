!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2018.                                
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
function ggeo_get_xy_coordinates_dll(c_branchids, c_branchoffsets, c_geopointsX, c_geopointsY, c_nbranchgeometrynodes, c_branchlengths, c_jsferic, c_meshXCoords, c_meshYCoords, nbranches, ngeopoints, nmeshnodes) result(ierr) bind(C, name="ggeo_get_xy_coordinates")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_get_xy_coordinates_dll
   
   use gridgeom
   use meshdata
   
   integer(kind=c_int), intent(in)   :: nbranches, ngeopoints, nmeshnodes, c_jsferic
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

   ierr = ggeo_get_xy_coordinates(branchids, branchoffsets, geopointsX, geopointsY, nbranchgeometrynodes, branchlengths, c_jsferic, meshXCoords, meshYCoords)
   
end function ggeo_get_xy_coordinates_dll

function ggeo_convert_dll(c_meshgeom, c_meshgeomdim, start_index) result(ierr) bind(C, name="ggeo_convert")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_convert_dll

   use gridoperations
   use gridgeom
   use meshdata
   
   type(c_t_ug_meshgeom), intent(in)      :: c_meshgeom
   type(c_t_ug_meshgeomdim), intent(in)   :: c_meshgeomdim
   type(t_ug_meshgeom)                    :: meshgeom
   integer, intent(in)                    :: start_index
   integer                                :: ierr
   
   ierr = convert_cptr_to_meshgeom(c_meshgeom, c_meshgeomdim, meshgeom)
   ierr = ggeo_convert(meshgeom, start_index)
   
end function ggeo_convert_dll

function ggeo_make1D2Dinternalnetlinks_dll(c_nin, c_xpl, c_ypl, c_zpl, c_jsferic, c_jasfer3D, c_jglobe) result(ierr) bind(C, name="ggeo_make1D2Dinternalnetlinks")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_make1D2Dinternalnetlinks_dll
   
   use gridgeom
   
   integer, intent(in)          :: c_nin
   type(c_ptr), intent(in)      :: c_xpl
   type(c_ptr), intent(in)      :: c_ypl
   type(c_ptr), intent(in)      :: c_zpl
   integer, intent(in)          :: c_jsferic
   integer, intent(in)          :: c_jasfer3D
   integer, intent(in)          :: c_jglobe
   integer                      :: ierr  
   double precision, pointer    :: xplLinks(:), yplLinks(:), zplLinks(:)   

   call c_f_pointer(c_xpl, xplLinks, (/c_nin/))
   call c_f_pointer(c_ypl, yplLinks, (/c_nin/))
   call c_f_pointer(c_zpl, zplLinks, (/c_nin/))
   
   ierr = ggeo_make1D2Dinternalnetlinks(xplLinks, yplLinks, zplLinks, c_jsferic, c_jasfer3D, c_jglobe)
   
end function ggeo_make1D2Dinternalnetlinks_dll

function ggeo_make1D2Droofgutterpipes_dll(c_nin, c_xpl, c_ypl, c_zpl, c_jsferic, c_jasfer3D, c_jglobe) result(ierr) bind(C, name="ggeo_make1D2Droofgutterpipes")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_make1D2Droofgutterpipes_dll
   
   use gridgeom
   
   integer, intent(in)          :: c_nin
   type(c_ptr), intent(in)      :: c_xpl
   type(c_ptr), intent(in)      :: c_ypl
   type(c_ptr), intent(in)      :: c_zpl
   integer, intent(in)          :: c_jsferic
   integer, intent(in)          :: c_jasfer3D
   integer, intent(in)          :: c_jglobe
   integer                      :: ierr  
   double precision, pointer    :: xplRoofs(:), yplRoofs(:), zplRoofs(:)   

   call c_f_pointer(c_xpl, xplRoofs, (/c_nin/))
   call c_f_pointer(c_ypl, yplRoofs, (/c_nin/))
   call c_f_pointer(c_zpl, zplRoofs, (/c_nin/))
   
   ierr = ggeo_make1D2Droofgutterpipes(xplRoofs, yplRoofs, zplRoofs, c_jsferic, c_jasfer3D, c_jglobe)
   
end function ggeo_make1D2Droofgutterpipes_dll

function ggeo_make1D2Dstreetinletpipes_dll(c_nin, c_xin, c_yin, c_jsferic, c_jasfer3D, c_jglobe) result(ierr) bind(C, name="ggeo_make1D2Dstreetinletpipes")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_make1D2Dstreetinletpipes_dll

   use gridgeom
   
   integer, intent(in)          :: c_nin
   type(c_ptr), intent(in)      :: c_xin
   type(c_ptr), intent(in)      :: c_yin
   integer, intent(in)          :: c_jsferic
   integer, intent(in)          :: c_jasfer3D
   integer, intent(in)          :: c_jglobe
   integer                      :: ierr  
   double precision, pointer    :: xsStreetInletPipes(:), ysStreetInletPipes(:)

   call c_f_pointer(c_xin, xsStreetInletPipes, (/c_nin/))
   call c_f_pointer(c_yin, ysStreetInletPipes, (/c_nin/))

   ierr = ggeo_make1D2Dstreetinletpipes(xsStreetInletPipes, ysStreetInletPipes, c_jsferic, c_jasfer3D, c_jglobe)

end function ggeo_make1D2Dstreetinletpipes_dll

function ggeo_get_links_count_dll(nlinks, linkType) result(ierr) bind(C, name="ggeo_get_links_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_get_links_count_dll
   
   use gridoperations
   
   integer(kind=c_int), intent(inout):: nlinks, linkType
   integer :: ierr
   
   ierr =  ggeo_get_links_count(nlinks, linkType)
   
end function ggeo_get_links_count_dll


function ggeo_get_links_dll(c_arrayfrom, c_arrayto, nlinks, linkType) result(ierr) bind(C, name="ggeo_get_links")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_get_links_dll
   use gridoperations
   
   type(c_ptr), intent(in)                  :: c_arrayfrom, c_arrayto
   integer(kind=c_int), intent(in)          :: nlinks, linkType
   integer, pointer                         :: arrayfrom(:), arrayto(:)
   integer                                  :: ierr
   
   call c_f_pointer(c_arrayfrom, arrayfrom, (/ nlinks /))
   call c_f_pointer(c_arrayto, arrayto, (/ nlinks /))
   
   ierr = ggeo_get_links(arrayfrom, arrayto, linkType)
   
end function ggeo_get_links_dll

function ggeo_convert_1d_arrays_dll(c_nodex, c_nodey, c_branchoffset, c_branchlength, c_branchid, c_sourceNodeId, c_targetNodeId, nbranches, nmeshnodes, startIndex) result(ierr) bind(C, name="ggeo_convert_1d_arrays")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_convert_1d_arrays_dll

   use gridoperations
   use gridgeom
   use meshdata
   
   type(c_ptr), intent(in)                :: c_nodex, c_nodey, c_branchoffset, c_branchlength, c_branchid, c_sourceNodeId, c_targetNodeId   
   integer(kind=c_int), intent(in)        :: nmeshnodes, nBranches, startIndex
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
   
   !ggeo_convert_1d_arrays gives back 1d based arrays
   ierr =  ggeo_convert_1d_arrays(nodex, nodey, branchoffset, branchlength, branchid, sourcenodeid, targetnodeid, meshgeom, startIndex)
    
   !1d based arrays are provided
   ierr = ggeo_convert(meshgeom, 1)

end function ggeo_convert_1d_arrays_dll

function ggeo_create_edge_nodes_dll(c_branchoffset, c_branchlength, c_branchids, c_sourceNodeId, c_targetNodeId, c_edgenodes, nBranches, nNodes, nEdgeNodes, startIndex) result(ierr) bind(C, name="ggeo_create_edge_nodes")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_create_edge_nodes_dll

   use gridoperations
   
   type(c_ptr), intent(in)   :: c_branchoffset, c_branchids, c_edgenodes, c_sourceNodeId, c_targetNodeId, c_branchlength    
   integer, intent(in)       :: nBranches, nNodes, nEdgeNodes, startIndex
   double precision, pointer :: branchoffset(:), branchlength(:)
   integer, pointer          :: branchids(:), edgenodes(:,:), sourceNodeId(:), targetNodeId(:) 
   integer                   :: ierr,numedge

   

   call c_f_pointer(c_branchlength, branchlength, (/ nBranches /))
   
   call c_f_pointer(c_branchids, branchids, (/ nNodes /))
   call c_f_pointer(c_sourceNodeId, sourceNodeId, (/ nBranches /))
   call c_f_pointer(c_targetNodeId, targetNodeId, (/ nBranches /))
   call c_f_pointer(c_edgenodes, edgenodes, (/ 2, nEdgeNodes /))
   call c_f_pointer(c_branchoffset, branchoffset, (/ nNodes /))

   ierr = ggeo_count_or_create_edge_nodes(branchids, branchoffset, sourcenodeid, targetnodeid, branchlength, startIndex, numedge, edgenodes)

end function ggeo_create_edge_nodes_dll

function ggeo_deallocate_dll() result(ierr) bind(C, name="ggeo_deallocate")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_deallocate_dll
   use gridoperations   
   
   integer                   :: ierr
   
   ierr = ggeo_deallocate()

end function ggeo_deallocate_dll

end module gridgeom_api
