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

!> Module for grid operations.

module meshdata

use iso_c_binding

implicit none

!> Structure for storing an entire mesh geometry (topology and coordinates and more).
!> This is general data structures shared also by gridgeom
type t_ug_meshgeom
! TODO: AvD: extend this to 3D (volumes)
   character(len=256) :: meshname           !< Name of this mesh ! TODO: AvD: should this be in this data type?
   integer            :: dim                !< Dimensionality of the mesh (1/2/3)
   integer            :: numnode            !< Number of mesh nodes.
   integer            :: numedge            !< Number of mesh edges (size of kn)
   integer            :: numface            !< Number of mesh faces.
   integer            :: maxnumfacenodes    !< Maximum of number of face nodes.
   integer            :: numlayer           !< Number of mesh layers (num interfaces == numlayer + 1), numlayer = 0 means "no layers".
   integer            :: layertype          !< Type of vertical layer definition (only if numlayer >= 1), one of LAYERTYPE_* parameters.
   integer            :: nt_nbranches       !< Number of branches
   integer            :: nt_ngeometry       !< Number of geometrical points
   integer            :: start_index        !< The base index of the arrays

   integer,      pointer :: edge_nodes(:,:) !< Edge-to-node mapping array.
   integer,      pointer :: face_nodes(:,:) !< Face-to-node mapping array.
   integer,      pointer :: edge_faces(:,:) !< Edge-to-face mapping array (optional, can be null()).
   integer,      pointer :: face_edges(:,:) !< Face-to-edge mapping array (optional, can be null()).
   integer,      pointer :: face_links(:,:) !< Face-to-face mapping array (optional, can be null()).
   
   !Mesh1d, Network1d variables
   integer,      pointer :: branchids(:)     !< Branch id of each mesh node 
   integer,      pointer :: nbranchgeometrynodes(:)    !< Number of geometry nodes in each branch
   
   double precision, pointer :: nodex(:)       !< x-coordinates of the mesh nodes.
   double precision, pointer :: nodey(:)       !< y-coordinates of the mesh nodes.
   double precision, pointer :: nodez(:)       !< z-coordinates of the mesh nodes.
   double precision, pointer :: edgex(:)       !< x-coordinates of the mesh edges.
   double precision, pointer :: edgey(:)       !< y-coordinates of the mesh edges.
   double precision, pointer :: edgez(:)       !< z-coordinates of the mesh edges.
   double precision, pointer :: facex(:)       !< x-coordinates of the mesh faces.
   double precision, pointer :: facey(:)       !< y-coordinates of the mesh faces.
   double precision, pointer :: facez(:)       !< z-coordinates of the mesh faces.
   
   !Mesh1d, Network1d variables
   double precision, pointer :: branchoffsets(:) !< Branch offset of each mesh node
   double precision, pointer :: geopointsX(:)    !< x-coordinates of the geometry points.
   double precision, pointer :: geopointsY(:)    !< y-coordinates of the geometry points.
   double precision, pointer :: branchlengths(:) !< lengths of each branch
   
   double precision, pointer :: layer_zs(:)     !< Vertical coordinates of the mesh layers' center (either z or sigma).
   double precision, pointer :: interface_zs(:) !< Vertical coordinates of the mesh layers' interface (either z or sigma).

end type t_ug_meshgeom

type c_t_ug_meshgeomdim

   type(c_ptr)              :: meshname           !< Name of this mesh ! TODO: AvD: should this be in this data type?
   integer(kind=c_int)      :: dim                !< Dimensionality of the mesh (1/2/3)
   integer(kind=c_int)      :: numnode            !< Number of mesh nodes.
   integer(kind=c_int)      :: numedge            !< Number of mesh edges.
   integer(kind=c_int)      :: numface            !< Number of mesh faces.
   integer(kind=c_int)      :: maxnumfacenodes    !< Maximum of number of face nodes.
   integer(kind=c_int)      :: numlayer           !< Number of mesh layers (num interfaces == numlayer + 1), numlayer = 0 means "no layers".
   integer(kind=c_int)      :: layertype          !< Type of vertical layer definition (only if numlayer >= 1), one of LAYERTYPE_* parameters.
   integer(kind=c_int)      :: nt_nbranches       !< Number of branches
   integer(kind=c_int)      :: nt_ngeometry       !< Number of geometry points
   integer(kind=c_int)      :: start_index        !< The base index of the arrays
   
end type c_t_ug_meshgeomdim

type c_t_ug_meshgeom

   type(c_ptr) :: edge_nodes !< Edge-to-node mapping array.
   type(c_ptr) :: face_nodes !< Face-to-node mapping array.
   type(c_ptr) :: edge_faces !< Edge-to-face mapping array (optional, can be null()).
   type(c_ptr) :: face_edges !< Face-to-edge mapping array (optional, can be null()).
   type(c_ptr) :: face_links !< Face-to-face mapping array (optional, can be null()).
   
   !Mesh 1d variables
   type(c_ptr) :: branchids               !< Branch id of each mesh node
   type(c_ptr) :: nbranchgeometrynodes    !< Number of geometry nodes in each branch

   type(c_ptr) :: nodex       !< x-coordinates of the mesh nodes.
   type(c_ptr) :: nodey       !< y-coordinates of the mesh nodes.
   type(c_ptr) :: nodez       !< z-coordinates of the mesh nodes.
   type(c_ptr) :: edgex       !< x-coordinates of the mesh edges.
   type(c_ptr) :: edgey       !< y-coordinates of the mesh edges.
   type(c_ptr) :: edgez       !< z-coordinates of the mesh edges.
   type(c_ptr) :: facex       !< x-coordinates of the mesh faces.
   type(c_ptr) :: facey       !< y-coordinates of the mesh faces.
   type(c_ptr) :: facez       !< z-coordinates of the mesh faces.
   
   !Network 1d variables
   type(c_ptr) :: branchoffsets           !< Branch offset of each mesh node
   type(c_ptr) :: geopointsX    !< x-coordinates of the geometry points.
   type(c_ptr) :: geopointsY    !< y-coordinates of the geometry points.
   type(c_ptr) :: branchlengths !< lengths of each branch
      
   type(c_ptr) :: layer_zs
   type(c_ptr) :: interface_zs

end type c_t_ug_meshgeom

   contains 
   
function convert_meshgeom_to_cptr(meshgeom, c_meshgeom) result(ierr)

   type(t_ug_meshgeom), intent(in)      :: meshgeom
   type(c_t_ug_meshgeom), intent(inout) :: c_meshgeom
   integer                              :: ierr
!support variables
   integer,          pointer  :: edge_nodes(:,:) !< Edge-to-node mapping array.
   integer,          pointer  :: face_nodes(:,:) !< Face-to-node mapping array.
   integer,          pointer  :: edge_faces(:,:) !< Edge-to-face mapping array (optional, can be null()).
   integer,          pointer  :: face_edges(:,:) !< Face-to-edge mapping array (optional, can be null()).
   integer,          pointer  :: face_links(:,:) !< Face-to-face mapping array (optional, can be null()).
   
   !Mesh 1d variables
   integer,      pointer :: branchids(:)     !< Branch id of each mesh node
   integer,      pointer :: nbranchgeometrynodes(:)    !< Number of geometry nodes in each branch
   
   double precision, pointer :: nodex(:)       !< x-coordinates of the mesh nodes.
   double precision, pointer :: nodey(:)       !< y-coordinates of the mesh nodes.
   double precision, pointer :: nodez(:)       !< z-coordinates of the mesh nodes.
   double precision, pointer :: edgex(:)       !< x-coordinates of the mesh edges.
   double precision, pointer :: edgey(:)       !< y-coordinates of the mesh edges.
   double precision, pointer :: edgez(:)       !< z-coordinates of the mesh edges.
   double precision, pointer :: facex(:)       !< x-coordinates of the mesh faces.
   double precision, pointer :: facey(:)       !< y-coordinates of the mesh faces.
   double precision, pointer :: facez(:)       !< z-coordinates of the mesh faces.

   !Network 1d variables
   double precision, pointer :: branchoffsets(:) !< Branch offset of each mesh node
   double precision, pointer :: geopointsX(:)    !< x-coordinates of the geometry points.
   double precision, pointer :: geopointsY(:)    !< y-coordinates of the geometry points.
   double precision, pointer :: branchlengths(:) !< lengths of each branch

   double precision, pointer :: layer_zs(:)     !< Vertical coordinates of the mesh layers' center (either z or sigma).
   double precision, pointer :: interface_zs(:) !< Vertical coordinates of the mesh layers' interface (either z or sigma).
   
   ierr = 0
   !! array variables
   if (associated(meshgeom%edge_nodes)) then
      call c_f_pointer(c_meshgeom%edge_nodes, edge_nodes, (/ size(meshgeom%edge_nodes,1), size(meshgeom%edge_nodes,2)/) )
      edge_nodes = meshgeom%edge_nodes
   endif
   
   if (associated(meshgeom%face_nodes)) then
      call c_f_pointer(c_meshgeom%face_nodes, face_nodes, (/ size(meshgeom%face_nodes,1), size(meshgeom%face_nodes,2)/) )
      face_nodes= meshgeom%face_nodes
   endif
   
   if (associated(meshgeom%edge_faces)) then
      call c_f_pointer(c_meshgeom%edge_faces, edge_faces, (/ size(meshgeom%edge_faces,1), size(meshgeom%edge_faces,2)/) )
      edge_faces= meshgeom%edge_faces
   endif
   
   if (associated(meshgeom%face_edges)) then
      call c_f_pointer(c_meshgeom%face_edges, face_edges, (/ size(meshgeom%face_edges,1), size(meshgeom%face_edges,2)/) )
      face_edges= meshgeom%face_edges
   endif
   
   if (associated(meshgeom%face_links)) then
      call c_f_pointer(c_meshgeom%face_links, face_links, (/ size(meshgeom%face_links,1), size(meshgeom%face_links,2)/) )
      face_links= meshgeom%face_links
   endif

   !Mesh1d/Network1d
   if (associated(meshgeom%branchids)) then
      call c_f_pointer(c_meshgeom%branchids, branchids, (/ size(meshgeom%branchids,1)/) )
      branchids= meshgeom%branchids
   endif
      
   if (associated(meshgeom%nbranchgeometrynodes)) then
      call c_f_pointer(c_meshgeom%nbranchgeometrynodes, nbranchgeometrynodes, (/ size(meshgeom%nbranchgeometrynodes,1)/) )
      nbranchgeometrynodes= meshgeom%nbranchgeometrynodes
    endif
               
   !mesh nodes
   if (associated(meshgeom%nodex)) then
      call c_f_pointer(c_meshgeom%nodex, nodex, (/ size(meshgeom%nodex,1) /) )
      nodex = meshgeom%nodex
   endif

   if (associated(meshgeom%nodey)) then
      call c_f_pointer(c_meshgeom%nodey, nodey, (/ size(meshgeom%nodey,1)/) )
      nodey = meshgeom%nodey
   endif
   
   if (associated(meshgeom%nodez)) then
      call c_f_pointer(c_meshgeom%nodez, nodez, (/ size(meshgeom%nodez,1)/) )
      nodez = meshgeom%nodez
   endif

   !mesh edges
   if (associated(meshgeom%edgex)) then
      call c_f_pointer(c_meshgeom%edgex, edgex, (/ size(meshgeom%edgex,1)/) )
      edgex= meshgeom%edgex
   endif
   if (associated(meshgeom%edgey)) then
      call c_f_pointer(c_meshgeom%edgey, edgey, (/ size(meshgeom%edgey,1)/) )
      edgey= meshgeom%edgey
   endif
   if (associated(meshgeom%edgez)) then
      call c_f_pointer(c_meshgeom%edgez, edgez, (/ size(meshgeom%edgez,1)/) )
      edgez= meshgeom%edgez
   endif   
   
   !mesh faces
   if (associated(meshgeom%facex)) then
      call c_f_pointer(c_meshgeom%facex, facex, (/ size(meshgeom%facex,1)/) )
      facex= meshgeom%facex
   endif
   if (associated(meshgeom%facey)) then
      call c_f_pointer(c_meshgeom%facey, facey, (/ size(meshgeom%facey,1)/) )
      facey= meshgeom%facey
   endif
   if (associated(meshgeom%facez)) then
      call c_f_pointer(c_meshgeom%facez, facez, (/ size(meshgeom%facez,1)/) )
      facez= meshgeom%facez
   endif
   
   !Mesh1d/Network1d
   if (associated(meshgeom%branchoffsets)) then
      call c_f_pointer(c_meshgeom%branchoffsets, branchoffsets, (/ size(meshgeom%branchoffsets,1)/) )
      branchoffsets= meshgeom%branchoffsets
   endif

   if (associated(meshgeom%geopointsX)) then
      call c_f_pointer(c_meshgeom%geopointsX, geopointsX, (/ size(meshgeom%geopointsX,1)/) )
      geopointsX= meshgeom%geopointsX
   endif

   if (associated(meshgeom%geopointsY)) then
      call c_f_pointer(c_meshgeom%geopointsY, geopointsY, (/ size(meshgeom%geopointsY,1)/) )
      geopointsY= meshgeom%geopointsY
   endif

   if (associated(meshgeom%branchlengths)) then
      call c_f_pointer(c_meshgeom%branchlengths, branchlengths, (/ size(meshgeom%branchlengths,1)/) )
      branchlengths= meshgeom%branchlengths
   endif

   !layer
   if (associated(meshgeom%layer_zs)) then
      call c_f_pointer(c_meshgeom%layer_zs, layer_zs, (/ size(meshgeom%layer_zs,1)/) )
      layer_zs= meshgeom%layer_zs
   endif

   !interface
   if (associated(meshgeom%interface_zs)) then
      call c_f_pointer(c_meshgeom%interface_zs, interface_zs, (/ size(meshgeom%interface_zs,1)/) )
      interface_zs= meshgeom%interface_zs
   endif
   
end function convert_meshgeom_to_cptr


function convert_cptr_to_meshgeom(c_meshgeom, c_meshgeomdim, meshgeom) result(ierr)

   type(c_t_ug_meshgeom), intent(in)      :: c_meshgeom
   type(c_t_ug_meshgeomdim), intent(in)   :: c_meshgeomdim
   type(t_ug_meshgeom), intent(inout)     :: meshgeom
   integer                                :: ierr
   
   ! get the dimensions
   
   meshgeom%dim = c_meshgeomdim%dim                
   meshgeom%numnode = c_meshgeomdim%numnode           
   meshgeom%numedge = c_meshgeomdim%numedge           
   meshgeom%numface = c_meshgeomdim%numface          
   meshgeom%maxnumfacenodes = c_meshgeomdim%maxnumfacenodes    
   meshgeom%numlayer = c_meshgeomdim%numlayer          
   meshgeom%layertype = c_meshgeomdim%layertype     
   meshgeom%nt_nbranches = c_meshgeomdim%nt_nbranches        
   meshgeom%nt_ngeometry = c_meshgeomdim%nt_ngeometry 
   meshgeom%start_index = c_meshgeomdim%start_index
  
   ierr = 0
   ! to finish
   call c_f_pointer(c_meshgeom%edge_nodes, meshgeom%edge_nodes, (/ 2, c_meshgeomdim%numedge /)) 
   call c_f_pointer(c_meshgeom%face_nodes, meshgeom%face_nodes, (/ c_meshgeomdim%maxnumfacenodes, c_meshgeomdim%numface /))
   call c_f_pointer(c_meshgeom%edge_faces, meshgeom%edge_faces, (/ 2, c_meshgeomdim%numedge /))
   call c_f_pointer(c_meshgeom%face_edges, meshgeom%face_edges, (/ c_meshgeomdim%maxnumfacenodes, c_meshgeomdim%numface /))
   call c_f_pointer(c_meshgeom%face_links, meshgeom%face_links, (/ c_meshgeomdim%maxnumfacenodes, c_meshgeomdim%numface /))
   
   call c_f_pointer(c_meshgeom%branchids, meshgeom%branchids, (/ c_meshgeomdim%numnode/))
   call c_f_pointer(c_meshgeom%nbranchgeometrynodes, meshgeom%nbranchgeometrynodes, (/ c_meshgeomdim%nt_nbranches /))
   
   call c_f_pointer(c_meshgeom%nodex, meshgeom%nodex,(/c_meshgeomdim%numnode/))
   call c_f_pointer(c_meshgeom%nodey, meshgeom%nodey,(/c_meshgeomdim%numnode/))
   call c_f_pointer(c_meshgeom%nodez, meshgeom%nodez,(/c_meshgeomdim%numnode/))
   
   call c_f_pointer(c_meshgeom%edgex, meshgeom%edgex,(/c_meshgeomdim%numedge/))
   call c_f_pointer(c_meshgeom%edgey, meshgeom%edgey,(/c_meshgeomdim%numedge/))
   call c_f_pointer(c_meshgeom%edgez, meshgeom%edgez,(/c_meshgeomdim%numedge/))
   
   call c_f_pointer(c_meshgeom%facex, meshgeom%facex,(/c_meshgeomdim%numface/))
   call c_f_pointer(c_meshgeom%facey, meshgeom%facey,(/c_meshgeomdim%numface/))
   call c_f_pointer(c_meshgeom%facez, meshgeom%facez,(/c_meshgeomdim%numface/))
 
   call c_f_pointer(c_meshgeom%branchoffsets, meshgeom%branchoffsets, (/ c_meshgeomdim%numnode /))
   call c_f_pointer(c_meshgeom%geopointsX, meshgeom%geopointsX, (/ c_meshgeomdim%nt_ngeometry/))
   call c_f_pointer(c_meshgeom%geopointsY, meshgeom%geopointsY, (/ c_meshgeomdim%nt_ngeometry/))
   call c_f_pointer(c_meshgeom%branchlengths, meshgeom%branchlengths, (/ c_meshgeomdim%nt_nbranches /))
   
   call c_f_pointer(c_meshgeom%layer_zs, meshgeom%layer_zs,(/c_meshgeomdim%numlayer/))
   call c_f_pointer(c_meshgeom%interface_zs, meshgeom%interface_zs,(/c_meshgeomdim%numlayer + 1/))
      
end function convert_cptr_to_meshgeom



end module meshdata