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

integer, parameter :: ug_strLenMeta      = 100
integer, parameter :: ug_idsLen          = 40
integer, parameter :: ug_idsLongNamesLen = 80

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
   integer            :: nnodes             !< Number of branches
   integer            :: nbranches          !< Number of branches
   integer            :: ngeometry          !< Number of geometrical points
   integer            :: start_index        !< The base index of the arrays

   integer, pointer :: edge_nodes(:,:) => null() !< Edge-to-node mapping array.
   integer, pointer :: face_nodes(:,:) => null() !< Face-to-node mapping array.
   integer, pointer :: edge_faces(:,:) => null() !< Edge-to-face mapping array (optional, can be null()).
   integer, pointer :: face_edges(:,:) => null() !< Face-to-edge mapping array (optional, can be null()).
   integer, pointer :: face_links(:,:) => null() !< Face-to-face mapping array (optional, can be null()).
   
   !Network1d variables
   double precision,                  pointer :: nnodex(:) => null()                 !< x-coordinates of the network points.
   double precision,                  pointer :: nnodey(:) => null()                 !< y-coordinates of the network points.
   character(len=ug_idsLen),          pointer :: nnodeids(:) => null()               !< network nodes ids description 
   character(len=ug_idsLongNamesLen), pointer :: nnodelongnames(:) => null()         !< network nodes nnodelongnames description 

   integer,                           pointer :: nedge_nodes(:,:) => null()          !< Start-end node of each branch
   character(len=ug_idsLen),          pointer :: nbranchids(:) => null()             !< Branch nodes ids 
   character(len=ug_idsLongNamesLen), pointer :: nbranchlongnames(:) => null()       !< Branch long names
   double precision,                  pointer :: nbranchlengths(:) => null()         !< Branch lenghts
   integer,                           pointer :: nbranchgeometrynodes(:) => null()   !< Number of geometry points in each branch
   double precision,                  pointer :: ngeopointx(:) => null()             !< x-coordinates of geometry points.
   double precision,                  pointer :: ngeopointy(:) => null()             !< y-coordinates of geometry points.
   integer,                           pointer :: nbranchorder(:) => null()           !< the branch order
   
   !Mesh1d variables
   integer,                           pointer :: branchidx(:) => null()              !< The branch index of each 1d mesh point
   double precision,                  pointer :: branchoffsets(:)=> null()           !< The branch offset of each 1d mesh point

   double precision, pointer :: nodex(:)=> null()       !< x-coordinates of the mesh nodes.
   double precision, pointer :: nodey(:)=> null()       !< y-coordinates of the mesh nodes.
   double precision, pointer :: nodez(:)=> null()       !< z-coordinates of the mesh nodes.
   double precision, pointer :: edgex(:)=> null()       !< x-coordinates of the mesh edges.
   double precision, pointer :: edgey(:)=> null()       !< y-coordinates of the mesh edges.
   double precision, pointer :: edgez(:)=> null()       !< z-coordinates of the mesh edges.
   double precision, pointer :: facex(:)=> null()       !< x-coordinates of the mesh faces.
   double precision, pointer :: facey(:)=> null()       !< y-coordinates of the mesh faces.
   double precision, pointer :: facez(:)=> null()       !< z-coordinates of the mesh faces.
   
   double precision, pointer :: layer_zs(:) => null()    !< Vertical coordinates of the mesh layers' center (either z or sigma).
   double precision, pointer :: interface_zs(:)=> null() !< Vertical coordinates of the mesh layers' interface (either z or sigma).

end type t_ug_meshgeom

type, bind(C) :: c_t_ug_meshgeomdim

   type(c_ptr)              :: meshname           !< Name of this mesh ! TODO: AvD: should this be in this data type?
   integer(kind=c_int)      :: dim                !< Dimensionality of the mesh (1/2/3)
   integer(kind=c_int)      :: numnode            !< Number of mesh nodes.
   integer(kind=c_int)      :: numedge            !< Number of mesh edges.
   integer(kind=c_int)      :: numface            !< Number of mesh faces.
   integer(kind=c_int)      :: maxnumfacenodes    !< Maximum of number of face nodes.
   integer(kind=c_int)      :: numlayer           !< Number of mesh layers (num interfaces == numlayer + 1), numlayer = 0 means "no layers".
   integer(kind=c_int)      :: layertype          !< Type of vertical layer definition (only if numlayer >= 1), one of LAYERTYPE_* parameters.
   integer(kind=c_int)      :: nnodes
   integer(kind=c_int)      :: nbranches          !< Number of branches
   integer(kind=c_int)      :: ngeometry          !< Number of geometry points
   
end type c_t_ug_meshgeomdim

type, bind(C) :: c_t_ug_meshgeom

   type(c_ptr) :: edge_nodes              !< Edge-to-node mapping array.
   type(c_ptr) :: face_nodes              !< Face-to-node mapping array.
   type(c_ptr) :: edge_faces              !< Edge-to-face mapping array (optional, can be null()).
   type(c_ptr) :: face_edges              !< Face-to-edge mapping array (optional, can be null()).
   type(c_ptr) :: face_links              !< Face-to-face mapping array (optional, can be null()).
   
   !Mesh 1d variables
   type(c_ptr)  :: nnodex                 !< x-coordinates of the network points.  
   type(c_ptr)  :: nnodey                 !< y-coordinates of the network points.
   type(c_ptr)  :: nedge_nodes            !< Start-end node of each branch                
   type(c_ptr)  :: nbranchlengths         !< The branch lenghts  
   type(c_ptr)  :: nbranchgeometrynodes   !< Number of geometry points in each branch
   type(c_ptr)  :: ngeopointx             !< x-coordinates of geometry points.
   type(c_ptr)  :: ngeopointy             !< y-coordinates of geometry points.
   type(c_ptr)  :: nbranchorder           !< the branch order
   
   type(c_ptr)  :: branchidx              !< The branch index of each 1d mesh point
   type(c_ptr)  :: branchoffsets          !< The branch offset of each 1d mesh point
   
   type(c_ptr) :: nodex                   !< x-coordinates of the mesh nodes.
   type(c_ptr) :: nodey                   !< y-coordinates of the mesh nodes.
   type(c_ptr) :: nodez                   !< z-coordinates of the mesh nodes.
   type(c_ptr) :: edgex                   !< x-coordinates of the mesh edges.
   type(c_ptr) :: edgey                   !< y-coordinates of the mesh edges.
   type(c_ptr) :: edgez                   !< z-coordinates of the mesh edges.
   type(c_ptr) :: facex                   !< x-coordinates of the mesh faces.
   type(c_ptr) :: facey                   !< y-coordinates of the mesh faces.
   type(c_ptr) :: facez                   !< z-coordinates of the mesh faces.
   
   type(c_ptr)              :: layer_zs           !< Vertical coordinates of the mesh layers' center (either z or sigma).
   type(c_ptr)              :: interface_zs       !< Vertical coordinates of the mesh layers' interface (either z or sigma).
   integer(kind=c_int)      :: start_index        !< The base index of the arrays

end type c_t_ug_meshgeom

   contains 
   
function convert_meshgeom_to_cptr(meshgeom, c_meshgeom) result(ierr)

   type(t_ug_meshgeom), intent(in)      :: meshgeom
   type(c_t_ug_meshgeom), intent(inout) :: c_meshgeom
   integer                              :: ierr
   !support variables
   integer,          pointer  :: edge_nodes(:,:) => null()!< Edge-to-node mapping array.
   integer,          pointer  :: face_nodes(:,:) => null()!< Face-to-node mapping array.
   integer,          pointer  :: edge_faces(:,:) => null()!< Edge-to-face mapping array (optional, can be null()).
   integer,          pointer  :: face_edges(:,:) => null()!< Face-to-edge mapping array (optional, can be null()).
   integer,          pointer  :: face_links(:,:) => null()!< Face-to-face mapping array (optional, can be null()).
   
   !Network1d variables
   double precision, pointer :: nnodex(:) => null() 
   double precision, pointer :: nnodey(:) => null() 
   integer,          pointer :: nedge_nodes(:,:) => null()        
   double precision, pointer :: nbranchlengths(:) => null() 
   integer,          pointer :: nbranchgeometrynodes(:)=> null()  
   double precision, pointer :: ngeopointx(:) => null() 
   double precision, pointer :: ngeopointy(:)   => null() 
   integer,          pointer :: nbranchorder(:) => null()   
   !Mesh1d variables
   integer,          pointer :: branchidx(:) => null()    !< Branch id of each mesh node 
   double precision, pointer :: branchoffsets(:)=> null() !< Branch offset of each mesh node
   
   double precision, pointer :: nodex(:) => null()       !< x-coordinates of the mesh nodes.
   double precision, pointer :: nodey(:) => null()      !< y-coordinates of the mesh nodes.
   double precision, pointer :: nodez(:) => null()      !< z-coordinates of the mesh nodes.
   double precision, pointer :: edgex(:) => null()      !< x-coordinates of the mesh edges.
   double precision, pointer :: edgey(:) => null()      !< y-coordinates of the mesh edges.
   double precision, pointer :: edgez(:) => null()      !< z-coordinates of the mesh edges.
   double precision, pointer :: facex(:) => null()      !< x-coordinates of the mesh faces.
   double precision, pointer :: facey(:) => null()      !< y-coordinates of the mesh faces.
   double precision, pointer :: facez(:) => null()      !< z-coordinates of the mesh faces.

   double precision, pointer :: layer_zs(:) => null()     !< Vertical coordinates of the mesh layers' center (either z or sigma).
   double precision, pointer :: interface_zs(:) => null() !< Vertical coordinates of the mesh layers' interface (either z or sigma).
   
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
   
   !Network1d variables
   if (associated(meshgeom%nnodex)) then
      call c_f_pointer(c_meshgeom%nnodex, nnodex, (/ size(meshgeom%nnodex)/))
      nnodex = meshgeom%nnodex
   endif
   
   if (associated(meshgeom%nnodey)) then
      call c_f_pointer(c_meshgeom%nnodey, nnodey, (/ size(meshgeom%nnodey)/))
      nnodey = meshgeom%nnodey
   endif
   
   if (associated(meshgeom%nedge_nodes)) then
      call c_f_pointer(c_meshgeom%nedge_nodes,nedge_nodes , (/ size(meshgeom%nedge_nodes, 1), size(meshgeom%nedge_nodes, 2)/))
      nedge_nodes= meshgeom%nedge_nodes
   endif
   
   if (associated(meshgeom%nbranchlengths)) then
      call c_f_pointer(c_meshgeom%nbranchlengths, nbranchlengths, (/ size(meshgeom%nbranchlengths)/))
      nbranchlengths= meshgeom%nbranchlengths
   endif
   
   if (associated(meshgeom%nbranchgeometrynodes)) then
      call c_f_pointer(c_meshgeom%nbranchgeometrynodes, nbranchgeometrynodes , (/ size(meshgeom%nbranchgeometrynodes)/))
      nbranchgeometrynodes = meshgeom%nbranchgeometrynodes
   endif
   
   if (associated(meshgeom%ngeopointx)) then
      call c_f_pointer(c_meshgeom%ngeopointx, ngeopointx , (/ size(meshgeom%ngeopointx)/))
      ngeopointx = meshgeom%ngeopointx
   endif
   
   if (associated(meshgeom%ngeopointy)) then
      call c_f_pointer(c_meshgeom%ngeopointy, ngeopointy , (/ size(meshgeom%ngeopointy)/))
      ngeopointy = meshgeom%ngeopointy
   endif
   
   if (associated(meshgeom%nbranchorder)) then
      call c_f_pointer(c_meshgeom%nbranchorder, nbranchorder, (/ size(meshgeom%nbranchorder)/))
      nbranchorder = meshgeom%nbranchorder
   endif
   
   !Mesh1d
   if (associated(meshgeom%branchidx)) then
      call c_f_pointer(c_meshgeom%branchidx, branchidx, (/ size(meshgeom%branchidx,1)/) )
      branchidx = meshgeom%branchidx
   endif
      
   if (associated(meshgeom%branchoffsets)) then
      call c_f_pointer(c_meshgeom%branchoffsets, branchoffsets, (/ size(meshgeom%branchoffsets,1)/) )
      branchoffsets = meshgeom%branchoffsets
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
   
   meshgeom%nnodes = c_meshgeomdim%nnodes  
   meshgeom%nbranches = c_meshgeomdim%nbranches       
   meshgeom%ngeometry = c_meshgeomdim%ngeometry
   
   !start index is an array property
   meshgeom%start_index = c_meshgeom%start_index
  
   ierr = 0
   
   call c_f_pointer(c_meshgeom%edge_nodes, meshgeom%edge_nodes, (/ 2, c_meshgeomdim%numedge /)) 
   call c_f_pointer(c_meshgeom%face_nodes, meshgeom%face_nodes, (/ c_meshgeomdim%maxnumfacenodes, c_meshgeomdim%numface /))
   call c_f_pointer(c_meshgeom%edge_faces, meshgeom%edge_faces, (/ 2, c_meshgeomdim%numedge /))
   call c_f_pointer(c_meshgeom%face_edges, meshgeom%face_edges, (/ c_meshgeomdim%maxnumfacenodes, c_meshgeomdim%numface /))
   call c_f_pointer(c_meshgeom%face_links, meshgeom%face_links, (/ c_meshgeomdim%maxnumfacenodes, c_meshgeomdim%numface /))
   
   !Network variables 
   call c_f_pointer(c_meshgeom%nnodex, meshgeom%nnodex,(/c_meshgeomdim%nnodes/))
   call c_f_pointer(c_meshgeom%nnodey, meshgeom%nnodey,(/c_meshgeomdim%nnodes/))
   !nodeids and nodelongnames are not communicated using meshgeom
   call c_f_pointer(c_meshgeom%nedge_nodes, meshgeom%nedge_nodes, (/ 2, c_meshgeomdim%nbranches /))   
   !branchids and branchlongnames are not communicated using meshgeom
   call c_f_pointer(c_meshgeom%nbranchlengths, meshgeom%nbranchlengths, (/ c_meshgeomdim%nbranches /))
   call c_f_pointer(c_meshgeom%nbranchgeometrynodes, meshgeom%nbranchgeometrynodes, (/ c_meshgeomdim%nbranches /))   
   call c_f_pointer(c_meshgeom%ngeopointx, meshgeom%ngeopointx, (/ c_meshgeomdim%ngeometry/))
   call c_f_pointer(c_meshgeom%ngeopointy, meshgeom%ngeopointy, (/ c_meshgeomdim%ngeometry/))   
   call c_f_pointer(c_meshgeom%nbranchorder, meshgeom%nbranchorder, (/ c_meshgeomdim%nbranches/))   
   
   !Mesh1d variables
   call c_f_pointer(c_meshgeom%branchidx, meshgeom%branchidx, (/ c_meshgeomdim%numnode/))
   call c_f_pointer(c_meshgeom%branchoffsets, meshgeom%branchoffsets, (/ c_meshgeomdim%numnode /))   
   
   call c_f_pointer(c_meshgeom%nodex, meshgeom%nodex,(/c_meshgeomdim%numnode/))
   call c_f_pointer(c_meshgeom%nodey, meshgeom%nodey,(/c_meshgeomdim%numnode/))
   call c_f_pointer(c_meshgeom%nodez, meshgeom%nodez,(/c_meshgeomdim%numnode/))
   call c_f_pointer(c_meshgeom%edgex, meshgeom%edgex,(/c_meshgeomdim%numedge/))
   call c_f_pointer(c_meshgeom%edgey, meshgeom%edgey,(/c_meshgeomdim%numedge/))
   call c_f_pointer(c_meshgeom%edgez, meshgeom%edgez,(/c_meshgeomdim%numedge/)) 
   call c_f_pointer(c_meshgeom%facex, meshgeom%facex,(/c_meshgeomdim%numface/))
   call c_f_pointer(c_meshgeom%facey, meshgeom%facey,(/c_meshgeomdim%numface/))
   call c_f_pointer(c_meshgeom%facez, meshgeom%facez,(/c_meshgeomdim%numface/))
  
   call c_f_pointer(c_meshgeom%layer_zs, meshgeom%layer_zs,(/c_meshgeomdim%numlayer/))
   call c_f_pointer(c_meshgeom%interface_zs, meshgeom%interface_zs,(/c_meshgeomdim%numlayer + 1/))
      
end function convert_cptr_to_meshgeom



end module meshdata
