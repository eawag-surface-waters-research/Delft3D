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

!> I/O module for reading and writing NetCDF files with UGRID-compliant data on unstructured grids.
!! UGRID Conventions website: https://github.com/ugrid-conventions/ugrid-conventions
module io_ugrid
use netcdf
use messagehandling
use coordinate_reference_system
implicit none

! TODO: AvD: GL2: add 'full_grid_output' support, to write 1. face_edge_connectivity; 2. edge_face_connectivity; and possibly more.
! TODO: AvD: GL2: add cell_methods to edge/face data (:mean)
! TODO: AvD: GL2: add integer variable Mesh2_edge_bc with :flag_meanings = "none closed dirichlet"; :flag_values = 0, 1, 2 ;
! TODO: AvD: GL2: move grid_mapping attribute to all data variables, not coordinate variables.

!! Conventions
character(len=6), parameter :: UG_CONV_CF   = 'CF-1.6'      !< Version of CF conventions currently adopted.
character(len=9), parameter :: UG_CONV_UGRID = 'UGRID-1.0'  !< Version of UGRID conventions currently adopted.
character(len=16), parameter :: UG_CONV_DELTARES = 'Deltares-0.8' !< Version of Deltares extension.

!! Meta data
integer, parameter :: ug_strLenMeta = 100
type t_ug_meta
   character(len=ug_strLenMeta) :: institution
   character(len=ug_strLenMeta) :: source
   character(len=ug_strLenMeta) :: references
   character(len=ug_strLenMeta) :: version
   character(len=ug_strLenMeta) :: modelname
end type t_ug_meta

!! Error codes
integer, parameter :: UG_NOERR                 = NF90_NOERR
integer, parameter :: UG_SOMEERR               = 10 !< Some unspecified error.
integer, parameter :: UG_INVALID_MESHNAME      = 11
integer, parameter :: UG_INVALID_MESHDIMENSION = 12
integer, parameter :: UG_INVALID_DATALOCATION  = 13
integer, parameter :: UG_ARRAY_TOOSMALL        = 14 !< If while getting data, the target array is too small for the amount of data that needs to be put into it.
integer, parameter :: UG_VAR_NOTFOUND          = 15 !< Some variable was not found.
integer, parameter :: UG_INVALID_LAYERS        = 16
integer, parameter :: UG_INVALID_CRS           = 30 !< Invalid/missing coordinate reference system (using default)
integer, parameter :: UG_NOTIMPLEMENTED        = 99

!! Geometry options
integer, parameter :: LAYERTYPE_OCEANSIGMA = 1 !< Dimensionless vertical ocean sigma coordinate.
integer, parameter :: LAYERTYPE_Z          = 2 !< Vertical coordinate for fixed z-layers.

!! Location types
integer, parameter :: UG_LOC_NONE = 0 !< Mesh data location: nowhere at all (include only required mesh locations)
integer, parameter :: UG_LOC_NODE = 1 !< Mesh data location: mesh node (corner)
integer, parameter :: UG_LOC_EDGE = 2 !< Mesh data location: mesh edge
integer, parameter :: UG_LOC_FACE = 4 !< Mesh data location: mesh face
integer, parameter :: UG_LOC_VOL  = 8 !< Mesh data location: mesh volume
integer, parameter :: UG_LOC_ALL2D = UG_LOC_NODE + UG_LOC_EDGE + UG_LOC_FACE !< All three possible 2D locations.

! The following edge type codes define for each netlink (UGRID 'edge') the type (or absence) of flowlink.
integer, parameter :: UG_EDGETYPE_INTERNAL_CLOSED = 0
integer, parameter :: UG_EDGETYPE_INTERNAL        = 1
integer, parameter :: UG_EDGETYPE_BND             = 2
integer, parameter :: UG_EDGETYPE_BND_CLOSED      = 3

!! Dimension types (form a supplement to the preceding location types)
integer, parameter :: UG_DIM_MAXFACENODES = 128 !< The dimension containing the max number of nodes in the face_node_connectivity table.
! TODO: AvD: the above is not a dimension. At most it is a dimension type.

!! Basics
integer, parameter :: dp=kind(1.0d00)
integer, parameter :: maxMessageLen = 1024
character(len=maxMessageLen) :: ug_messagestr !< Placeholder string for storing diagnostic messages. /see{ug_get_message}

!> Type t_face describes a 'netcell', a cell with net nodes as vertices.
type t_face
   integer                        :: n               !< nr of nodes
   integer, allocatable           :: nod(:)          !< node nrs
   integer, allocatable           :: lin(:)          !< link nrs, kn(1 of 2,netcell(n)%lin(1)) =  netcell(n)%nod(1)  
end type t_face

!> Structure for storing all variable ids for an unstructured mesh.
type t_ug_meshids
   !
   ! Dimensions:
   !
   integer :: id_nodedim          = -1 !< Dimension ID for nodes.
   integer :: id_edgedim          = -1 !< Dimension ID for edges.
   integer :: id_facedim          = -1 !< Dimension ID for faces.
   integer :: id_maxfacenodesdim  = -1 !< Dimension ID for max nr of nodes per face.
   integer :: id_layerdim         = -1 !< Dimension ID for layer centers.
   integer :: id_interfacedim     = -1 !< Dimension ID for layer interfaces.
   integer :: id_twodim           = -1 !< Dimension ID for fixed dimension length 2.
   ! 1d network dimensions
   integer :: id_1dmeshpoints           = -1 !< Dimension ID for 1d mesh points.
   integer :: id_1dbranchesdim          = -1 !< Dimension ID for 1d network branches.
   integer :: id_1dnodesdim             = -1 !< Dimension ID for 1d network nodes.   
   integer :: id_1dgeopointsdim         = -1 !< Dimension ID for 1d network geometry points.
   integer :: id_1didstrlength          = -1 !< Dimension ID for 1d ids characters arrays.
   integer :: id_1dlongstrlength        = -1 !< Dimension ID for 1d longnames characters arrays.
   integer :: id_1dedgenodesdim         = -1 !< Dimension ID for 1d sourcetargets arrays.
         
   !
   ! Coordinate variables
   !
   integer :: id_nodex           = -1 !< Coordinate variable ID for node x-coordinate.
   integer :: id_nodey           = -1 !< Coordinate variable ID for node y-coordinate.
   integer :: id_nodez           = -1 !< Data       variable ID for node z-coordinate.
   integer :: id_nodelon         = -1 !< Coordinate variable ID for node longitude coordinate.
   integer :: id_nodelat         = -1 !< Coordinate variable ID for node latitude coordinate.

   integer :: id_edgex           = -1 !< Coordinate variable ID for edge x-coordinate.
   integer :: id_edgey           = -1 !< Coordinate variable ID for edge y-coordinate.
   integer :: id_edgexbnd        = -1 !<            variable ID for edge boundaries' x-coordinate.
   integer :: id_edgeybnd        = -1 !<            variable ID for edge boundaries' y-coordinate.
   integer :: id_edgelon         = -1 !< Coordinate variable ID for edge longitude coordinate.
   integer :: id_edgelat         = -1 !< Coordinate variable ID for edge latitude coordinate.
   integer :: id_edgelonbnd      = -1 !<            variable ID for edge boundaries' longitude coordinate.
   integer :: id_edgelatbnd      = -1 !<            variable ID for edge boundaries' latitude coordinate.

   integer :: id_facex           = -1 !< Coordinate variable ID for face x-coordinate.
   integer :: id_facey           = -1 !< Coordinate variable ID for face y-coordinate.
   integer :: id_facexbnd        = -1 !<            variable ID for face boundaries' x-coordinate.
   integer :: id_faceybnd        = -1 !<            variable ID for face boundaries' y-coordinate.
   integer :: id_facelon         = -1 !< Coordinate variable ID for face longitude coordinate.
   integer :: id_facelat         = -1 !< Coordinate variable ID for face latitude coordinate.
   integer :: id_facelonbnd      = -1 !<            variable ID for face boundaries' longitude coordinate.
   integer :: id_facelatbnd      = -1 !<            variable ID for face boundaries' latitude coordinate.

   integer :: id_layer_zs        = -1 !< Coordinate variable ID for fixed z/sigma layer center vertical coordinate (either z or sigma).
   integer :: id_interface_zs    = -1 !< Coordinate variable ID for fixed z/sigma layer interface vertical coordinate (either z or sigma).
   
   ! 1d network dimensions
   integer :: id_1dnodIds               = -1 !< Coordinate variable ID for node ids.
   integer :: id_1dnodlongnames         = -1 !< Coordinate variable ID for node longnames.
   integer :: id_1dnodex                = -1 !< Coordinate variable ID for node x coordinate.
   integer :: id_1dnodey                = -1 !< Coordinate variable ID for node y coordinate.
   integer :: id_1dgeox                 = -1 !< Coordinate variable ID for geopoints x coordinate.
   integer :: id_1dgeoy                 = -1 !< Coordinate variable ID for geopoints y coordinate.
   integer :: id_1doffset               = -1 !< Coordinate variable ID for mesh offsets.
   integer :: id_1dgeopointsperbranch   = -1 !< Coordinate variable ID for geopoints per branch.
   integer :: id_1dbranchlengths        = -1 !< Coordinate variable ID for branch lengths.
   
   !
   ! Topology variables
   !
   integer :: id_meshtopo           = -1 !< Top-level variable ID for mesh topology, collects all related variable names via attributes.
   integer :: id_edgenodes          = -1 !< Variable ID for edge-to-node mapping table.
   integer :: id_facenodes          = -1 !< Variable ID for face-to-node mapping table.
   integer :: id_edgefaces          = -1 !< Variable ID for edge-to-face mapping table (optional, can be -1).
   integer :: id_faceedges          = -1 !< Variable ID for face-to-edge mapping table (optional, can be -1).
   integer :: id_facelinks          = -1 !< Variable ID for face-to-face mapping table (optional, can be -1).
   ! 1d network topology variables
   integer :: id_1dtopo             = -1 !< Top-level variable for 1d network topology.
   integer :: id_1dedgenodes        = -1 !< Variable ID for start and end node of each branch.
   integer :: id_1dgeometry         = -1 !< Variable ID for geometry points.
   integer :: id_1dmeshtobranch     = -1 !< Variable ID for branchid of each mesh point.
   integer :: id_1dbranchids        = -1 !< Variable ID for branch ids.
   integer :: id_1dbranchlongnames  = -1 !< Variable ID for branch long names.
   integer :: id_1dmeshtopo         = -1 !< Variable ID for 1d mesh topology.
   
end type t_ug_meshids
!> Structure for storing an entire mesh geometry (topology and coordinates and more).
type t_ug_meshgeom
! TODO: AvD: extend this to 3D (volumes)
   character(len=256) :: meshname           !< Name of this mesh ! TODO: AvD: should this be in this data type?
   integer            :: dim                !< Dimensionality of the mesh (1/2/3)
   integer            :: numnode            !< Number of mesh nodes.
   integer            :: numedge            !< Number of mesh edges.
   integer            :: numface            !< Number of mesh faces.
   integer            :: maxnumfacenodes    !< Maximum of number of face nodes.
   integer            :: numlayer           !< Number of mesh layers (num interfaces == numlayer + 1), numlayer = 0 means "no layers".
   integer            :: layertype          !< Type of vertical layer definition (only if numlayer >= 1), one of LAYERTYPE_* parameters.

   integer,      pointer :: edge_nodes(:,:) !< Edge-to-node mapping array.
   integer,      pointer :: face_nodes(:,:) !< Face-to-node mapping array.
   integer,      pointer :: edge_faces(:,:) !< Edge-to-face mapping array (optional, can be null()).
   integer,      pointer :: face_edges(:,:) !< Face-to-edge mapping array (optional, can be null()).
   integer,      pointer :: face_links(:,:) !< Face-to-face mapping array (optional, can be null()).

   real(kind=dp), pointer :: nodex(:)       !< x-coordinates of the mesh nodes.
   real(kind=dp), pointer :: nodey(:)       !< y-coordinates of the mesh nodes.
   real(kind=dp), pointer :: nodez(:)       !< z-coordinates of the mesh nodes.
   real(kind=dp), pointer :: edgex(:)       !< x-coordinates of the mesh edges.
   real(kind=dp), pointer :: edgey(:)       !< y-coordinates of the mesh edges.
   real(kind=dp), pointer :: edgez(:)       !< z-coordinates of the mesh edges.
   real(kind=dp), pointer :: facex(:)       !< x-coordinates of the mesh faces.
   real(kind=dp), pointer :: facey(:)       !< y-coordinates of the mesh faces.
   real(kind=dp), pointer :: facez(:)       !< z-coordinates of the mesh faces.

   real(kind=dp), pointer :: layer_zs(:)     !< Vertical coordinates of the mesh layers' center (either z or sigma).
   real(kind=dp), pointer :: interface_zs(:) !< Vertical coordinates of the mesh layers' interface (either z or sigma).

   type(t_crs),  pointer :: crs           !< Map projection/coordinate transformation used for the coordinates of this mesh.
end type t_ug_meshgeom

type t_ug_file
   character(len=256)               :: filename
   integer                          :: nummesh
   type(t_ug_meshids), allocatable  :: meshids(:)      !< The struct with underlying variable IDs, one for each mesh topology.
   character(len=256), allocatable  :: meshnames(:)    !< The variable names for all mesh topologies in file.
end type t_ug_file

   contains

!> Returns the latest message string from this module.
!!
!! Use this when a previous function call has returned a nonzero error status.
!! Call this function only once for each returned error: message buffer will be cleared on each call.
integer function ug_get_message(str) result(ierr)
   character(len=*), intent(out) :: str !< String variable in which the message will be stored.

   ierr = UG_NOERR
   str = trim(ug_messagestr)

   ! Directly clear the message buffer, to prevent false messages for future errors.
   ug_messagestr = ' '

end function ug_get_message

!
! -- Writing-related routines ---------------------------------------------
!

!> Puts global attributes in an open NetCDF data set.
!! This includes: institution, Conventions, etc.
function ug_addglobalatts(ncid, meta) result(ierr)
   integer, intent(in)           :: ncid              !< Already opened NetCDF id to put global attributes into.
   type (t_ug_meta), intent (in)   :: meta
   integer          :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.

   character(len=8)  :: cdate
   character(len=10) :: ctime
   character(len=5)  :: czone
   integer :: wasInDefine

   ierr = UG_NOERR
   wasInDefine = 0

   ierr = nf90_redef(ncid)
   if (ierr == nf90_eindefine) wasInDefine = 1 ! Was still in define mode.
   if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
      ug_messagestr = 'Could not put global attributes in NetCDF'
      return
   end if

   ierr = nf90_put_att(ncid, nf90_global,  'institution', trim(meta%institution))
   ierr = nf90_put_att(ncid, nf90_global,  'references',  trim(meta%references))
   ierr = nf90_put_att(ncid, nf90_global,  'source',      trim(meta%source)//' '//trim(meta%version)//'. Model: '//trim(meta%modelname))

   call date_and_time(cdate, ctime, czone)
   ierr = nf90_put_att(ncid, nf90_global,  'history', &
      'Created on '//cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)// &
      ', '//trim(meta%source))

   ierr = nf90_put_att(ncid, nf90_global,  'Conventions', trim(UG_CONV_CF)//' '//trim(UG_CONV_UGRID)//'/'//trim(UG_CONV_DELTARES))

   ! Leave the dataset in the same mode as we got it.
   if (wasInDefine == 0) then
      ierr = nf90_enddef(ncid)
   end if
end function ug_addglobalatts


!> Gets all NetCDF-attributes for a given variable.
!!
!! This function is non-UGRID-specific: only used to read grid mapping variables.
!! @see ug_put_var_attset
function ug_get_var_attset(ncid, varid, attset) result(ierr)
   integer,                         intent(in)  :: ncid      !< NetCDF dataset id
   integer,                         intent(in)  :: varid     !< NetCDF variable id
   type(nc_attribute), allocatable, intent(out) :: attset(:) !< Resulting attribute set.
   integer                                      :: ierr      !< Result status (UG_NOERR==NF90_NOERR) if successful.

   character(len=64) :: attname
   character(len=1024) :: tmpstr
   integer :: i, j, natts, atttype, attlen, nlen

   ierr = UG_NOERR

   ierr = nf90_inquire_variable(ncid, varid, natts = natts)
   if (ierr /= nf90_noerr) then
      goto 888
   end if

   if (allocated(attset)) deallocate(attset)
   allocate(attset(natts), stat=ierr)

   do i = 1,natts
      ierr = nf90_inq_attname(ncid, varid, i, attname)    ! get attribute name
      ierr = nf90_inquire_attribute(ncid, varid, trim(attname), xtype = atttype, len=attlen) ! get other attribute information

      select case(atttype)
      case(NF90_CHAR)
         tmpstr = ''
         ierr = nf90_get_att(ncid, varid, attname, tmpstr)

         allocate(attset(i)%strvalue(attlen))

         nlen = min(len(tmpstr), attlen)
         do j=1,nlen
            attset(i)%strvalue(j) = tmpstr(j:j)
         end do
      case(NF90_INT)
         allocate(attset(i)%intvalue(attlen))
         ierr = nf90_get_att(ncid, varid, attname, attset(i)%intvalue)
      case(NF90_FLOAT)
         allocate(attset(i)%fltvalue(attlen))
         ierr = nf90_get_att(ncid, varid, attname, attset(i)%fltvalue)
      case(NF90_DOUBLE)
         allocate(attset(i)%dblvalue(attlen))
         ierr = nf90_get_att(ncid, varid, attname, attset(i)%dblvalue)
      case default
         ! NF90_BYTE
         ! NF90_SHORT
         ug_messagestr = 'ug_get_var_attset: error for attribute '''//trim(attname)//'''. Data types byte/short not implemented.'
         ierr = UG_NOTIMPLEMENTED
         goto 888
      end select
      attset(i)%attname = attname
      attset(i)%xtype   = atttype
      attset(i)%len     = attlen
   end do

   return ! Return with success

888 continue
    
end function ug_get_var_attset


!> Puts a set of NetCDF-attributes onto a given variable.
!!
!! This function is non-UGRID-specific: only used to write grid mapping variables.
!! @see ug_get_var_attset
function ug_put_var_attset(ncid, varid, attset) result(ierr)
   integer,             intent(in)  :: ncid      !< NetCDF dataset id
   integer,             intent(in)  :: varid     !< NetCDF variable id
   type(nc_attribute),  intent(in)  :: attset(:) !< Attribute set to be put into the variable.
   integer                          :: ierr      !< Result status (UG_NOERR==NF90_NOERR) if successful.

   character(len=1024) :: tmpstr
   integer :: i, j, natts, nlen

   ierr = UG_NOERR

   natts = size(attset)

   do i = 1,natts
      select case(attset(i)%xtype)
      case(NF90_CHAR)
         tmpstr = ' '
         nlen = min(len(tmpstr), attset(i)%len)
         do j=1,nlen
            tmpstr(j:j) = attset(i)%strvalue(j)
         end do

         ierr = nf90_put_att(ncid, varid, attset(i)%attname, tmpstr)
      case(NF90_INT)
         ierr = nf90_put_att(ncid, varid, attset(i)%attname, attset(i)%intvalue(1:attset(i)%len))
      case(NF90_FLOAT)
         ierr = nf90_put_att(ncid, varid, attset(i)%attname, attset(i)%fltvalue(1:attset(i)%len))
      case(NF90_DOUBLE)
         ierr = nf90_put_att(ncid, varid, attset(i)%attname, attset(i)%dblvalue(1:attset(i)%len))
      case default
         ! NF90_BYTE
         ! NF90_SHORT
         ug_messagestr = 'ug_put_var_attset: error for attribute '''//trim(attset(i)%attname)//'''. Data types byte/short not implemented.'
         ierr = UG_NOTIMPLEMENTED
      end select
   end do

end function ug_put_var_attset


!> Creates/initializes an empty mesh geometry.
!! This function could also have been called ug_init_meshgeom.
!!
!! NOTE: do not pass already filled mesh geometries to this function,
!! since array pointers will become disassociated, possibly causing
!! memory leaks.
function ug_new_meshgeom(meshgeom) result(ierr)
   type(t_ug_meshgeom), intent(out) :: meshgeom !< The mesh geometry that is to be created.
   integer                          :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.

   ierr = UG_NOERR

   meshgeom%dim    = 0 
   meshgeom%numNode = 0 
   meshgeom%numEdge = 0 
   meshgeom%numFace = 0 
   meshgeom%numlayer = 0
   meshgeom%layertype = -1

   meshgeom%edge_nodes => null()
   meshgeom%face_nodes => null()
   meshgeom%edge_faces => null()
   meshgeom%face_edges => null()
   meshgeom%face_links => null()

   meshgeom%nodex => null()
   meshgeom%nodey => null()
   meshgeom%nodez => null()

   meshgeom%edgex => null()
   meshgeom%edgey => null()
   meshgeom%edgez => null()

   meshgeom%facex => null()
   meshgeom%facey => null()
   meshgeom%facez => null()

   meshgeom%layer_zs     => null()
   meshgeom%interface_zs => null()

   meshgeom%crs   => null()

end function ug_new_meshgeom



! -- COORDINATES ------------
!> Adds coordinate variables according to CF conventions.
!! Non-standard attributes (such as bounds) should be set elsewhere.
function ug_addcoordvars(ncid, id_varx, id_vary, id_dimension, name_varx, name_vary, longname_varx, longname_vary, mesh, location, crs) result(ierr)
   integer,               intent(in)    :: ncid          !< NetCDF dataset id
   integer,               intent(inout) :: id_varx       !< NetCDF 'x' variable id
   integer,               intent(inout) :: id_vary       !< NetCDF 'y' variable id
   integer, dimension(:), intent(in)    :: id_dimension  !< NetCDF dimension id
   character(len=*),      intent(in)    :: name_varx     !< NetCDF 'x' variable name
   character(len=*),      intent(in)    :: name_vary     !< NetCDF 'y' variable name
   character(len=*),      intent(in)    :: longname_varx !< NetCDF 'x' variable long name
   character(len=*),      intent(in)    :: longname_vary !< NetCDF 'y' variable long name
   character(len=*),      intent(in)    :: mesh          !< Name of the mesh that contains the coordinate variables to add
   character(len=*),      intent(in)    :: location      !< location on the mesh of the coordinate variables to add
   type(t_crs),           intent(in)    :: crs           !< Coordinate reference system for the x/y-coordinates variables.
   integer                              :: ierr          !< Result status (UG_NOERR==NF90_NOERR) if successful.

   ierr = UG_NOERR

   ierr = nf90_def_var(ncid, name_varx, nf90_double, id_dimension, id_varx)
   ierr = nf90_def_var(ncid, name_vary, nf90_double, id_dimension, id_vary)
   ierr = ug_addcoordatts(ncid, id_varx, id_vary, crs)
   ierr = nf90_put_att(ncid, id_varx, 'mesh',      mesh)
   ierr = nf90_put_att(ncid, id_vary, 'mesh',      mesh)
   ierr = nf90_put_att(ncid, id_varx, 'location',  location)
   ierr = nf90_put_att(ncid, id_vary, 'location',  location)
   ierr = nf90_put_att(ncid, id_varx, 'long_name', longname_varx)
   ierr = nf90_put_att(ncid, id_vary, 'long_name', longname_vary)
end function ug_addcoordvars

!> Adds WGS84 coordinate variables according to CF conventions.
!! Non-standard attributes (such as bounds) should be set elsewhere.
function ug_addlonlatcoordvars(ncid, id_varlon, id_varlat, id_dimension, name_varlon, name_varlat, longname_varlon, longname_varlat, mesh, location) result(ierr)
   integer,               intent(in)    :: ncid            !< NetCDF dataset id
   integer,               intent(inout) :: id_varlon       !< NetCDF 'lon' variable id
   integer,               intent(inout) :: id_varlat       !< NetCDF 'lat' variable id
   integer, dimension(:), intent(in)    :: id_dimension    !< NetCDF dimension id
   character(len=*),      intent(in)    :: name_varlon     !< NetCDF 'lon' variable name
   character(len=*),      intent(in)    :: name_varlat     !< NetCDF 'lat' variable name
   character(len=*),      intent(in)    :: longname_varlon !< NetCDF 'lon' variable long name
   character(len=*),      intent(in)    :: longname_varlat !< NetCDF 'lat' variable long name
   character(len=*),      intent(in)    :: mesh            !< Name of the mesh that contains the coordinate variables to add
   character(len=*),      intent(in)    :: location        !< location on the mesh of the coordinate variables to add
   integer                              :: ierr            !< Result status (UG_NOERR==NF90_NOERR) if successful.

   ierr = UG_NOERR

   ierr = nf90_def_var(ncid, name_varlon, nf90_double, id_dimension, id_varlon)
   ierr = nf90_def_var(ncid, name_varlat, nf90_double, id_dimension, id_varlat)
   ierr = ug_addlonlatcoordatts(ncid, id_varlon, id_varlat)
   ierr = nf90_put_att(ncid, id_varlon, 'mesh',      mesh)
   ierr = nf90_put_att(ncid, id_varlat, 'mesh',      mesh)
   ierr = nf90_put_att(ncid, id_varlon, 'location',  location)
   ierr = nf90_put_att(ncid, id_varlat, 'location',  location)
   ierr = nf90_put_att(ncid, id_varlon, 'long_name', longname_varlon)
   ierr = nf90_put_att(ncid, id_varlat, 'long_name', longname_varlat)
end function ug_addlonlatcoordvars

!> Adds coordinate attributes according to CF conventions, based on given coordinate projection type.
!! Non-standard attributes (such as long_name) should be set elsewhere.
function ug_addcoordatts(ncid, id_varx, id_vary, crs) result(ierr)
   integer,      intent(in) :: ncid     !< NetCDF dataset id
   integer,      intent(in) :: id_varx  !< NetCDF 'x' variable id
   integer,      intent(in) :: id_vary  !< NetCDF 'y' variable id
   type(t_crs),  intent(in) :: crs      !< Coordinate reference system for the x/y-coordinates variables.
   integer                  :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.

   ierr = UG_NOERR

   if (crs%is_spherical) then ! If WGS84 system.
      ierr = ug_addlonlatcoordatts(ncid, id_varx, id_vary)
   else ! If projected crs.
      ierr = nf90_put_att(ncid, id_varx, 'units',       'm')
      ierr = nf90_put_att(ncid, id_vary, 'units',       'm')
      ierr = nf90_put_att(ncid, id_varx, 'standard_name', 'projection_x_coordinate')
      ierr = nf90_put_att(ncid, id_vary, 'standard_name', 'projection_y_coordinate')
      ierr = nf90_put_att(ncid, id_varx, 'long_name'   , 'x')
      ierr = nf90_put_att(ncid, id_vary, 'long_name'   , 'y')
   end if
end function ug_addcoordatts

!> Adds WGS84 coordinate attributes according to CF conventions.
function ug_addlonlatcoordatts(ncid, id_varlon, id_varlat) result(ierr)
   integer, intent(in) :: ncid      !< NetCDF dataset id
   integer, intent(in) :: id_varlon !< NetCDF 'longitude' variable id
   integer, intent(in) :: id_varlat !< NetCDF 'latitude' variable id
   integer             :: ierr      !< Result status (UG_NOERR==NF90_NOERR) if successful.

   ierr = UG_NOERR

   ierr = nf90_put_att(ncid, id_varlon, 'units',       'degrees_east')
   ierr = nf90_put_att(ncid, id_varlat, 'units',       'degrees_north')
   ierr = nf90_put_att(ncid, id_varlon, 'standard_name', 'longitude')
   ierr = nf90_put_att(ncid, id_varlat, 'standard_name', 'latitude')
   ierr = nf90_put_att(ncid, id_varlon, 'long_name'   , 'longitude')
   ierr = nf90_put_att(ncid, id_varlat, 'long_name'   , 'latitude')
end function ug_addlonlatcoordatts

!> Adds coordinate mapping attributes according to CF conventions, based on jsferic.
!! Attributes are put in a scalar integer variable.
function ug_add_coordmapping(ncid, crs) result(ierr)
   integer,      intent(in) :: ncid  !< NetCDF dataset id
   type(t_crs),  intent(in) :: crs   !< Coordinate reference system that was used for the coordinate mapping.
   integer                  :: ierr  !< Result status (UG_NOERR==NF90_NOERR) if successful.

   integer :: id_crs
   integer :: epsg
   integer :: ierr_missing
   character(len=11) :: epsgstring
   character(len=30) :: varname  !< Name of the created grid mapping variable.

   ierr = UG_NOERR
   ierr_missing = UG_NOERR ! Store whether crs was missing (and default was used)

   epsgstring = ' '

   varname = ' '
   if (len_trim(crs%varname) > 0) then
      varname = crs%varname
   else if (crs%is_spherical) then
      ierr_missing = UG_INVALID_CRS
      varname = 'wgs84'
   else
      ierr_missing = UG_INVALID_CRS
      varname = 'projected_coordinate_system'
   end if

   ierr = nf90_inq_varid(ncid, trim(varname), id_crs)
   if (ierr == nf90_noerr) then
      ! A variable with that name already exists. Return without error.
      ierr = UG_NOERR
      goto 888
   end if

   ierr = nf90_def_var(ncid, trim(varname), nf90_int, id_crs)

   if (allocated(crs%attset)) then
      ierr = ug_put_var_attset(ncid, id_crs, crs%attset)
   elseif (crs%is_spherical) then
      ierr_missing = UG_INVALID_CRS
      epsg      = 4326
      epsgstring = 'EPSG:4326'
      ierr = nf90_put_att(ncid, id_crs, 'name',                       'WGS84'             ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'epsg',                       epsg                ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'grid_mapping_name',          'latitude_longitude') ! CF
      ierr = nf90_put_att(ncid, id_crs, 'longitude_of_prime_meridian', 0d0                ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'semi_major_axis',             6378137d0          ) ! CF 
      ierr = nf90_put_att(ncid, id_crs, 'semi_minor_axis',             6356752.314245d0   ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'inverse_flattening',          298.257223563d0    ) ! CF
!      ierr = nf90_put_att(ncid, id_crs, 'proj4_params',                ' '                ) ! ADAGUC
      ierr = nf90_put_att(ncid, id_crs, 'epsg_code',                   trim(epsgstring)   ) ! ADAGUC
!      ierr = nf90_put_att(ncid, id_crs, 'projection_name',             ' '                ) ! ADAGUC
!      ierr = nf90_put_att(ncid, id_crs, 'wkt',                         ' '                ) ! WKT
!      ierr = nf90_put_att(ncid, id_crs, 'comment',                     ' '                )
      ierr = nf90_put_att(ncid, id_crs, 'value',                       'value is equal to EPSG code')
   else
      ierr_missing = UG_INVALID_CRS
      epsg      = crs%epsg_code
      ! TODO: remove hardcoded defaults below. Replace by cloning the crs%attset  into this new NetCDF var.
      write (epsgstring, '("EPSG:",I0)') epsg
      ierr = nf90_put_att(ncid, id_crs, 'name',                        'Unknown projected' ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'epsg',                        epsg                ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'grid_mapping_name',           'Unknown projected' ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'longitude_of_prime_meridian', 0d0                 ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'semi_major_axis',             6378137d0           ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'semi_minor_axis',             6356752.314245d0    ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'inverse_flattening',          298.257223563d0     ) ! CF
!      ierr = nf90_put_att(ncid, id_crs, 'proj4_params',                ' '                 ) ! ADAGUC
      ierr = nf90_put_att(ncid, id_crs, 'EPSG_code',                   trim(epsgstring)    ) ! ADAGUC
!      ierr = nf90_put_att(ncid, id_crs, 'projection_name',             ' '                 ) ! ADAGUC
!      ierr = nf90_put_att(ncid, id_crs, 'wkt',                         ' '                 ) ! WKT
!      ierr = nf90_put_att(ncid, id_crs, 'comment',                     ' '                 )
      ierr = nf90_put_att(ncid, id_crs, 'value',                       'value is equal to EPSG code')
   end if

   if (ierr_missing /= UG_NOERR) then
      ierr = ierr_missing
      ug_messagestr = 'Missing coordinate reference system. Now using default: '//trim(varname)//' ('//trim(epsgstring)//').'
      ! But continue...
   end if

   ! Check for any remaining native NetCDF errors
   if (ierr /= nf90_noerr) then
      goto 888
   end if

   ierr = UG_NOERR
   return ! Return with success

   ! TODO: AvD: actual epsg value is not put in variable value yet (redef stuff)

888 continue

end function ug_add_coordmapping


!> Add the grid mapping attribute to one or more NetCDF variables.
function ug_put_gridmapping_att(ncid, id_vars, crs) result(ierr)
   integer,               intent(in) :: ncid     !< NetCDF dataset id
   integer, dimension(:), intent(in) :: id_vars  !< Array of NetCDF variable ids
   type(t_crs),           intent(in) :: crs      !< Projection type that was used for the coordinate mapping.
   integer                           :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.

   integer :: i, n
   character(len=30)  :: gridmappingvar           !< Name of grid mapping variable

   ierr = UG_SOMEERR

   gridmappingvar = ' '
   if (.true.) then
      gridmappingvar = crs%varname
   else if (crs%is_spherical) then
      gridmappingvar = 'wgs84'
   else
      gridmappingvar = 'projected_coordinate_system'
   end if

   ierr = UG_NOERR
   n   = size(id_vars)

   do i=1,n
      ierr = nf90_put_att(ncid, id_vars(i), 'grid_mapping', trim(gridmappingvar))
      if (ierr /= nf90_noerr) then
         goto 888
      end if
   end do

   ierr = UG_NOERR
   return ! Return with success

888 continue

end function ug_put_gridmapping_att



!> Checks whether a specific mesh data location is inside a location specification code.
!! Mesh data may be specified on nodes (corners), edges and faces, encoded as a sum of location codes.
!! Used to decide which optional mesh topology data should be written to file, and which not.
!! \see UG_LOC_NODE, UG_LOC_EDGE, UG_LOC_FACE
function ug_checklocation(dataLocsCode, locType) result(is_used)
   integer, intent(in) :: dataLocsCode  !< Integer code describing on which topological locations data is/will be used.
   integer, intent(in) :: locType       !< Integer location code to test on (one of UG_LOC_NODE, UG_LOC_EDGE, UG_LOC_FACE, UG_LOC_VOL).
   logical             :: is_used       !< Returns whether specified locType is contained in dataLocsCode.

   ! Perform logical AND to determine whether locType is inside dataLocs 'set'.
   is_used = iand(dataLocsCode, locType) == locType
end function ug_checklocation



!> Translates the string name of a topological location into the integer location type.
subroutine ug_location_to_loctype(locName, locType)
   character(len=*), intent(in)    :: locName !< String name of the location, e.g., as read from a :location attribute value.
   integer,          intent(  out) :: locType !< Integer location code (one of UG_LOC_NODE, UG_LOC_EDGE, UG_LOC_FACE, UG_LOC_VOL).

   select case (trim(locName))
   case ('face')
      locType = UG_LOC_FACE
   case ('edge')
      locType = UG_LOC_EDGE
   case ('node')
      locType = UG_LOC_NODE
   case ('volume')
      locType = UG_LOC_VOL
   case default
      locType = UG_LOC_NONE
   end select   

end subroutine ug_location_to_loctype


!> Write mesh topoplogy
!! This only writes the mesh topology variable, not the other variables that are part of the mesh.
function ug_write_meshtopology(ncid, meshids, meshName, dim, dataLocsCode, add_edge_face_connectivity, add_face_edge_connectivity, add_face_face_connectivity, add_layers) result(ierr)
   implicit none

   integer,          intent(in) :: ncid         !< NetCDF dataset id
   type(t_ug_meshids), intent(inout) :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
   character(len=*), intent(in) :: meshName     !< Name for the mesh variable, also used as prefix for all related entities.
   integer,          intent(in) :: dim          !< Dimensionality of the mesh (1/2/3)
   integer,          intent(in) :: dataLocsCode !< Specifies at which mesh locations data may be specified.
   logical,          intent(in) :: add_edge_face_connectivity !< Specifies whether edge_face_connectivity should be added.
   logical,          intent(in) :: add_face_edge_connectivity !< Specifies whether face_edge_connectivity should be added.
   logical,          intent(in) :: add_face_face_connectivity !< Specifies whether face_face_connectivity should be added.
   logical,          intent(in) :: add_layers   !< Specifies whether layer and interface vertical dimensions should be added.
   integer                      :: ierr         !< Result status (UG_NOERR==NF90_NOERR) if successful.

   character(len=len_trim(meshName)) :: prefix
   character(len=256) :: buffer

   ierr = UG_SOMEERR

   prefix = trim(meshName)

   if (len_trim(meshName) == 0) then
      ierr = UG_INVALID_MESHNAME
      goto 888
   end if

   if (dim <=0 .or. dim > 3) then
      ierr = UG_INVALID_MESHDIMENSION
      goto 888
   end if

   ! TODO: AvD: check for conflicts between dataLocsCode and dim (e.g. FACE data in a 1D model)

   ! Define the mesh topology variable
   ierr = nf90_def_var(ncid, prefix, nf90_int, meshids%id_meshtopo)

   ! Attributes for all dimensions:
   ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'cf_role',       'mesh_topology')
   write(buffer, '(a,i0,a)') 'Topology data of ', dim, 'D network'
   ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'long_name',      trim(buffer))
   ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'topology_dimension',      dim)
   ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'node_coordinates', prefix//'_node_x '//prefix//'_node_y')
   ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'node_dimension', 'n'//prefix//'_node')
   ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'max_face_nodes_dimension', 'max_n'//prefix//'_face_nodes') ! non ugrid standard!
   
   ! 1D: required, 2D: optionally required if data there
   if (dim == 1 .or. ug_checklocation(dataLocsCode, UG_LOC_EDGE)) then
      ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'edge_node_connectivity', prefix//'_edge_nodes')
      ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'edge_dimension', 'n'//prefix//'_edge')
   end if

   ! Optionally required if data there:
   if (ug_checklocation(dataLocsCode, UG_LOC_EDGE)) then
      ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'edge_coordinates',      prefix//'_edge_x '//prefix//'_edge_y')
   end if

   ! 2D: required, 3D: optionally required if data there:
   if (dim == 2 .or. ug_checklocation(dataLocsCode, UG_LOC_FACE)) then
      ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'face_node_connectivity', prefix//'_face_nodes')
      ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'face_dimension', 'n'//prefix//'_face')
      if (add_face_edge_connectivity) then
         ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'face_edge_connectivity', prefix//'_face_edges')
      end if
      if (add_face_face_connectivity) then
         ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'face_face_connectivity', prefix//'_face_links')
      end if
      ! Note that edge_face_connectivity is not officially part of the UGRID conventions, however it is very similar to e.g. face_edge_connectivity, which is part of the UGRID conventions.
      if (add_edge_face_connectivity) then
         ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'edge_face_connectivity', prefix//'_edge_faces')
      end if
   end if

   ! Optionally required if data there:
   if (ug_checklocation(dataLocsCode, UG_LOC_FACE)) then
      ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'face_coordinates', prefix//'_face_x '//prefix//'_face_y')
   end if

   ! Optionally required if layers present (1D or 2D layered mesh topology):
   if (add_layers) then
      ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'layer_dimension',     'n'//prefix//'_layer')
      ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'interface_dimension', 'n'//prefix//'_interface')
   end if

   if (dim >= 3) then
      ierr = UG_NOTIMPLEMENTED
      goto 888
   end if

   ! Check for any remaining native NetCDF errors
   if (ierr /= nf90_noerr) then
      goto 888
   end if

   ierr = UG_NOERR
   return ! Return with success

888 continue

end function ug_write_meshtopology

!> Defines a new variable in an existing dataset.
!! Does not write the actual data yet.
function ug_def_var(ncid, meshids, id_var, id_dims, itype, iloc, mesh_name, var_name, standard_name, long_name, &
                    unit, cell_method, crs, ifill, dfill) result(ierr)
   integer,                 intent(in)    :: ncid          !< NetCDF dataset id
   type(t_ug_meshids),      intent(in)    :: meshids       !< Set of NetCDF-ids for all mesh geometry arrays.
   integer,                 intent(out)   :: id_var        !< Created NetCDF variable id.
   integer, dimension(:),   intent(in)    :: id_dims       !< NetCDF dimension ids for this variable. Example: (/ id_edgedim /) for scalar data on edges, or (/ 2, id_facedim /) for vector data on faces.
   integer,                 intent(in)    :: itype         !< The variable type expressed in one of the basic nf90_* types, e.g., nf90_double.
   integer,                 intent(in)    :: iloc          !< Specifies at which unique mesh location data will be specified.
   character(len=*),        intent(in)    :: mesh_name     !< Name for the mesh variable, also used as prefix for all related entities.
   character(len=*),        intent(in)    :: var_name      !< Name for the new data variable.
   character(len=*),        intent(in)    :: standard_name !< Standard name (CF-compliant) for 'standard_name' attribute in this variable.
   character(len=*),        intent(in)    :: long_name     !< Long name for 'long_name' attribute in this variable (use empty string if not wanted).
   character(len=*),        intent(in)    :: unit          !< Unit of this variable (CF-compliant) (use empty string for dimensionless quantities).
   character(len=*),        intent(in)    :: cell_method   !< Cell method for the spatial dimension (i.e., for edge/face/volume), value should be one of 'point', 'mean', etc. (See CF) (empty string if not relevant).
   type(t_crs),      optional, intent(in)    :: crs           !< (Optional) Add grid_mapping attribute based on this coordinate reference system for independent coordinates
   integer,          optional, intent(in)    :: ifill         !< (Optional) Integer fill value.
   double precision, optional, intent(in)    :: dfill         !< (Optional) Double precision fill value.
   integer                                :: ierr          !< Result status (UG_NOERR==NF90_NOERR) if successful.

   character(len=len_trim(mesh_name)) :: prefix

   ierr = UG_SOMEERR

   prefix = trim(mesh_name)
   
   ierr = nf90_def_var(ncid, prefix//'_'//trim(var_name), itype, id_dims, id_var)
   ierr = nf90_put_att(ncid, id_var, 'mesh',   trim(mesh_name))
   select case (iloc)
   case (UG_LOC_NODE)
      ierr = nf90_put_att(ncid, id_var, 'location',    'node')
      ierr = nf90_put_att(ncid, id_var, 'coordinates', prefix//'_node_x '//prefix//'_node_y')
   case (UG_LOC_EDGE)
      ierr = nf90_put_att(ncid, id_var, 'location',    'edge')
      ierr = nf90_put_att(ncid, id_var, 'coordinates', prefix//'_edge_x '//prefix//'_edge_y')
      if (len_trim(cell_method) > 0) then
         ierr = nf90_put_att(ncid, id_var, 'cell_methods', 'n'//prefix//'_edge: '//trim(cell_method))
      end if
   case (UG_LOC_FACE)
      ierr = nf90_put_att(ncid, id_var, 'location',    'face')
      ierr = nf90_put_att(ncid, id_var, 'coordinates', prefix//'_face_x '//prefix//'_face_y')
      if (len_trim(cell_method) > 0) then
         ierr = nf90_put_att(ncid, id_var, 'cell_methods', 'n'//prefix//'_face: '//trim(cell_method))
      end if
   case (UG_LOC_VOL)
      ierr = UG_NOTIMPLEMENTED
      goto 888
   case default
      ierr = UG_INVALID_DATALOCATION
      goto 888
   end select

   ierr = nf90_put_att(ncid, id_var, 'standard_name', trim(standard_name))
   ierr = nf90_put_att(ncid, id_var, 'long_name'    , trim(long_name))
   ierr = nf90_put_att(ncid, id_var, 'units'        , trim(unit))

   if (present(crs)) then
      ierr = ug_put_gridmapping_att(ncid, (/ id_var /), crs)
   endif
   if (itype == nf90_int .and. present(ifill)) then
      ierr = nf90_put_att(ncid, id_var, '_FillValue'   , ifill)
   end if
   if (itype == nf90_double .and. present(dfill)) then
      ierr = nf90_put_att(ncid, id_var, '_FillValue'   , dfill)
   end if

   ! Check for any remaining native NetCDF errors
   if (ierr /= nf90_noerr) then
      goto 888
   end if

   ierr = UG_NOERR
   return ! Return with success

888 continue


end function ug_def_var


!> Writes a complete mesh geometry to an open NetCDF data set.
!! The mesh geometry is the required starting point for all variables/data defined ON that mesh.
!! This function accepts the mesh geometry derived type as input, for the arrays-based function, see ug_write_mesh_arrays
!! This only writes the mesh variables, not the actual data variables that are defined ON the mesh.
function ug_write_mesh_struct(ncid, meshids, meshgeom) result(ierr)
   integer,             intent(in   ) :: ncid     !< NetCDF dataset id, should be already open and ready for writing.
   type(t_ug_meshids),  intent(inout) :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
   type(t_ug_meshgeom), intent(in   ) :: meshgeom !< The complete mesh geometry in a single struct.
   integer                            :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.

   ierr = ug_write_mesh_arrays(ncid, meshids, meshgeom%meshName, meshgeom%dim, UG_LOC_ALL2D, meshgeom%numNode, meshgeom%numEdge, meshgeom%numFace, meshgeom%maxNumFaceNodes, &
                               meshgeom%edge_nodes, meshgeom%face_nodes, meshgeom%edge_faces, meshgeom%face_edges, meshgeom%face_links, meshgeom%nodex, meshgeom%nodey, & ! meshgeom%nodez, &
                               meshgeom%edgex, meshgeom%edgey, meshgeom%facex, meshgeom%facey, &
                               meshgeom%crs, -999, -999d0, meshgeom%numlayer, meshgeom%layertype, meshgeom%layer_zs, meshgeom%interface_zs)
end function ug_write_mesh_struct

!> Writes a complete mesh geometry to an open NetCDF data set based on separate arrays with all mesh data.
!! The mesh geometry is the required starting point for all variables/data defined ON that mesh.
!! This function requires all mesh arrays as input, for the derived type-based function, see ug_write_mesh_struct.
!! This only writes the mesh variables, not the actual data variables that are defined ON the mesh.
function ug_write_mesh_arrays(ncid, meshids, meshName, dim, dataLocs, numNode, numEdge, numFace, maxNumNodesPerFace, &
                              edge_nodes, face_nodes, edge_faces, face_edges, face_links, xn, yn, xe, ye, xf, yf, &
                              crs, imiss, dmiss, numLayer, layerType, layer_zs, interface_zs) result(ierr)

   use m_alloc

   implicit none

   integer,          intent(in) :: ncid     !< NetCDF dataset id, should be already open and ready for writing.
   type(t_ug_meshids), intent(inout) :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
   character(len=*), intent(in) :: meshName !< Name for the mesh variable, also used as prefix for all related entities.
   integer,          intent(in) :: dim      !< Dimensionality of the mesh (1/2/3)
   integer,          intent(in) :: dataLocs !< Integer code describing on which topological locations data is/will be used.
   integer,          intent(in) :: numNode  !< Number of nodes in the mesh.
   integer,          intent(in) :: numEdge  !< Number of edges in the mesh.
   integer,          intent(in) :: numFace  !< Number of faces in the mesh.
   integer,          intent(in) :: maxNumNodesPerFace  !< Maximum number of nodes per face in the mesh.
   integer,          intent(in) :: edge_nodes(:,:) !< Edge-to-node mapping array.
   integer,          intent(in) :: face_nodes(:,:) !< Face-to-node mapping array.
   integer, pointer, intent(in) :: edge_faces(:,:) !< Edge-to-face mapping array (optional, can be null()).
   integer, pointer, intent(in) :: face_edges(:,:) !< Face-to-edge mapping array (optional, can be null()).
   integer, pointer, intent(in) :: face_links(:,:) !< Face-to-face mapping array (optional, can be null()).
   real(kind=dp),    intent(in) :: xn(:), yn(:) !< x,y-coordinates of the mesh nodes.
   real(kind=dp),    intent(in) :: xe(:), ye(:) !< representative x,y-coordinates of the mesh edges.
   real(kind=dp),    intent(in) :: xf(:), yf(:) !< representative x,y-coordinates of the mesh faces.
   type(t_crs),      intent(in) :: crs      !< Coordinate reference system for input coordinates
   integer,          intent(in) :: imiss    !< Fill value used for integer values (e.g. in edge/face_nodes arrays).
   real(kind=dp),    intent(in) :: dmiss    !< Fill value used for double precision values (e.g. in face_x_bnd variable).
   integer, optional,       intent(in) :: numLayer  !< Number of vertical layers in the mesh. Optional.
   integer, optional,       intent(in) :: layerType !< Type of vertical layering in the mesh. One of LAYERTYPE_* parameters. Optional, only used if numLayer >= 1.
   real(kind=dp), optional, intent(in) :: layer_zs(:)     !< Vertical coordinates of the mesh layers' center (either z or sigma). Optional, only used if numLayer >= 1.
   real(kind=dp), optional, intent(in) :: interface_zs(:) !< Vertical coordinates of the mesh layers' interface (either z or sigma). Optional, only used if numLayer >= 1.
   integer                      :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.
      
   real(kind=dp), allocatable :: edgexbnd(:,:), edgeybnd(:,:), facexbnd(:,:), faceybnd(:,:)
   real(kind=dp), allocatable :: lonn(:), latn(:) !< lon,lat-coordinates of the mesh nodes.
   real(kind=dp), allocatable :: lone(:), late(:) !< representative lon,lat-coordinates of the mesh edges.
   real(kind=dp), allocatable :: lonf(:), latf(:) !< representative lon,lat-coordinates of the mesh faces.
   real(kind=dp), allocatable :: edgelonbnd(:,:), edgelatbnd(:,:), facelonbnd(:,:), facelatbnd(:,:)
   integer :: maxnv, k, n
   character(len=len_trim(meshName)) :: prefix
   integer :: wasInDefine
   logical :: add_edge_face_connectivity !< Specifies whether edge_face_connectivity should be added.
   logical :: add_face_edge_connectivity !< Specifies whether face_edge_connectivity should be added.
   logical :: add_face_face_connectivity !< Specifies whether face_face_connectivity should be added.
   logical :: add_layers                 !< Specifies whether layer and interface vertical dimensions should be added.

   ierr = UG_SOMEERR
   wasInDefine = 0

   ierr = nf90_redef(ncid)
   if (ierr == nf90_eindefine) wasInDefine = 1 ! Was still in define mode.

   prefix=trim(meshName)

   add_edge_face_connectivity = associated(edge_faces)
   add_face_edge_connectivity = associated(face_edges)
   add_face_face_connectivity = associated(face_links)
   add_layers = .false.
   if (present(numLayer) .and. present(layerType) .and. present(layer_zs) .and. present(interface_zs)) then
      add_layers = numLayer >= 1
   end if
   ierr = ug_write_meshtopology(ncid, meshids, meshName, dim, dataLocs, add_edge_face_connectivity, add_face_edge_connectivity, add_face_face_connectivity, add_layers)
   if (ierr /= UG_NOERR) then
      goto 888
   end if
   

   ! Dimensions
   ierr = nf90_def_dim(ncid, 'n'//prefix//'_node',        numNode,   meshids%id_nodedim)
   ierr = nf90_def_dim(ncid, 'n'//prefix//'_edge',        numEdge,   meshids%id_edgedim)
   !ierr = nf90_def_dim(ncid, 'max_n'//prefix//'_face_nodes',        maxNumNodesPerFace,   meshids%id_maxfacenodesdim)
   ierr = nf90_def_dim(ncid, 'n'//prefix//'_Two',                         2,  meshids%id_twodim)
   if (dim == 2 .or. ug_checklocation(dataLocs, UG_LOC_FACE)) then
      ! TODO: AvD: the new maxNumNodesPerFace dummy variable overlaps with this nv here, but they may be different. Remove one.
      maxnv = size(face_nodes, 1)
      ierr = nf90_def_dim(ncid, 'n'//prefix//'_face',         numFace,   meshids%id_facedim)
      ierr = nf90_def_dim(ncid, 'max_n'//prefix//'_face_nodes', maxnv,   meshids%id_maxfacenodesdim)
   end if
 
   if (add_layers) then
      if (dim >= 3) then
         ! Only 1D and 2D mesh topologies can be layered.
         ierr = UG_INVALID_LAYERS
         goto 888
      end if
      ierr = nf90_def_dim(ncid, 'n'//prefix//'_layer',     numLayer,     meshids%id_layerdim)
      ierr = nf90_def_dim(ncid, 'n'//prefix//'_interface', numLayer + 1, meshids%id_interfacedim)
   end if

   ierr = ug_add_coordmapping(ncid, crs)

   ! Nodes
   ! node x,y coordinates.
   ierr = ug_addcoordvars(ncid, meshids%id_nodex, meshids%id_nodey, (/ meshids%id_nodedim /), prefix//'_node_x', prefix//'_node_y', &
                          'x-coordinate of mesh nodes', 'y-coordinate of mesh nodes', trim(meshName), 'node', crs)
#ifdef HAVE_PROJ
   if (.not. crs%is_spherical) then ! If x,y are not in WGS84 system, then add mandatory additional lon/lat coordinates.
      ierr = ug_addlonlatcoordvars(ncid, meshids%id_nodelon, meshids%id_nodelat, (/ meshids%id_nodedim /), prefix//'_node_lon', prefix//'_node_lat', &
                                   'longitude coordinate of mesh nodes', 'latitude coordinate of mesh nodes', trim(meshName), 'node')
   end if
#endif

   ierr = ug_def_var(ncid, meshids, meshids%id_nodez, (/ meshids%id_nodedim /), nf90_double, UG_LOC_NODE, &
                     meshName, 'node_z', 'altitude', 'z-coordinate of mesh nodes', 'm', '', crs, dfill=dmiss)
   ! ierr = nf90_put_att(ncid, meshids%id_nodez, 'positive',       'up') ! Not allowed for non-coordinate variables.

   ! Edges
   if (dim == 1 .or. ug_checklocation(dataLocs, UG_LOC_EDGE)) then
      ierr = nf90_def_var(ncid, prefix//'_edge_nodes', nf90_int, (/ meshids%id_twodim, meshids%id_edgedim /) , meshids%id_edgenodes)
      ierr = nf90_put_att(ncid, meshids%id_edgenodes, 'cf_role',   'edge_node_connectivity')
      ierr = nf90_put_att(ncid, meshids%id_edgenodes, 'mesh', trim(meshName))
      ierr = nf90_put_att(ncid, meshids%id_edgenodes, 'location', 'edge')
      ierr = nf90_put_att(ncid, meshids%id_edgenodes, 'long_name',  'Mapping from every edge to the two nodes that it connects')
      ierr = nf90_put_att(ncid, meshids%id_edgenodes, 'start_index',  1)
      ierr = nf90_put_att(ncid, meshids%id_edgenodes, '_FillValue',  imiss)
   end if
   if (ug_checklocation(dataLocs, UG_LOC_EDGE)) then
      ! edge x,y coordinates.
      ierr = ug_addcoordvars(ncid, meshids%id_edgex, meshids%id_edgey, (/ meshids%id_edgedim /), prefix//'_edge_x', prefix//'_edge_y', &
                             'characteristic x-coordinate of the mesh edge (e.g. midpoint)', 'characteristic y-coordinate of the mesh edge (e.g. midpoint)', trim(meshName), 'edge', crs)
      ierr = nf90_put_att(ncid, meshids%id_edgex, 'bounds',    prefix//'_edge_x_bnd')
      ierr = nf90_put_att(ncid, meshids%id_edgey, 'bounds',    prefix//'_edge_y_bnd')
      ! Add bounds.
      ierr = ug_addcoordvars(ncid, meshids%id_edgexbnd, meshids%id_edgeybnd, (/ meshids%id_twodim, meshids%id_edgedim /), prefix//'_edge_x_bnd', prefix//'_edge_y_bnd', &
                             'x-coordinate bounds of 2D mesh edge (i.e. end point coordinates)', 'y-coordinate bounds of 2D mesh edge (i.e. end point coordinates)', trim(meshName), 'edge', crs)

#ifdef HAVE_PROJ
      if (.not. crs%is_spherical) then ! If x,y are not in WGS84 system, then add mandatory additional lon/lat coordinates.
         ierr = ug_addlonlatcoordvars(ncid, meshids%id_edgelon, meshids%id_edgelat, (/ meshids%id_edgedim /), prefix//'_edge_lon', prefix//'_edge_lat', &
                                      'characteristic longitude coordinate of the mesh edge (e.g. midpoint)', 'characteristic latitude coordinate of the mesh edge (e.g. midpoint)', trim(meshName), 'edge')
         ierr = nf90_put_att(ncid, meshids%id_edgelon, 'bounds',    prefix//'_edge_lon_bnd')
         ierr = nf90_put_att(ncid, meshids%id_edgelat, 'bounds',    prefix//'_edge_lat_bnd')
         ! Add bounds.
         ierr = ug_addlonlatcoordvars(ncid, meshids%id_edgelonbnd, meshids%id_edgelatbnd, (/ meshids%id_twodim, meshids%id_edgedim /), prefix//'_edge_lon_bnd', prefix//'_edge_lat_bnd', &
                                      'longitude coordinate bounds of 2D mesh edge (i.e. end point coordinates)', 'latitude coordinate bounds of 2D mesh edge (i.e. end point coordinates)', trim(meshName), 'edge')
      end if
#endif
   end if

   !ierr = ug_def_var(ncid, meshids, meshName, prefix//'_u1', nf90_double, UG_LOC_EDGE, 'mean', (/ id_nodedim /), id_nodez)
   !ierr = nf90_put_att(ncid, id_nodez, 'units',          'm')
   !ierr = nf90_put_att(ncid, id_nodez, 'standard_name',  'altitude')
   !ierr = nf90_put_att(ncid, id_nodez, 'long_name',      'z-coordinate of mesh nodes')

   !ierr = nf90_def_var(inetfile, 'NetLinkType', nf90_int, id_netlinkdim, id_netlinktype)
   !ierr = nf90_put_att(inetfile, id_netlinktype, 'long_name',    'type of netlink')
   !ierr = nf90_put_att(inetfile, id_netlinktype, 'valid_range',   (/ 0, 4 /))
   !ierr = nf90_put_att(inetfile, id_netlinktype, 'flag_values',   (/ 0, 1, 2, 3, 4 /))
   !ierr = nf90_put_att(inetfile, id_netlinktype, 'flag_meanings', 'closed_link_between_2D_nodes link_between_1D_nodes link_between_2D_nodes embedded_1D2D_link 1D2D_link')

   ! Faces
   if (dim == 2 .or. ug_checklocation(dataLocs, UG_LOC_FACE)) then
      ierr = nf90_def_var(ncid, prefix//'_face_nodes', nf90_int, (/ meshids%id_maxfacenodesdim, meshids%id_facedim /) , meshids%id_facenodes)
      ierr = nf90_put_att(ncid, meshids%id_facenodes, 'cf_role',   'face_node_connectivity')
      ierr = nf90_put_att(ncid, meshids%id_facenodes, 'mesh', trim(meshName))
      ierr = nf90_put_att(ncid, meshids%id_facenodes, 'location', 'face')
      ierr = nf90_put_att(ncid, meshids%id_facenodes, 'long_name',  'Mapping from every face to its corner nodes (counterclockwise)')
      ierr = nf90_put_att(ncid, meshids%id_facenodes, 'start_index',  1)
      ierr = nf90_put_att(ncid, meshids%id_facenodes, '_FillValue',  imiss)

      ! Face edge connectivity.
      if (add_face_edge_connectivity) then
         ierr = nf90_def_var(ncid, prefix//'_face_edges', nf90_int, (/ meshids%id_maxfacenodesdim, meshids%id_facedim /) , meshids%id_faceedges)
         ierr = nf90_put_att(ncid, meshids%id_faceedges, 'cf_role',     'face_edge_connectivity')
         ierr = nf90_put_att(ncid, meshids%id_faceedges, 'long_name',   'Mapping from every face to its edges (in counterclockwise order)')
         ierr = nf90_put_att(ncid, meshids%id_faceedges, 'start_index', 1)
         ierr = nf90_put_att(ncid, meshids%id_faceedges, '_FillValue',  imiss)
      end if

      ! Face face connectivity.
      if (add_face_face_connectivity) then
         ierr = nf90_def_var(ncid, prefix//'_face_links', nf90_int, (/ meshids%id_maxfacenodesdim, meshids%id_facedim /) , meshids%id_facelinks)
         ierr = nf90_put_att(ncid, meshids%id_facelinks, 'cf_role',     'face_face_connectivity')
         ierr = nf90_put_att(ncid, meshids%id_facelinks, 'mesh', trim(meshName))
         ierr = nf90_put_att(ncid, meshids%id_facelinks, 'long_name',   'Mapping from every face to its neighboring faces (in counterclockwise order)')
         ierr = nf90_put_att(ncid, meshids%id_facelinks, 'start_index', 1)
         ierr = nf90_put_att(ncid, meshids%id_facelinks, '_FillValue',  imiss)
      end if

      ! Edge face connectivity.
      ! Note that edge_face_connectivity is not officially part of the UGRID conventions, however it is very similar to e.g. face_edge_connectivity, which is part of the UGRID conventions.
      if (add_edge_face_connectivity) then
         ierr = nf90_def_var(ncid, prefix//'_edge_faces', nf90_int, (/ meshids%id_twodim, meshids%id_edgedim /) , meshids%id_edgefaces)
         ierr = nf90_put_att(ncid, meshids%id_edgefaces, 'cf_role',     'edge_face_connectivity')
         ierr = nf90_put_att(ncid, meshids%id_edgefaces, 'long_name',   'Mapping from every edge to the two faces that it separates')
         ierr = nf90_put_att(ncid, meshids%id_edgefaces, 'start_index', 1)
         ierr = nf90_put_att(ncid, meshids%id_edgefaces, '_FillValue',  imiss)
      end if
   end if
   if (ug_checklocation(dataLocs, UG_LOC_FACE)) then
      ! face x,y coordinates.
      ierr = ug_addcoordvars(ncid, meshids%id_facex, meshids%id_facey, (/ meshids%id_facedim /), prefix//'_face_x', prefix//'_face_y', &
                             'Characteristic x-coordinate of mesh face', 'Characteristic y-coordinate of mesh face', trim(meshName), 'face', crs)
      ierr = nf90_put_att(ncid, meshids%id_facex, 'bounds',    prefix//'_face_x_bnd')
      ierr = nf90_put_att(ncid, meshids%id_facey, 'bounds',    prefix//'_face_y_bnd')
      ! Add bounds.
      ierr = ug_addcoordvars(ncid, meshids%id_facexbnd, meshids%id_faceybnd, (/ meshids%id_maxfacenodesdim, meshids%id_facedim /), prefix//'_face_x_bnd', prefix//'_face_y_bnd', &
                             'x-coordinate bounds of 2D mesh face (i.e. corner coordinates)', 'y-coordinate bounds of 2D mesh face (i.e. corner coordinates)', trim(meshName), 'face', crs)
      ierr = nf90_put_att(ncid, meshids%id_facexbnd, '_FillValue',  dmiss)
      ierr = nf90_put_att(ncid, meshids%id_faceybnd, '_FillValue',  dmiss)

#ifdef HAVE_PROJ
      if (.not. crs%is_spherical) then ! If x,y are not in WGS84 system, then add mandatory additional lon/lat coordinates.
         ierr = ug_addlonlatcoordvars(ncid, meshids%id_facelon, meshids%id_facelat, (/ meshids%id_facedim /), prefix//'_face_lon', prefix//'_face_lat', &
                                      'Characteristic longitude coordinate of mesh face', 'Characteristic latitude coordinate of mesh face', trim(meshName), 'face')
         ierr = nf90_put_att(ncid, meshids%id_facelon, 'bounds',    prefix//'_face_lon_bnd')
         ierr = nf90_put_att(ncid, meshids%id_facelat, 'bounds',    prefix//'_face_lat_bnd')
         ! Add bounds.
         ierr = ug_addlonlatcoordvars(ncid, meshids%id_facelonbnd, meshids%id_facelatbnd, (/ meshids%id_maxfacenodesdim, meshids%id_facedim /), prefix//'_face_lon_bnd', prefix//'_face_lat_bnd', &
                                      'longitude coordinate bounds of 2D mesh face (i.e. corner coordinates)', 'latitude coordinate bounds of 2D mesh face (i.e. corner coordinates)', trim(meshName), 'face')
         ierr = nf90_put_att(ncid, meshids%id_facelonbnd, '_FillValue',  dmiss)
         ierr = nf90_put_att(ncid, meshids%id_facelatbnd, '_FillValue',  dmiss)
      end if
#endif
   end if

   ! Layers
   if (add_layers) then
      ! Write mesh layer distribution (mesh-global, not per face)
      select case(layerType)
      case (LAYERTYPE_OCEANSIGMA)
         ierr = nf90_def_var(ncid, prefix//'_layer_sigma',     nf90_double, meshids%id_layerdim,     meshids%id_layer_zs)
         ierr = nf90_def_var(ncid, prefix//'_interface_sigma', nf90_double, meshids%id_interfacedim, meshids%id_interface_zs)
         ierr = nf90_put_att(ncid, meshids%id_layer_zs,     'standard_name', 'ocean_sigma_coordinate')
         ierr = nf90_put_att(ncid, meshids%id_interface_zs, 'standard_name', 'ocean_sigma_coordinate')
         ierr = nf90_put_att(ncid, meshids%id_layer_zs,     'long_name',     'Sigma coordinate of layer centres')
         ierr = nf90_put_att(ncid, meshids%id_interface_zs, 'long_name',     'Sigma coordinate of layer interfaces')
         ! See http://cfconventions.org/cf-conventions/cf-conventions.html#dimensionless-vertical-coordinate
         ! and http://cfconventions.org/cf-conventions/cf-conventions.html#_ocean_sigma_coordinate for info about formula_terms attribute for sigma coordinates.
         ! TODO this code assumes that the data variables with values for eta and depth are always called s1 and waterdepth. AK
         ierr = nf90_put_att(ncid, meshids%id_layer_zs,     'formula_terms', 'sigma: '//prefix//'_layer_sigma eta: '//prefix//'_s1 depth: '//prefix//'_waterdepth') ! TODO: AvD: do we define this only on faces?
         ierr = nf90_put_att(ncid, meshids%id_interface_zs, 'formula_terms', 'sigma: '//prefix//'_interface_sigma eta: '//prefix//'_s1 depth: '//prefix//'_waterdepth') ! TODO: AvD: do we define this only on faces?
      case (LAYERTYPE_Z)
         ierr = nf90_def_var(ncid, prefix//'_layer_z',     nf90_double, meshids%id_layerdim,     meshids%id_layer_zs)
         ierr = nf90_def_var(ncid, prefix//'_interface_z', nf90_double, meshids%id_interfacedim, meshids%id_interface_zs)
         ierr = nf90_put_att(ncid, meshids%id_layer_zs,     'standard_name', 'altitude')
         ierr = nf90_put_att(ncid, meshids%id_interface_zs, 'standard_name', 'altitude')
         ierr = nf90_put_att(ncid, meshids%id_layer_zs,     'long_name',     'Vertical coordinate of layer centres')
         ierr = nf90_put_att(ncid, meshids%id_interface_zs, 'long_name',     'Vertical coordinate of layer interfaces')
         ierr = nf90_put_att(ncid, meshids%id_layer_zs,     'unit',          'm')
         ierr = nf90_put_att(ncid, meshids%id_interface_zs, 'unit',          'm')
      case default
         ierr = UG_INVALID_LAYERS
         goto 888
      end select
   end if

! TODO: AvD: add the following (resolution may be difficult)
!> 		:geospatial_lat_min = 52.9590188916822 ;
!> 		:geospatial_lat_max = 53.8746171549558 ;
!> 		:geospatial_lat_units = "degrees_north" ;
!> 		:geospatial_lat_resolution = "on average     370.50 meters" ;
!> 		:geospatial_lon_min = 6.37848435307356 ;
!> 		:geospatial_lon_max = 7.68944972163126 ;
!> 		:geospatial_lon_units = "degrees_east" ;
!> 		:geospatial_lon_resolution = "on average     370.50 meters" ;

   ierr = nf90_enddef(ncid)

! -- end of header --
      
   ! Write the actual data
   ! Nodes:
   ierr = nf90_put_var(ncid, meshids%id_nodex,    xn(1:numNode))
   ierr = nf90_put_var(ncid, meshids%id_nodey,    yn(1:numNode))
!   ierr = nf90_put_var(ncid, meshids%id_nodez,    zn(1:numNode))
#ifdef HAVE_PROJ
   if (.not. crs%is_spherical) then ! If x,y are not in WGS84 system, then add mandatory additional lon/lat coordinates.
      call realloc(lonn, size(xn), fill=dmiss, keepExisting=.false.)
      call realloc(latn, size(yn), fill=dmiss, keepExisting=.false.)
      call transform_coordinates(RIJKSDRIEHOEK_PROJ_STRING, WGS84_PROJ_STRING, xn, yn, lonn, latn)
      ierr = nf90_put_var(ncid, meshids%id_nodelon, lonn(1:numNode))
      ierr = nf90_put_var(ncid, meshids%id_nodelat, latn(1:numNode))
   end if
#endif

   ! Edges:
   if (dim == 1 .or. ug_checklocation(dataLocs, UG_LOC_EDGE)) then
      ierr = nf90_put_var(ncid, meshids%id_edgenodes, edge_nodes, count=(/ 2, numEdge /))
   end if

   if (ug_checklocation(dataLocs, UG_LOC_EDGE)) then
      ierr = nf90_put_var(ncid, meshids%id_edgex,    xe(1:numEdge))
      ierr = nf90_put_var(ncid, meshids%id_edgey,    ye(1:numEdge))

      ! end point coordinates:
      allocate(edgexbnd(2, numEdge), edgeybnd(2, numEdge))
      edgexbnd = dmiss
      edgeybnd = dmiss
      do n=1,numEdge
         edgexbnd(1:2, n) = xn(edge_nodes(1:2, n))
         edgeybnd(1:2, n) = yn(edge_nodes(1:2, n))
      end do
      ierr = nf90_put_var(ncid, meshids%id_edgexbnd, edgexbnd)
      ierr = nf90_put_var(ncid, meshids%id_edgeybnd, edgeybnd)
      deallocate(edgexbnd, edgeybnd)

#ifdef HAVE_PROJ
      if (.not. crs%is_spherical) then ! If x,y are not in WGS84 system, then add mandatory additional lon/lat coordinates.
         call realloc(lone, size(xe), fill=dmiss, keepExisting=.false.)
         call realloc(late, size(ye), fill=dmiss, keepExisting=.false.)
         call transform_coordinates(RIJKSDRIEHOEK_PROJ_STRING, WGS84_PROJ_STRING, xe, ye, lone, late)
         ierr = nf90_put_var(ncid, meshids%id_edgelon, lone(1:numEdge))
         ierr = nf90_put_var(ncid, meshids%id_edgelat, late(1:numEdge))
         deallocate(lone)
         deallocate(late)

         ! end point coordinates:
         allocate(edgelonbnd(2, numEdge), edgelatbnd(2, numEdge))
         edgelonbnd = dmiss
         edgelatbnd = dmiss
         do n=1,numEdge
            edgelonbnd(1:2, n) = lonn(edge_nodes(1:2, n))
            edgelatbnd(1:2, n) = latn(edge_nodes(1:2, n))
         end do
         ierr = nf90_put_var(ncid, meshids%id_edgelonbnd, edgelonbnd)
         ierr = nf90_put_var(ncid, meshids%id_edgelatbnd, edgelatbnd)
         deallocate(edgelonbnd, edgelatbnd)
      end if
#endif
   end if

   ! Faces:
   if (dim == 2 .or. ug_checklocation(dataLocs, UG_LOC_FACE)) then
      ! Write mesh faces (2D cells)
      ierr = nf90_put_var(ncid, meshids%id_facenodes, face_nodes)

      ! Face edge connectivity:
      if (add_face_edge_connectivity) then
         ierr = nf90_put_var(ncid, meshids%id_faceedges, face_edges, count=(/ maxnv, numFace /))
      end if
      ! Face face connectivity:
      if (add_face_face_connectivity) then
         ierr = nf90_put_var(ncid, meshids%id_facelinks, face_links, count=(/ maxnv, numFace /))
      end if
      ! Edge face connectivity:
      if (add_edge_face_connectivity) then
         ierr = nf90_put_var(ncid, meshids%id_edgefaces, edge_faces, count=(/ 2, numEdge /))
      end if

      ! corner point coordinates:
      allocate(facexbnd(maxnv, numFace), faceybnd(maxnv, numFace))
      facexbnd = dmiss
      faceybnd = dmiss

      do n=1,numFace
         do k=1,maxnv
            if (face_nodes(k, n) == imiss) then
               exit ! This face has less corners than maxnv, we're done for this one.
            end if

            facexbnd(k, n) = xn(face_nodes(k, n))
            faceybnd(k, n) = yn(face_nodes(k, n))
         end do
      end do
      ierr = nf90_put_var(ncid, meshids%id_facexbnd, facexbnd)
      ierr = nf90_put_var(ncid, meshids%id_faceybnd, faceybnd)
      deallocate(facexbnd, faceybnd)

#ifdef HAVE_PROJ
      if (.not. crs%is_spherical) then ! If x,y are not in WGS84 system, then add mandatory additional lon/lat coordinates.
         ! corner point coordinates:
         allocate(facelonbnd(maxnv, numFace), facelatbnd(maxnv, numFace))
         facelonbnd = dmiss
         facelatbnd = dmiss

         do n=1,numFace
            do k=1,maxnv
               if (face_nodes(k, n) == imiss) then
                  exit ! This face has less corners than maxnv, we're done for this one.
               end if

               facelonbnd(k, n) = lonn(face_nodes(k, n))
               facelatbnd(k, n) = latn(face_nodes(k, n))
            end do
         end do
         ierr = nf90_put_var(ncid, meshids%id_facelonbnd, facelonbnd)
         ierr = nf90_put_var(ncid, meshids%id_facelatbnd, facelatbnd)
         deallocate(facelonbnd, facelatbnd)
      end if
#endif
   end if
   
   if (ug_checklocation(dataLocs, UG_LOC_FACE)) then
      ierr = nf90_put_var(ncid, meshids%id_facex,    xf(1:numFace))
      ierr = nf90_put_var(ncid, meshids%id_facey,    yf(1:numFace))

#ifdef HAVE_PROJ
      if (.not. crs%is_spherical) then ! If x,y are not in WGS84 system, then add mandatory additional lon/lat coordinates.
         call realloc(lonf, size(xf), fill=dmiss, keepExisting=.false.)
         call realloc(latf, size(yf), fill=dmiss, keepExisting=.false.)
         call transform_coordinates(RIJKSDRIEHOEK_PROJ_STRING, WGS84_PROJ_STRING, xf, yf, lonf, latf)
         ierr = nf90_put_var(ncid, meshids%id_facelon, lonf(1:numFace))
         ierr = nf90_put_var(ncid, meshids%id_facelat, latf(1:numFace))
         deallocate(lonf)
         deallocate(latf)
      end if
#endif
   end if

   if (allocated(lonn)) deallocate(lonn)
   if (allocated(latn)) deallocate(latn)

   ! Layers
   if (add_layers) then
      ! Write mesh layer distribution (mesh-global, not per face)
      ierr = nf90_put_var(ncid, meshids%id_layer_zs,     layer_zs(1:numLayer))
      ierr = nf90_put_var(ncid, meshids%id_interface_zs, interface_zs(1:numLayer + 1))
   end if

   ! Check for any remaining native NetCDF errors
   if (ierr /= nf90_noerr) then
      goto 888
   end if

   ! Leave the dataset in the same mode as we got it.
   if (wasInDefine == 1) then
      ierr = nf90_redef(ncid)
   end if

   ierr = UG_NOERR
   return ! Return with success

888 continue


end function ug_write_mesh_arrays


!
! -- Reading-related routines ---------------------------------------------
!

!> Initialized all UGRID-specific meta information present in an open data set.
function ug_init_dataset(ncid, ug_file) result(ierr)
   integer,         intent(in   ) :: ncid    !< ID of already opened data set.
   type(t_ug_file), intent(inout) :: ug_file !< UGRID file struct with cached meta information.
   integer                        :: ierr    !< Result status (UG_NOERR if successful).
   
   integer :: iv, im, nmesh, numvar
   logical :: is_mesh_topo
   
   ! Count nr of meshes present in the file
   ierr = ug_get_mesh_count(ncid, nmesh)
   if (ierr /= UG_NOERR) then
      goto 999
   end if
   ug_file%nummesh = nmesh
   allocate(ug_file%meshids(nmesh))
   allocate(ug_file%meshnames(nmesh))
   

   ! Now check all variables and if they're a mesh topology, read in all details.
   ierr = nf90_inquire(ncid, nVariables = numVar)

   im = 0
   do iv=1,numVar
      is_mesh_topo = ug_is_mesh_topology(ncid, iv)
      if (.NOT. is_mesh_topo) then
         cycle
      end if
      im = im + 1
      ierr = nf90_inquire_variable(ncid, iv, name = ug_file%meshnames(im))
      ierr = ug_init_mesh_topology(ncid, iv, ug_file%meshids(im))
      if (ierr /= UG_NOERR) then
         goto 999
      end if
   end do

   ierr = UG_NOERR
   return ! Return with success

999 continue
    ! Some error (status was set earlier)

end function ug_init_dataset


!> Reads the mesh_topology attributes from a NetCDF variable.
function ug_init_mesh_topology(ncid, varid, meshids) result(ierr)
   integer,         intent(in   ) :: ncid    !< ID of already opened data set.
   integer,         intent(in   ) :: varid   !< NetCDF variable ID that contains the mesh topology information.
   type(t_ug_meshids), intent(  out) :: meshids !< Structure in which all mesh topology variable ids will be stored.
   integer                        :: ierr    !< Result status (UG_NOERR if successful).

   character(len=255) :: varname
   integer :: id
   integer ::dimids(2)

   meshids%id_meshtopo        = varid              !< Top-level variable ID for mesh topology, collects all related variable names via attributes.

   !
   ! Dimensions:
   !
   call att_to_dimid('node_dimension', meshids%id_nodedim)
   call att_to_dimid('edge_dimension', meshids%id_edgedim)
   call att_to_dimid('face_dimension', meshids%id_facedim)

   call att_to_dimid('max_face_nodes_dimension', meshids%id_maxfacenodesdim)
   if (ierr /= nf90_noerr) then
      ! The non-UGRID max_face_nodes_dimension was not found. Detect it ourselves.
      varname = ''
      ierr = nf90_get_att(ncid, meshids%id_meshtopo, 'face_node_connectivity', varname)
      id = 0
      ierr = nf90_inq_varid(ncid, trim(varname), id)
      ! Get the dimension ids from the face-nodes variable, and select the correct one from that.
      ierr = nf90_inquire_variable(ncid, id, dimids=dimids)
      if (ierr == nf90_noerr) then
         if (dimids(1) == meshids%id_facedim) then
            meshids%id_maxfacenodesdim = dimids(2) ! the other
         else
            meshids%id_maxfacenodesdim = dimids(1)
         end if
      end if
   end if

   
   !
   ! Coordinate variables
   !
   call att_to_coordvarids('node_coordinates', meshids%id_nodex, meshids%id_nodey)!, meshids%id_nodez
   call att_to_coordvarids('edge_coordinates', meshids%id_edgex, meshids%id_edgey)
   !call att_to_varid('', id_edgexbnd        = -1 !<            variable ID for edge boundaries' x-coordinate.
   !call att_to_varid('', id_edgeybnd        = -1 !<            variable ID for edge boundaries' y-coordinate.
   call att_to_coordvarids('face_coordinates', meshids%id_facex, meshids%id_facey)
   !call att_to_varid('', id_facexbnd        = -1 !<            variable ID for face boundaries' x-coordinate.
   !call att_to_varid('', id_faceybnd        = -1 !<            variable ID for face boundaries' y-coordinate.

   !
   ! Topology variables
   !
   call att_to_varid('edge_node_connectivity', meshids%id_edgenodes) !< Variable ID for edge-to-node mapping table.
   call att_to_varid('face_node_connectivity', meshids%id_facenodes) !< Variable ID for face-to-node mapping table.
   call att_to_varid('edge_face_connectivity', meshids%id_edgefaces) !< Variable ID for edge-to-face mapping table (optional, can be -1).   
   call att_to_varid('face_edge_connectivity', meshids%id_faceedges) !< Variable ID for face-to-edge mapping table (optional, can be -1).
   call att_to_varid('face_face_connectivity', meshids%id_facelinks) !< Variable ID for face-to-face mapping table (optional, can be -1).


contains

subroutine att_to_dimid(name, id)
   character(len=*), intent(in   ) :: name
   integer,          intent(  out) :: id
   
   varname = ''
   ierr = nf90_get_att(ncid, varid, name, varname)
   if (ierr /= nf90_noerr) then
      goto 999
   end if
      ierr = nf90_inq_dimid(ncid, trim(varname), id)
999 continue
    ! Some error     
end subroutine att_to_dimid

subroutine att_to_varid(name, id)
   character(len=*), intent(in   ) :: name
   integer,          intent(  out) :: id
   
   varname = ''
   ierr = nf90_get_att(ncid, varid, name, varname)
   if (ierr /= nf90_noerr) then
      goto 999
   end if
      ierr = nf90_inq_varid(ncid, trim(varname), id)
   return
   
999 continue   
   ierr = 0 ! because could be optional
   id = -1   
   ! Some error
end subroutine att_to_varid

subroutine att_to_coordvarids(name, idx, idy, idz)
   character(len=*),  intent(in   ) :: name
   integer,           intent(  out) :: idx, idy
   integer, optional, intent(  out) :: idz

   integer :: i1, i2, n
   varname = ''

   ierr = nf90_get_att(ncid, varid, name, varname)
   if (ierr /= nf90_noerr) then
      goto 999
   end if
   
   i1 = 1
   n = len_trim(varname)   
   
   ! TODO: AvD: I'd rather use a string tokenizer here.
   i2 = index(varname(i1:n), ' ')
   if (i2 == 0) then
      i2 = n
   else
      i2 = i1 + i2 - 1
   end if
   ierr = nf90_inq_varid(ncid, varname(i1:i2-1), idx)
   i1 = i2+1

   i2 = index(varname(i1:n), ' ')
   if (i2 == 0) then
      i2 = n + 1
   else
      i2 = i1 + i2 - 1
   end if
   ierr = nf90_inq_varid(ncid, varname(i1:i2-1), idy)
   i1 = i2+1

   if (present(idz)) then
      i2 = index(varname(i1:n), ' ')
      if (i2 == 0) then
         i2 = n + 1
      else
         i2 = i1 + i2 - 1
      end if
      ierr = nf90_inq_varid(ncid, varname(i1:i2-1), idz)
      i1 = i2+1
   end if

   return

999 continue
    ! Some error  
end subroutine att_to_coordvarids

end function ug_init_mesh_topology


!> Returns whether a given variable is a mesh topology variable.
function ug_is_mesh_topology(ncid, varid) result(is_mesh_topo)
   integer,        intent(in)  :: ncid         !< NetCDF dataset id
   integer,        intent(in)  :: varid        !< NetCDF variable id
   logical                     :: is_mesh_topo !< Return value

   integer :: ierr
   character(len=13) :: buffer

   is_mesh_topo = .false.

   buffer = ' '
   ierr = nf90_get_att(ncid, varid, 'cf_role', buffer)
   if (ierr == nf90_noerr) then
      if (trim(buffer) == 'mesh_topology') then
         is_mesh_topo = .true.
      end if
   end if
end function ug_is_mesh_topology

!> Gets the number of mesh topologies in an open dataset.
!! use this to determine on how many different meshes, data is defined in the dataset.
!!
!! \see 
function ug_get_mesh_count(ncid, numMesh) result(ierr)
   integer,        intent(in)  :: ncid     !< NetCDF dataset id
   integer,        intent(out) :: numMesh  !< Number of mesh topologies in the dataset (>= 0).
   integer                     :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.

   integer :: numVar, i
   logical :: is_mesh_topo

   ierr = nf90_inquire(ncid, nVariables = numVar)
  
   numMesh = 0
   do i=1,numVar
      is_mesh_topo = ug_is_mesh_topology(ncid, i)
      if (is_mesh_topo) then
         numMesh = numMesh + 1
      end if
   end do

end function ug_get_mesh_count

!> Gets the name of the mesh topology variable in an open dataset.
!!
!! \see 
function ug_get_mesh_name(ncid, meshids, meshname) result(ierr)
   integer,             intent(in)    :: ncid     !< NetCDF dataset id, should be already open.
   type(t_ug_meshids),  intent(in)    :: meshids  !< Set of NetCDF-ids for all mesh geometry arrays.
   character(len=*),    intent(  out) :: meshname !< The name of the mesh topology variable.
   integer                            :: ierr     !< Result status, ug_noerr if successful.
   
   meshname = ''
   ierr = nf90_inquire_variable(ncid, meshids%id_meshtopo, name=meshname)
   if (ierr /= nf90_noerr) then
      write (ug_messagestr, '(a,i0)') 'ug_get_mesh_name: could not find meshname for topology var id ', meshids%id_meshtopo
      ierr = UG_INVALID_MESHNAME
      Call SetMessage(Level_Fatal, ug_messagestr)
   end if
end function ug_get_mesh_name

!> Gets the size/count of items for the specified topological location.
!! Use this to get the number of nodes/edges/faces/volumes.
function ug_inquire_dimension(ncid, meshids, idimtype, len) result(ierr)
   integer,            intent(in)    :: ncid     !< NetCDF dataset id, should be already open.
   type(t_ug_meshids), intent(in)    :: meshids  !< Set of NetCDF-ids for all mesh geometry arrays.
   integer,            intent(in)    :: idimtype !< The location type to count (one of UG_LOC_NODE, UG_LOC_EDGE, UG_LOC_FACE, UG_LOC_VOL).
   integer,            intent(  out) :: len      !< The number of items for that location.
   integer                           :: ierr     !< Result status (UG_NOERR==NF90_NOERR if successful).

   integer :: idim

   select case (idimtype)
   case (UG_LOC_NODE)
      idim = meshids%id_nodedim
   case (UG_LOC_EDGE)
      idim = meshids%id_edgedim
   case (UG_LOC_FACE)
      idim = meshids%id_facedim
   case (UG_DIM_MAXFACENODES)
      idim = meshids%id_maxfacenodesdim 
   case default
      ierr = UG_NOTIMPLEMENTED
      goto 999
   end select
   
   ierr = nf90_inquire_dimension(ncid, idim, len=len)

   ! Success
   return

999 continue
    ! Some error  
end function ug_inquire_dimension


!> Gets the dimension of the mesh topology for the specified mesh in a UGRID data set.
function ug_get_topology_dimension(ncid, meshids, dim) result(ierr)
   integer,            intent(in)    :: ncid     !< NetCDF dataset id, should be already open.
   type(t_ug_meshids), intent(in)    :: meshids  !< Set of NetCDF-ids for all mesh geometry arrays.
   integer,            intent(  out) :: dim      !< The topology dimension for the requested mesh.
   integer                           :: ierr     !< Result status (UG_NOERR==NF90_NOERR if successful).


   ierr = UG_NOERR
   ierr = nf90_get_att(ncid, meshids%id_meshtopo, 'topology_dimension', dim)

   ! Success
   return

999 continue
    ! Some error  
end function ug_get_topology_dimension


!> Reads the actual mesh geometry from the specified mesh in a UGRID dataset.
!! By default only reads in the dimensions (face/edge/node counts).
!! Optionally, also all coordinate arrays + connectivity tables can be read.
function ug_get_meshgeom(ncid, meshids, meshgeom, includeArrays) result(ierr)
   use m_alloc

   integer,             intent(in   ) :: ncid          !< ID of already opened data set.
   type(t_ug_meshids),  intent(in   ) :: meshids       !< Structure with all mesh topology variable ids (should be initialized already).
   type(t_ug_meshgeom), intent(inout) :: meshgeom      !< Structure in which all mesh geometry will be stored.
   logical, optional,   intent(in   ) :: includeArrays !< (optional) Whether or not to include coordinate arrays and connectivity tables. Default: .false., i.e., dimension counts only.
   integer                            :: ierr      !< Result status (UG_NOERR if successful).

   logical :: includeArrays_
   character(len=255) :: varname
   integer :: id
   integer ::dimids(2)

   ierr = UG_NOERR

   if (present(includeArrays)) then
      includeArrays_ = includeArrays
   else
      includeArrays_ = .false.
   end if

   !
   ! Topology dimension:
   !
   ierr = nf90_inquire_variable(ncid, meshids%id_meshtopo, name = meshgeom%meshname)
   ierr = ug_get_topology_dimension(ncid, meshids, meshgeom%dim)

   !
   ! Dimensions/location counts:
   !
   ierr = ug_inquire_dimension(ncid, meshids, UG_LOC_NODE, meshgeom%numnode)
   ierr = ug_inquire_dimension(ncid, meshids, UG_LOC_EDGE, meshgeom%numedge)
   if (meshgeom%dim >= 2) then
      ierr = ug_inquire_dimension(ncid, meshids, UG_LOC_FACE, meshgeom%numface)
      ierr = ug_inquire_dimension(ncid, meshids, UG_DIM_MAXFACENODES, meshgeom%maxnumfacenodes)
   end if
   ! TODO: AvD: extend to 3D

   if (includeArrays_) then
      ! TODO: AvD: inquire proper fillvalue as dmiss from file.

      !
      ! Nodes
      !
      call reallocP(meshgeom%nodex, meshgeom%numnode, keepExisting = .false., fill = -999d0)
      call reallocP(meshgeom%nodey, meshgeom%numnode, keepExisting = .false., fill = -999d0)
      call reallocP(meshgeom%nodez, meshgeom%numnode, keepExisting = .false., fill = -999d0)
      
      ierr = ug_get_node_coordinates(ncid, meshids, meshgeom%nodex, meshgeom%nodey)
      ! TODO: AvD: include zk coordinates

      !
      ! Edges
      !
      ! TODO: AvD: introduce ug_read_mesh_arrays( .. intent out arrays ..)
   end if
end function ug_get_meshgeom

   
!> Gets the x,y-coordinates for all nodes in the specified mesh.
!! The output x,y arrays are supposed to be of exact correct length already.
function ug_get_node_coordinates(ncid, meshids, xn, yn) result(ierr)
   integer,            intent(in)  :: ncid    !< NetCDF dataset id, should be already open.
   type(t_ug_meshids), intent(in)  :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
   real(kind=dp),      intent(out) :: xn(:), yn(:) !< Arrays to store x,y-coordinates of the mesh nodes.
   integer                         :: ierr     !< Result status (UG_NOERR==NF90_NOERR if successful).

   ierr = nf90_get_var(ncid, meshids%id_nodex, xn)
   if(ierr /= UG_NOERR) then 
       Call SetMessage(Level_Fatal, 'could not read x coordinates')
   end if 
   ierr = nf90_get_var(ncid, meshids%id_nodey, yn)
   ! TODO: AvD: some more careful error handling

end function ug_get_node_coordinates


!> Puts the x,y-coordinates for all nodes in the specified mesh.
!! The input x,y arrays are supposed to be of exact correct length already.
function ug_put_node_coordinates(ncid, meshids, xn, yn) result(ierr)
   integer,            intent(in)  :: ncid    !< NetCDF dataset id, should be already open.
   type(t_ug_meshids), intent(in)  :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
   real(kind=dp),      intent(in)  :: xn(:), yn(:) !< Arrays to store x,y-coordinates of the mesh nodes.
   integer                         :: ierr     !< Result status (UG_NOERR==NF90_NOERR if successful).

   ierr = nf90_put_var(ncid, meshids%id_nodex, xn)
   if(ierr /= NF90_NOERR) then 
       Call SetMessage(Level_Fatal, 'could not put x coordinates')
   end if 
   ierr = nf90_put_var(ncid, meshids%id_nodey, yn)
   ! TODO: AvD: some more careful error handling

end function ug_put_node_coordinates


!> Gets the edge-node connectivity table for all edges in the specified mesh.
!! The output edge_nodes array is supposed to be of exact correct size already.
function ug_get_edge_nodes(ncid, meshids, edge_nodes) result(ierr)
   integer,            intent(in)  :: ncid    !< NetCDF dataset id, should be already open.
   type(t_ug_meshids), intent(in)  :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
   integer,            intent(out) :: edge_nodes(:,:) !< Array to the edge-node connectivity table.
   integer                         :: ierr     !< Result status (UG_NOERR==NF90_NOERRif successful).

   ierr = nf90_get_var(ncid, meshids%id_edgenodes, edge_nodes)
   ! TODO: AvD: some more careful error handling
   
   ! TODO: AvD: also introduce 0-/1-based indexing handling.

end function ug_get_edge_nodes

!> Gets the face-node connectivity table for all faces in the specified mesh.
!! The output face_nodes array is supposed to be of exact correct size already.
function ug_get_face_nodes(ncid, meshids, face_nodes) result(ierr)
   integer,            intent(in)  :: ncid    !< NetCDF dataset id, should be already open.
   type(t_ug_meshids), intent(in)  :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
   integer,            intent(out) :: face_nodes(:,:) !< Array to the face-node connectivity table.
   integer                         :: ierr     !< Result status (UG_NOERR==NF90_NOERRif successful).

   ierr = nf90_get_var(ncid, meshids%id_facenodes, face_nodes)
   ! TODO: AvD: some more careful error handling
   
   ! TODO: AvD: also introduce 0-/1-based indexing handling.

end function ug_get_face_nodes


!> Returns the number of variables that are available in the specified dataset on the specified mesh.
!! The location type allows to select on specific topological mesh locations
!! (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
function ug_get_var_count(ncid, meshids, iloctype, nvar) result(ierr)
   use string_module
   integer,             intent(in)    :: ncid     !< NetCDF dataset id, should be already open.
   type(t_ug_meshids),  intent(in)    :: meshids  !< Set of NetCDF-ids for all mesh geometry arrays.
   integer,             intent(in)    :: iloctype !< The topological location on which to select data (one of UG_LOC_FACE/EDGE/NODE/ALL2D).
   integer,             intent(  out) :: nvar     !< Number of variables defined on the requested location type+mesh+dataset.
   integer                            :: ierr     !< Result status, ug_noerr if successful.

   integer :: numVar, iv, ivarloc
   character(len=255) :: str, meshname
   str = ''
   meshname = ''
   ierr = nf90_inquire_variable(ncid, meshids%id_meshtopo, name=meshname)
   if (ierr /= nf90_noerr) then
      ierr = UG_INVALID_MESHNAME
      goto 999
   end if

   ! Now check all variables and if they're data variables on the right mesh+location.
   ierr = nf90_inquire(ncid, nVariables = numVar)

   nvar = 0
   do iv=1,numVar
      ! Step 1 of 2: check mesh name
      str = ''
      ierr = nf90_get_att(ncid, iv, 'mesh', str)
      if (ierr /= nf90_noerr) then
         ! No UGRID :mesh attribute, ignore this var.
         cycle
      end if
      
      if (.not.strcmpi(str,meshname)) then
         ! Mesh names do not match
         cycle
      end if

      ! Step 2 of 2: check location name
      str = ''
      ierr = nf90_get_att(ncid, iv, 'location', str)
      if (ierr /= nf90_noerr) then
         ! No UGRID :location attribute, ignore this var.
         cycle
      end if
      call ug_location_to_loctype(str, ivarloc)
      if (ug_checklocation(iloctype, ivarloc)) then
         ! This variable is ok. Mesh matched, and now the location also matched.
         nvar = nvar + 1
      end if
   end do

   ierr = UG_NOERR
   return ! Return with success

999 continue
    ! Some error (status was set earlier)

end function ug_get_var_count


!> Gets a list of variable IDs that are available in the specified dataset on the specified mesh.
!! The location type allows to select on specific topological mesh locations
!! (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D)
function ug_inq_varids(ncid, meshids, iloctype, varids) result(ierr)
   use string_module
   integer,             intent(in)    :: ncid     !< NetCDF dataset id, should be already open.
   type(t_ug_meshids),  intent(in)    :: meshids  !< Set of NetCDF-ids for all mesh geometry arrays.
   integer,             intent(in)    :: iloctype !< The topological location on which to select data variables (one of UG_LOC_FACE/EDGE/NODE/ALL2D).
   integer,             intent(  out) :: varids(:) !< Array to store the variable ids in.
   integer                            :: ierr     !< Result status, ug_noerr if successful.

   integer :: numVar, iv, ivarloc, nvar, maxvar
   character(len=255) :: str, meshname
   str = ''
   meshname = ''

   ierr = nf90_inquire_variable(ncid, meshids%id_meshtopo, name=meshname)
   if (ierr /= nf90_noerr) then
      ierr = UG_INVALID_MESHNAME
      goto 999
   end if

   ! Now check all variables and if they're data variables on the right mesh+location.
   ierr = nf90_inquire(ncid, nVariables = numVar)

   maxvar = size(varids)
   nvar = 0
   do iv=1,numVar
      ! Step 1 of 2: check mesh name
      str = ''
      ierr = nf90_get_att(ncid, iv, 'mesh', str)
      if (ierr /= nf90_noerr) then
         ! No UGRID :mesh attribute, ignore this var.
         cycle
      end if
      
      if (.not.strcmpi(str,meshname)) then
         ! Mesh names do not match
         cycle
      end if

      ! Step 2 of 2: check location name
      str = ''
      ierr = nf90_get_att(ncid, iv, 'location', str)
      if (ierr /= nf90_noerr) then
         ! No UGRID :location attribute, ignore this var.
         cycle
      end if
      call ug_location_to_loctype(str, ivarloc)
      if (ug_checklocation(iloctype, ivarloc)) then
         ! This variable is ok. Mesh matched, and now the location also matched.
         if (nvar >= maxvar) then
            ierr = UG_ARRAY_TOOSMALL
            goto 999
         end if

         nvar = nvar + 1
         varids(nvar) = iv
      end if
   end do

   ierr = UG_NOERR
   return ! Return with success

999 continue
    ! Some error (status was set earlier)

end function ug_inq_varids


!> Gets the variable ID for a data variable that is defined in the specified dataset on the specified mesh.
!! The variable is searched based on variable name (without any "meshnd_" prefix), and which :mesh it is defined on.
function ug_inq_varid(ncid, meshids, varname, varid) result(ierr)
   use string_module
   integer,             intent(in)    :: ncid     !< NetCDF dataset id, should be already open.
   type(t_ug_meshids),  intent(in)    :: meshids  !< Set of NetCDF-ids for all mesh geometry arrays.
   character(len=*),    intent(in)    :: varname  !< The name of the variable to be found. Should be without any "meshnd_" prefix.
   integer,             intent(  out) :: varid    !< The resulting variable id, if found.
   integer                            :: ierr     !< Result status, ug_noerr if successful.

   integer :: numVar, iv, ivarloc, nvar, maxvar
   character(len=255) :: str, meshname
   str = ''
   meshname = ''

   ierr = nf90_inquire_variable(ncid, meshids%id_meshtopo, name=meshname)
   if (ierr /= nf90_noerr) then
      ierr = UG_INVALID_MESHNAME
      goto 999
   end if

   ! Now check variable with user-specified name and if it's a data variable on the right mesh.
   ierr = nf90_inq_varid(ncid, trim(meshname)//'_'//trim(varname), iv)
   if (ierr /= nf90_noerr) then
      ! Do a second try with the varname without the meshname prefix.
      ierr = nf90_inq_varid(ncid, trim(varname), iv)
   end if
   if (ierr /= nf90_noerr) then
      ug_messagestr = 'ug_inc_varid: no candidate variable could be found for name'''//trim(varname)//'''.'
      ierr = UG_VAR_NOTFOUND
      goto 999
   end if

   str = ''
   ierr = nf90_get_att(ncid, iv, 'mesh', str)
   if (ierr /= nf90_noerr) then
      ! No UGRID :mesh attribute, discard this var.
      ug_messagestr = 'ug_inc_varid: candidate variable for name '''//trim(varname)//''' has no :mesh attribute.'
      ierr = UG_INVALID_MESHNAME
      goto 999
   end if
      
   if (.not.strcmpi(str,meshname)) then
      ! Mesh names do not match
      ug_messagestr = 'ug_inc_varid: candidate variable for name '''//trim(varname)//''' on mesh '''//trim(meshname)//''' has different :mesh attribute '''//trim(str)//'''.'
      ierr = UG_INVALID_MESHNAME
      goto 999
   end if

   varid = iv
   ierr = UG_NOERR
   return ! Return with success

999 continue
    ! Some error (status was set earlier)

end function ug_inq_varid


!> Writes the given edge type variable to the given netcdf file.
subroutine write_edge_type_variable(igeomfile, meshids, meshName, edge_type)
    implicit none

    integer, intent(in)            :: igeomfile    !< file pointer to netcdf file to write to.
    type(t_ug_meshids), intent(in) :: meshids      !< Set of NetCDF-ids for all mesh geometry variables.
    character(len=*),   intent(in) :: meshName     !< Name of the mesh.
    integer, intent(in)            :: edge_type(:) !< Edge type variable to be written to the NetCDF file.

    integer                        :: id_edgetype !< Variable ID for edge type variable.
    integer                        :: was_in_define_mode
    integer                        :: ierr !< Result status (UG_NOERR==NF90_NOERR if successful).

    ierr = UG_NOERR

    ! Put netcdf file in define mode.
    was_in_define_mode = 0
    ierr = nf90_redef(igeomfile)
    if (ierr == nf90_eindefine) then
        was_in_define_mode = 1 ! If was still in define mode.
    end if
    ierr = UG_NOERR

    ! Define edge type variable.
    ierr = ug_def_var(igeomfile, meshids, id_edgetype, (/ meshids%id_edgedim /), nf90_int, UG_LOC_EDGE, &
                      meshName, 'edge_type', '', 'edge type (relation between edge and flow geometry)', '', 'mean', ifill=-999)
    ierr = nf90_put_att(igeomfile, id_edgetype, 'flag_values',   (/ UG_EDGETYPE_INTERNAL_CLOSED, UG_EDGETYPE_INTERNAL, UG_EDGETYPE_BND, UG_EDGETYPE_BND_CLOSED /))
    ierr = nf90_put_att(igeomfile, id_edgetype, 'flag_meanings', 'internal_closed internal boundary boundary_closed')

    ! Put netcdf file in write mode.
    ierr = nf90_enddef(igeomfile)

    ! Write edge type variable.
    ierr = nf90_put_var(igeomfile, id_edgetype, edge_type)

    ! Leave the dataset in the same mode as we got it.
    if (was_in_define_mode == 1) then
        ierr = nf90_redef(igeomfile)
    end if

end subroutine write_edge_type_variable

!> Creates and initializes mesh geometry that contains the 2D unstructured network and edge type array.
!! NOTE: this routine is currently only a TEST GEOMETRY CREATOR
!!
!! NOTE: do not pass already filled mesh geometries to this function,
!! since array pointers will become disassociated, possibly causing memory leaks.
function ug_create_ugrid_geometry(meshgeom) result(ierr)   
    type(t_ug_meshgeom), intent(out) :: meshgeom     !< The mesh geometry that is to be created and filled.
    
    type(t_crs), target, save :: crs  !< The coordinate system
    
    ! TODO why need save here?
    integer, allocatable, target, save       :: edge_nodes(:,:), edge_faces(:,:), face_nodes(:,:), face_edges(:,:), face_links(:,:) !< Output arrays.
    
    type (t_face), pointer        :: netcell(:) 
    real(kind=dp), allocatable, target, save :: edgex(:), edgey(:) !< Output coordinate arrays.
    integer                                  :: edge, face, maxNodesPerFace, nodesPerFace, nump !< Counters.
    integer, parameter                       :: missing_value = -999
    double precision, parameter              :: dmiss = -999.0
    integer                                  :: ierr !< Result status (UG_NOERR==NF90_NOERR if successful).

    ierr = UG_NOERR

    ! Create 2D mesh geometry that contains all 2D faces, edges and nodes.
    ierr = ug_new_meshgeom(meshgeom)
    
    meshgeom%meshName = 'mesh2d'
    meshgeom%dim = 2
        
    crs%is_spherical = .TRUE.
    crs%varname = "wgs84"
    crs%epsg_code = 4326
    
    meshgeom%crs => crs


    ! Nodes.
    meshgeom%numNode = 5
    
    ! Get node coordinates.
    allocate(meshgeom%nodex(5))
    meshgeom%nodex(1) = 0.
    meshgeom%nodex(2) = 10.
    meshgeom%nodex(3) = 15.
    meshgeom%nodex(4) = 10.
    meshgeom%nodex(5) = 5.
    
    allocate(meshgeom%nodey(5))
    meshgeom%nodey(1) = 0.
    meshgeom%nodey(2) = 0.
    meshgeom%nodey(3) = 5.
    meshgeom%nodey(4) = 10.
    meshgeom%nodey(5) = 5.     

    ! Edges.
    ! Use only 2D net links (= edges).
    meshgeom%numEdge = 6

    ! Get edge nodes connectivity, edge types and edge coordinates (ordered as follows: first flow links, then closed edges).
    allocate(edge_nodes(2, meshgeom%numEdge)) 
    edge_nodes = missing_value
    
    allocate(edgex(meshgeom%numEdge))
    allocate(edgey(meshgeom%numEdge))
    edgex = dmiss
    edgey = dmiss

    edge_nodes(1,1) = 5
    edge_nodes(2,1) = 2
    edge_nodes(1,2) = 2
    edge_nodes(2,2) = 1
    edge_nodes(1,3) = 1
    edge_nodes(2,3) = 5
    edge_nodes(1,4) = 5
    edge_nodes(2,4) = 4
    edge_nodes(1,5) = 4
    edge_nodes(2,5) = 3
    edge_nodes(1,6) = 3
    edge_nodes(2,6) = 2
    
    meshgeom%edge_nodes => edge_nodes
    
    meshgeom%edgex => edgex
    meshgeom%edgey => edgey
    ! Edge z coordinates are unknown.

    ! Get edge faces connectivity.
    allocate(edge_faces( 2, meshgeom%numEdge ))
    ! Here need to use reverse_edge_mapping_table to map edges to net links, because edges are ordered as follows: first flow links, then closed edges.
    edge_faces(1:2, 1) = (/ 1, 2/)
    edge_faces(1:2, 2) = (/ 1, 0/)
    edge_faces(1:2, 3) = (/ 1, 0/)
    edge_faces(1:2, 4) = (/ 2, 0/)
    edge_faces(1:2, 5) = (/ 2, 0/)
    edge_faces(1:2, 6) = (/ 2, 0/)

    do edge = 1,meshgeom%numEdge
        ! 0 means no face, i.e. edge is on the boundary of the mesh.
        ! Replace zeroes with missing values.
        if (edge_faces(1, edge) == 0) edge_faces(1, edge) = missing_value
        if (edge_faces(2, edge) == 0) edge_faces(2, edge) = missing_value
    end do
    meshgeom%edge_faces => edge_faces


    ! Faces.
    ! Use only 2D internal net cells = 2D internal flow nodes (= faces).
    nump = 2 
    meshgeom%numFace = nump

    ! Get face coordinates.
    allocate(meshgeom%facex(2))
    meshgeom%facex(1) = 5
    meshgeom%facex(2) = 10
    
    allocate(meshgeom%facey(2))
    meshgeom%facey(1) = 2.5
    meshgeom%facey(2) = 5
        
    ! Determine max nr of net nodes per 2D net cell = face.
    maxNodesPerFace = 4
    meshgeom%maxNumFaceNodes = maxNodesPerFace
  
    ! Get face nodes connectivity, face edges connectivity and face-face connectivity.
    allocate(face_nodes(maxNodesPerFace, meshgeom%numFace))
    face_nodes = missing_value
    
    allocate(face_edges(maxNodesPerFace, meshgeom%numFace))
    face_edges = missing_value
    
    allocate(face_links(maxNodesPerFace, meshgeom%numFace))
    face_links = missing_value
    
    allocate(netcell(2))
    netcell(1)%n = 3
    netcell(2)%n = 4
    
    allocate(netcell(1)%nod(3))
    netcell(1)%nod(1) = 1
    netcell(1)%nod(2) = 2
    netcell(1)%nod(3) = 5
    
    allocate(netcell(2)%nod(4))
    netcell(2)%nod(1) = 2
    netcell(2)%nod(2) = 3
    netcell(2)%nod(3) = 4
    netcell(2)%nod(4) = 5
    
    do face = 1,nump
        nodesPerFace = netcell(face)%n
        face_nodes(1:nodesPerFace, face) = netcell(face)%nod        
    end do
    
    meshgeom%face_nodes => face_nodes
    meshgeom%face_edges => face_edges
    meshgeom%face_links => face_links    

end function ug_create_ugrid_geometry

subroutine ug_create_ugrid_meta(meta)
    type(t_ug_meta) :: meta  !< Meta information on file.
        
    meta%institution = "Deltares"
    meta%source      = "DeltaShell"
    meta%references  = "geen idee"    
    meta%version     = "0"
    meta%modelname   = "manual"
    
end subroutine ug_create_ugrid_meta

!> Writes the unstructured network and edge type to an already opened netCDF dataset.
function ug_write_geom_filepointer_ugrid(ncid, meshgeom, meshids) result(ierr)
    integer,             intent(in)    :: ncid !< file pointer to netcdf file to write to.
    type(t_ug_meshgeom), intent(in)    :: meshgeom !< Mesh geometry to be written to the NetCDF file.
    type(t_ug_meshids),  intent(  out) :: meshids  !< Set of NetCDF-ids for all mesh geometry variables.
    integer                           :: ierr     !< Result status (UG_NOERR==NF90_NOERR if successful).

    type(t_ug_meta)                    :: meta  !< Meta information on file. ! TODO: later also input arg
    
    ierr = UG_NOERR

    ! create default meta
    call ug_create_ugrid_meta(meta)
    
    ! Add global attributes to NetCDF file.
    ierr = ug_addglobalatts(ncid, meta)
    
    ! Write mesh geometry.
    ierr = ug_write_mesh_struct(ncid, meshids, meshgeom)

end function ug_write_geom_filepointer_ugrid

!> Writes the unstructured network and edge type to a netCDF file.
!! If file exists, it will be overwritten.
function ug_write_geom_ugrid(filename) result(ierr)

    character(len=*), intent(in) :: filename
    integer :: ierr

    type(t_ug_meshgeom) :: meshgeom !< Mesh geometry to be written to the NetCDF file.
    type(t_ug_meshids)  :: meshids  !< Set of NetCDF-ids for all mesh geometry variables.
    integer :: ncid
    
    ierr = nf90_create(filename, 0, ncid)
    if (ierr /= nf90_noerr) then
        return
    end if

    ! create mesh geometry
    ierr = ug_create_ugrid_geometry(meshgeom)

    ierr = ug_write_geom_filepointer_ugrid(ncid, meshgeom, meshids)
         
    ierr = nf90_close(ncid)
        
end function ug_write_geom_ugrid


!> Writes the unstructured network and edge type AND time-dep output data to a netCDF file.
!! If file exists, it will be overwritten.
function ug_write_map_ugrid(filename) result(ierr)

    character(len=*), intent(in) :: filename
    integer :: ierr

    type(t_ug_meshgeom)   :: meshgeom !< Mesh geometry to be written to the NetCDF file.
    type(t_ug_meshids)    :: meshids  !< Set of NetCDF-ids for all mesh geometry variables.
    integer :: id_s1, id_s2, id_u1, id_zk, id_time, itim ! example: water levels, water depth, edge speed, bed level and a timer
    integer :: ncid, id_timedim
    double precision, allocatable :: workf(:), worke(:), workn(:)

    ! NOTE: this routine is currently only a TEST FILE WRITER

    ! TODO: some if, to only do this at first time step
    ierr = nf90_create(filename, 0, ncid)
    if (ierr /= nf90_noerr) then
        return
    end if

    ! create mesh geometry
    ierr = ug_create_ugrid_geometry(meshgeom)

    ierr = ug_write_geom_filepointer_ugrid(ncid, meshgeom, meshids)

    ierr = nf90_def_dim(ncid, 'time', nf90_unlimited, id_timedim)

    ierr = nf90_def_var(ncid, 'time', nf90_double, id_timedim, id_time)
    ierr = nf90_put_att(ncid, id_time, 'standard_name', 'time')
    ierr = nf90_put_att(ncid, id_time, 'units'        , 'seconds since 2008-01-09 00:00:00')

    ierr = ug_def_var(ncid, meshids, id_s1, (/ meshids%id_facedim, id_timedim /), nf90_double, UG_LOC_FACE, meshgeom%meshname, "s1", "sea_surface_level_above_geoid", "Water level on cell centres", &
                    "m", "average", meshgeom%crs, -1, -999d0)
    
    ierr = ug_def_var(ncid, meshids, id_s2, (/ meshids%id_facedim, id_timedim /), nf90_double, UG_LOC_FACE, meshgeom%meshname, "s2", "sea_floor_depth_below_geoid", "Water depth on cell centres", &
                    "m", "average", meshgeom%crs, -1, -999d0)
    
    ierr = ug_def_var(ncid, meshids, id_u1, (/ meshids%id_edgedim, id_timedim /), nf90_double, UG_LOC_EDGE, meshgeom%meshname, "u1", "", "Normal velocity on cell edges", &
                    "m s-1", "average", meshgeom%crs, -1, -999d0)
    
    ierr = ug_def_var(ncid, meshids, id_zk, (/ meshids%id_nodedim, id_timedim /), nf90_double, UG_LOC_NODE, meshgeom%meshname, "zk", "", "Bed level on cell corners", &
                    "m", "point", meshgeom%crs, -1, -999d0)
    ! NOTE: zk is rarely time-dependent, but just as an example

    ierr = nf90_enddef(ncid)

    allocate(workf(meshgeom%numface))
    workf = 1.23d0 ! TODO: make this hardcoded spatially varying.
    allocate(worke(meshgeom%numedge))
    worke = 3.45d-2
    allocate(workn(meshgeom%numnode))
    workn = -7.68d0
    do itim=1,10
       
        workf(:) = workf(:) + itim ! Dummy data time-dependent
        ierr = nf90_put_var(ncid, id_time, dble(itim))

        ierr = nf90_put_var(ncid, id_s1, workf, count = (/ meshgeom%numface, 1 /), start = (/ 1, itim /))        
        ierr = nf90_put_var(ncid, id_s2, workf+5d0, count = (/ meshgeom%numface, 1 /), start = (/ 1, itim /))

        worke(:) = worke(:) + itim*.01d0 ! Dummy data time-dependent
        ierr = nf90_put_var(ncid, id_u1, worke, count = (/ meshgeom%numedge, 1 /), start = (/ 1, itim /))

        workn(:) = workn(:) + itim*.1d0 ! Dummy data time-dependent
        ierr = nf90_put_var(ncid, id_zk, workn, count = (/ meshgeom%numnode, 1 /), start = (/ 1, itim /))

    end do

    ! ..
    deallocate(workn, worke, workf)
    ierr = nf90_close(ncid)
        
end function ug_write_map_ugrid

! UGRID mesh and network1d functions 

!> This function creates a 1d network accordingly to the new 1d format. 
!! To be called only if the network 1d is defined using the new rules (a preliminary check is required)
function ug_create_1d_network(ncid,meshids,networkName, nNodes, nBranches,nGeometry) result(ierr)

   integer, intent(in)               :: ncid 
   character(len=*), intent(in)      :: networkName
   integer, intent(in)               :: nNodes,nBranches,nGeometry
   integer                           :: ierr, wasInDefine
   type(t_ug_meshids)                :: meshids
      
   ierr = UG_SOMEERR
   wasInDefine = 0
   ierr = nf90_redef(ncid) !open NetCDF in define mode
    
   ierr  = nf90_def_dim(ncid, 'nNetworkBranches', nBranches, meshids%id_1dbranchesdim)
   ierr  = nf90_def_dim(ncid, 'nNetworkNodes'   , nNodes,    meshids%id_1dnodesdim)
   ierr  = nf90_def_dim(ncid, 'nGeometryNodes'  , nGeometry, meshids%id_1dgeopointsdim)
   ierr  = nf90_def_dim( ncid,'idstrlength'     , 30, meshids%id_1didstrlength) 
   ierr  = nf90_def_dim( ncid,'longstrlength'   , 80, meshids%id_1dlongstrlength) 
   ierr  = nf90_def_dim( ncid,'network1D_edge_nodes_dim', nBranches * 2, meshids%id_1dedgenodesdim) 

   !Variable declarations: Network1d
   !1. Network1D
   ierr = nf90_def_var(ncid, networkName, nf90_int, meshids%id_1dtopo)
   ierr = nf90_put_att(ncid, meshids%id_1dtopo, 'cf_role', 'mesh_topology')
   ierr = nf90_put_att(ncid, meshids%id_1dtopo, 'topology_dimension', 1)
   ierr = nf90_put_att(ncid, meshids%id_1dtopo, 'edge_dimension', 'nNetworkBranches')
   ierr = nf90_put_att(ncid, meshids%id_1dtopo, 'edge_node_connectivity', 'network1D_edge_nodes')
   ierr = nf90_put_att(ncid, meshids%id_1dtopo, 'edge_geometry', 'network1D_geometry')
   ierr = nf90_put_att(ncid, meshids%id_1dtopo, 'node_dimension', 'nNetworkNodes')
   ierr = nf90_put_att(ncid, meshids%id_1dtopo, 'node_coordinates', 'network1D_nodes_x network1D_nodes_y')

   !2. Branch: the start and the end nodes of each branch
   ierr = nf90_def_var(ncid, 'network1D_edge_nodes', nf90_int, meshids%id_1dedgenodesdim, meshids%id_1dedgenodes)
   ierr = nf90_put_att(ncid, meshids%id_1dedgenodes, 'cf_role', 'edge_node_connectivity')
   ierr = nf90_put_att(ncid, meshids%id_1dedgenodes, 'long_name', 'start and end nodes of each branch in the network')
   !2. Branch: the branch ids
   ierr = nf90_def_var(ncid, 'network1D_branches_ids', nf90_char, (/ meshids%id_1didstrlength, meshids%id_1dbranchesdim /) , meshids%id_1dbranchids)
   ierr = nf90_put_att(ncid, meshids%id_1dbranchids, 'long_name', 'ids of the branches')
   !2. Branch: the long names of the branches
   ierr = nf90_def_var(ncid, 'network1D_branches_long_name', nf90_char, (/ meshids%id_1dlongstrlength, meshids%id_1dbranchesdim /) , meshids%id_1dbranchlongnames)
   ierr = nf90_put_att(ncid, meshids%id_1dbranchlongnames, 'long_name', 'the branches long names')
   !2. Branch: the branch lengths
   ierr = nf90_def_var(ncid, 'network1D_branches_lengths', nf90_double, (/ meshids%id_1dbranchesdim /) , meshids%id_1dbranchlengths)
   ierr = nf90_put_att(ncid, meshids%id_1dbranchlengths, 'long_name', 'the branch lengths')
 

   !3. Nodes: the ids of the nodes
   ierr = nf90_def_var(ncid, 'network1D_node_ids', nf90_char, (/ meshids%id_1didstrlength, meshids%id_1dnodesdim /) , meshids%id_1dnodIds)
   ierr = nf90_put_att(ncid, meshids%id_1dnodIds, 'long_name', 'ids of the network connection nodes')
   !3. Nodes: the long names of the nodes
   ierr = nf90_def_var(ncid, 'network1D_nodes_long_names', nf90_char, (/ meshids%id_1dlongstrlength, meshids%id_1dnodesdim /) , meshids%id_1dnodlongnames)
   ierr = nf90_put_att(ncid, meshids%id_1dnodlongnames, 'long_name', 'long names of the network connection nodes')
   !3. Nodes: x coord
   ierr = nf90_def_var(ncid, 'network1D_nodes_x', nf90_double, (/ meshids%id_1dnodesdim /) , meshids%id_1dnodex)
   ierr = nf90_put_att(ncid, meshids%id_1dnodex, 'standard_name', 'projection_x_coordinate')
   ierr = nf90_put_att(ncid, meshids%id_1dnodex, 'long_name', 'x coordinates of the network connection nodes')
   ierr = nf90_put_att(ncid, meshids%id_1dnodex, 'units', 'm')
   !3. Nodes: y coord
   ierr = nf90_def_var(ncid, 'network1D_nodes_y', nf90_double, (/ meshids%id_1dnodesdim /) , meshids%id_1dnodey)
   ierr = nf90_put_att(ncid, meshids%id_1dnodey, 'standard_name', 'projection_x_coordinate')
   ierr = nf90_put_att(ncid, meshids%id_1dnodey, 'long_name', 'y coordinates of the network connection nodes')
   ierr = nf90_put_att(ncid, meshids%id_1dnodey, 'units', 'm')

   !4. Geometry
   ierr = nf90_def_var(ncid, 'network1D_geometry', nf90_int, meshids%id_1dgeometry)
   ierr = nf90_put_att(ncid, meshids%id_1dgeometry, 'geometry_type', 'multiline')
   ierr = nf90_put_att(ncid, meshids%id_1dgeometry, 'node_count', 'nGeometryNodes')
   ierr = nf90_put_att(ncid, meshids%id_1dgeometry, 'part_node_count', 'network1D_part_node_count')
   ierr = nf90_put_att(ncid, meshids%id_1dgeometry, 'node_coordinates', 'network1D_geom_x network1D_geom_y')
   !4. Geometry: number of geometry points per each branch
   ierr = nf90_def_var(ncid, 'network1D_part_node_count', nf90_int, (/ meshids%id_1dbranchesdim /) , meshids%id_1dgeopointsperbranch)
   ierr = nf90_put_att(ncid, meshids%id_1dgeopointsperbranch, 'long_name', 'number of geometry nodes per branch')
   !4. Geometry points x coordinates
   ierr = nf90_def_var(ncid, 'network1D_geom_x', nf90_double, (/ meshids%id_1dgeopointsdim /) , meshids%id_1dgeox)
   ierr = nf90_put_att(ncid, meshids%id_1dgeox, 'standard_name', 'projection_x_coordinate')
   ierr = nf90_put_att(ncid, meshids%id_1dgeox, 'long_name', 'x coordinates of the branch geometries')
   ierr = nf90_put_att(ncid, meshids%id_1dgeox, 'units', 'm')
   ierr = nf90_put_att(ncid, meshids%id_1dgeox, 'cf_role', 'geometry_x_node')
   !4. Geometry points y coordinates
   ierr = nf90_def_var(ncid, 'network1D_geom_y', nf90_double, (/ meshids%id_1dgeopointsdim /) , meshids%id_1dgeoy)
   ierr = nf90_put_att(ncid, meshids%id_1dgeoy, 'standard_name', 'projection_y_coordinate')
   ierr = nf90_put_att(ncid, meshids%id_1dgeoy, 'long_name', 'y coordinates of the branch geometries')
   ierr = nf90_put_att(ncid, meshids%id_1dgeoy, 'units', 'm')
   ierr = nf90_put_att(ncid, meshids%id_1dgeoy, 'cf_role', 'geometry_y_node')

   ierr = nf90_enddef(ncid)
   
end function ug_create_1d_network

!> This function creates a 1d mesh accordingly to the new 1d format. 
!! To be called only if the network 1d is defined using the new rules (a preliminary check is required)
function ug_create_1d_mesh(ncid,meshids,nmeshpoints) result(ierr)
   
   integer, intent(in)               :: nmeshpoints
   type(t_ug_meshids), intent(inout) :: meshids   
   integer                           :: ierr,ncid

   ierr = UG_SOMEERR
   ierr = nf90_redef(ncid) !open NetCDF in define mode
   
   !define dim
   ierr  = nf90_def_dim(ncid, 'nMeshNodes', nmeshpoints, meshids%id_1dmeshpoints)
   
   !define mesh1d accordingly to the UGRID format
   ierr = nf90_def_var(ncid, 'mesh1D', nf90_int, meshids%id_1dmeshtopo)
   ierr = nf90_put_att(ncid, meshids%id_1dmeshtopo, 'cf_role','mesh_topology')
   ierr = nf90_put_att(ncid, meshids%id_1dmeshtopo, 'topology_dimension', 1)
   ierr = nf90_put_att(ncid, meshids%id_1dmeshtopo, 'coordinate_space', 'network1D') 
   ierr = nf90_put_att(ncid, meshids%id_1dmeshtopo, 'edge_dimension','nMeshEdges')
   ierr = nf90_put_att(ncid, meshids%id_1dmeshtopo, 'edge_node_connectivity','mesh1D_edge_nodes')
   ierr = nf90_put_att(ncid, meshids%id_1dmeshtopo, 'node_dimension','nMeshNodes')
   ierr = nf90_put_att(ncid, meshids%id_1dmeshtopo, 'node_coordinates','mesh1D_nodes_branch_id mesh1D_nodes_branch_offset')
   
   ! 1. mesh1D :assign the branch number to each node
   ierr = nf90_def_var(ncid, 'mesh1D_nodes_branch_id', nf90_int, (/ meshids%id_1dnodesdim /) , meshids%id_1dmeshtobranch)
   ierr = nf90_put_att(ncid, meshids%id_1dmeshtobranch, 'long_name', 'Number of the branch on which the node is located')
   ! 1. mesh1D :assign the the offset from the starting node
   ierr = nf90_def_var(ncid, 'mesh1D_nodes_branch_offset', nf90_double, (/ meshids%id_1dnodesdim /) , meshids%id_1doffset)
   ierr = nf90_put_att(ncid, meshids%id_1doffset, 'long_name', 'Offset along the branch at which the node is located')

   ierr = nf90_enddef(ncid)

end function ug_create_1d_mesh

!> This function writes the nodes of the 1d network
function ug_write_1d_network_nodes(ncid,meshids, nodesX, nodesY, nodeIds, nodeLongnames) result(ierr)

   integer, intent(in)               :: ncid
   type(t_ug_meshids), intent(in)    :: meshids !< Set of NetCDF-ids for all mesh geometry arrays
   integer                           :: ierr
   character(len=*), intent(in)      :: nodeIds(:),nodeLongnames(:)
   double precision, intent(in)      :: nodesX(:), nodesY(:)
   
   
   ierr = UG_SOMEERR
   !Put the NetCDF in write mode
   ierr = nf90_enddef(ncid)
   
   ierr = nf90_put_var(ncid, meshids%id_1dnodex, nodesX)
   ierr = nf90_put_var(ncid, meshids%id_1dnodey, nodesY)
   ierr = nf90_put_var(ncid, meshids%id_1dnodIds, nodeIds) 
   ierr = nf90_put_var(ncid, meshids%id_1dnodLongNames, nodeLongnames)

end function ug_write_1d_network_nodes

!> This function writes the branches information
function ug_write_1d_network_branches(ncid,meshids, sourceNodeId,targetNodeId, branchIds, branchlengths, branchlongnames, nbranchgeometrynodes,nBranches) result(ierr)

   integer, intent(in)               ::ncid, nBranches
   type(t_ug_meshids), intent(in)    ::meshids !< Set of NetCDF-ids for all mesh geometry arrays
   integer,           intent(in)     ::sourceNodeId(:),targetNodeId(:)
   integer,           allocatable    ::sourcestargets(:)
   character(len=*),  intent(in)     ::branchIds(:)
   double precision,  intent(in)     ::branchlengths(:) 
   character(len=*),  intent(in)     ::branchlongnames(:)
   integer,           intent(in)     ::nbranchgeometrynodes(:)
   integer                           ::ierr, wasInDefine, n, k
   
   ierr = UG_SOMEERR
   ierr = nf90_enddef(ncid)
   
   allocate(sourcestargets(nBranches*2))
   k = 0
   do n=1,nBranches
       k = k + 1
       sourcestargets(k)=sourceNodeId(n) 
       k = k + 1
       sourcestargets(k)=targetNodeId(n)
   end do
   
   ierr = nf90_put_var(ncid, meshids%id_1dedgenodes, sourcestargets)
   ierr = nf90_put_var(ncid, meshids%id_1dbranchids, branchIds(1:nBranches))  
   ierr = nf90_put_var(ncid, meshids%id_1dbranchlongnames, branchLongnames(1:nBranches)) 
   ierr = nf90_put_var(ncid, meshids%id_1dbranchlengths, branchlengths(1:nBranches)) 
   ierr = nf90_put_var(ncid, meshids%id_1dgeopointsperbranch, nbranchgeometrynodes(1:nBranches)) 
  
end function ug_write_1d_network_branches

!> This function writes the geometry points
function ug_write_1d_network_branches_geometry(ncid,meshids, geopointsX, geopointsY, ngeometry)  result(ierr)

   integer, intent(in)               :: ncid, ngeometry
   type(t_ug_meshids), intent(in)    :: meshids 
   double precision,  intent(in)     :: geopointsX(:), geopointsY(:) 
   integer                           :: ierr
   
   ierr = UG_SOMEERR
   ierr = nf90_enddef(ncid)
   
   ierr = nf90_put_var(ncid, meshids%id_1dgeox, geopointsX(1:ngeometry))
   ierr = nf90_put_var(ncid, meshids%id_1dgeoy, geopointsY(1:ngeometry))
   
end function ug_write_1d_network_branches_geometry

!> This function writes the mesh points
function ug_write_1d_mesh_discretisation_points(ncid, meshids, branchidx, offset) result(ierr)

   integer, intent(in)                :: ncid, branchidx(:)
   double precision, intent(in)       :: offset(:)
   type(t_ug_meshids), intent(in)     :: meshids 
   integer                            :: ierr,nmeshpoints

   ierr = UG_SOMEERR
   ierr = nf90_enddef(ncid)
   
   ierr = nf90_inquire_dimension(ncid, meshids%id_1dmeshpoints, len=nmeshpoints)
   if(ierr /= UG_NOERR) then 
       Call SetMessage(Level_Fatal, 'could not read the branch dimension')
   end if 
   
   ierr = nf90_put_var(ncid, meshids%id_1dmeshtobranch, branchidx)
   ierr = nf90_put_var(ncid, meshids%id_1doffset, offset)

end function ug_write_1d_mesh_discretisation_points

!> This function gets the number of network nodes
function ug_get_1d_network_nodes_count(ncid,meshids, nNodes) result(ierr)

   integer, intent(in)               :: ncid
   type(t_ug_meshids), intent(in)    :: meshids 
   integer, intent(out)              :: nNodes
   integer                           :: ierr
   
  ierr = nf90_inquire_dimension(ncid, meshids%id_1dnodesdim, len=nNodes)
  if(ierr /= UG_NOERR) then 
       Call SetMessage(Level_Fatal, 'could not read the 1d nodes count')
  end if 
   
end function ug_get_1d_network_nodes_count

!> This function gets the number of branches
function ug_get_1d_network_branches_count(ncid,meshids, nbranches) result(ierr)

   integer, intent(in)               :: ncid
   type(t_ug_meshids), intent(in)    :: meshids 
   integer, intent(out)              :: nbranches
   integer                           :: ierr
   
  ierr = nf90_inquire_dimension(ncid, meshids%id_1dbranchesdim, len=nbranches)
  if(ierr /= UG_NOERR) then 
       Call SetMessage(Level_Fatal, 'could not read the 1d number of branches')
  end if 
   
end function ug_get_1d_network_branches_count

!> This function gets the number of geometry points
function ug_get_1d_network_branches_geometry_coordinate_count(ncid,meshids, ngeometry) result(ierr)

   integer, intent(in)               :: ncid
   type(t_ug_meshids), intent(in)    :: meshids 
   integer, intent(out)              :: ngeometry
   integer                           :: ierr
   
  ierr = nf90_inquire_dimension(ncid, meshids%id_1dgeopointsdim, len=ngeometry)
    if(ierr /= UG_NOERR) then 
       Call SetMessage(Level_Fatal, 'could not read the 1d number of geometry points')
  end if 

end function ug_get_1d_network_branches_geometry_coordinate_count

!> This function gets the coordinates of the network nodes
function ug_read_1d_network_nodes(ncid, meshids, nodesX, nodesY, nodeids, nodelongnames) result(ierr)

   integer, intent(in)             :: ncid
   type(t_ug_meshids), intent(in)  :: meshids 
   double precision,intent(out)    :: nodesX(:), nodesY(:) 
   character(len=*),intent(out)    :: nodeids(:), nodelongnames(:)
   integer                         :: ierr
 
   ierr = nf90_get_var(ncid, meshids%id_1dnodex, nodesX)
   if(ierr /= UG_NOERR) then 
       Call SetMessage(Level_Fatal, 'could not read x coordinates of 1d network')
   end if 
   
   ierr = nf90_get_var(ncid, meshids%id_1dnodey, nodesY)
   if(ierr /= UG_NOERR) then 
       Call SetMessage(Level_Fatal, 'could not read x coordinates of 1d network')
   end if 
   
   ierr = nf90_get_var(ncid, meshids%id_1dnodIds, nodeids)
   if(ierr /= UG_NOERR) then 
       Call SetMessage(Level_Fatal, 'could not read nodeids of 1d network')
   end if 
   
   ierr = nf90_get_var(ncid, meshids%id_1dnodlongnames, nodelongnames)
   if(ierr /= UG_NOERR) then 
       Call SetMessage(Level_Fatal, 'could not read nodelongnames of 1d network')
   end if 

end function ug_read_1d_network_nodes

!> This function reads the network branches
function ug_read_1d_network_branches(ncid, meshids, sourcenodeid, targetnodeid, branchid, branchlengths, branchlongnames, nbranchgeometrypoints) result(ierr)

   integer, intent(in)              :: ncid
   type(t_ug_meshids), intent(in)   :: meshids 
   integer,intent(out)              :: sourcenodeid(:), targetnodeid(:),nbranchgeometrypoints(:) 
   real(kind=dp),intent(out)        :: branchlengths(:)
   character(len=*),intent(out)     :: branchid(:),branchlongnames(:)
   integer                          :: ierr,n, nbranches, k
   integer,allocatable              :: sourcestargets(:)
   
   nbranches = size(sourceNodeId,1)
   allocate(sourcestargets(nbranches * 2))
   
   ierr = nf90_get_var(ncid, meshids%id_1dedgenodes, sourcestargets)
   if(ierr /= UG_NOERR) then 
       Call SetMessage(Level_Fatal, 'could not read the source and targets nodes of each branch in 1d network')
   end if 
   k = 0
   do n=1,nBranches
       k = k + 1
       sourceNodeId(n)=sourcestargets(k)
       k = k + 1
       targetNodeId(n)=sourcestargets(k)
   end do

   ierr = nf90_get_var(ncid, meshids%id_1dbranchids, branchId)
   if(ierr /= UG_NOERR) then 
       Call SetMessage(Level_Fatal, 'could not read the branch ids of 1d network')
   end if 
   
   ierr = nf90_get_var(ncid, meshids%id_1dbranchlengths, branchlengths)
   if(ierr /= UG_NOERR) then 
       Call SetMessage(Level_Fatal, 'could not read the branch lengths of 1d network')
   end if 
   
   ierr = nf90_get_var(ncid, meshids%id_1dbranchlongnames, branchlongnames)
   if(ierr /= UG_NOERR) then 
       Call SetMessage(Level_Fatal, 'could not read the branch longnames of 1d network')
   end if 
   
   ierr = nf90_get_var(ncid, meshids%id_1dgeopointsperbranch, nbranchgeometrypoints)
   if(ierr /= UG_NOERR) then 
       Call SetMessage(Level_Fatal, 'could not read the geometry points of each branch in 1d network')
   end if 
   
end function ug_read_1d_network_branches

!> This function reads the coordinates of the geometry points
function ug_read_1d_network_branches_geometry(ncid, meshids, geopointsX, geopointsY) result(ierr)

   integer, intent(in)                      :: ncid
   type(t_ug_meshids), intent(in)           :: meshids 
   real(kind=dp), intent(out)               :: geopointsX(:), geopointsY(:)
   integer                                  :: ierr
         
   ierr = nf90_get_var(ncid, meshids%id_1dgeox, geopointsX)
   if(ierr /= UG_NOERR) then 
       Call SetMessage(Level_Fatal, 'could not read the x coordinates of the geometry points')
   end if 
   
   ierr = nf90_get_var(ncid, meshids%id_1dgeoy, geopointsY)
   if(ierr /= UG_NOERR) then 
       Call SetMessage(Level_Fatal, 'could not read the y coordinates of the geometry points')
   end if 

end function ug_read_1d_network_branches_geometry

!> This function gets the number of mesh points
function ug_get_1d_mesh_discretisation_points_count(ncid, meshids, nmeshpoints) result(ierr)

   integer, intent(in)               :: ncid
   type(t_ug_meshids), intent(in)    :: meshids 
   integer, intent(out)              :: nmeshpoints
   integer                           :: ierr
   
   ierr = nf90_inquire_dimension(ncid, meshids%id_1dmeshpoints, len=nmeshpoints)
   if(ierr /= UG_NOERR) then 
       Call SetMessage(Level_Fatal, 'could not read the number of mesh points')
   end if 
   
end function ug_get_1d_mesh_discretisation_points_count

!> This function reads the geometry information for the mesh points
function ug_read_1d_mesh_discretisation_points(ncid, meshids, branchidx, offset) result(ierr)

   integer, intent(in)                      :: ncid
   type(t_ug_meshids), intent(in)           :: meshids 
   real(kind=dp),   intent(out)             :: offset(:)
   integer,intent(out)                      :: branchidx(:)
   integer                                  :: ierr
         
   ierr = nf90_get_var(ncid, meshids%id_1dmeshtobranch, branchidx)
   !define dim
   if(ierr /= UG_NOERR) then 
       Call SetMessage(Level_Fatal, 'could not read the branch ids')
   end if 
   ierr = nf90_get_var(ncid, meshids%id_1doffset, offset)
   if(ierr /= UG_NOERR) then 
       Call SetMessage(Level_Fatal, 'could not read the branch offsets')
   end if 
    
end function  ug_read_1d_mesh_discretisation_points


end module io_ugrid
