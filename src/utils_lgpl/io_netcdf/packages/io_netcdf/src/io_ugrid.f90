!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
implicit none

! TODO: AvD: GL2: add 'full_grid_output' support, to write 1. face_edge_connectivity; 2. edge_face_connectivity; and possibly more.
! TODO: AvD: GL2: add cell_methods to edge/face data (:mean)
! TODO: AvD: GL2: add integer variable Mesh2_edge_bc with :flag_meanings = "none closed dirichlet"; :flag_values = 0, 1, 2 ;
! TODO: AvD: GL2: move grid_mapping attribute to all data variables, not coordinate variables.

!! Conventions
character(len=6), parameter :: UG_CONV_CF   = 'CF-1.6'      !< Version of CF conventions currently adopted.
character(len=9), parameter :: UG_CONV_UGRID = 'UGRID-0.9'  !< Version of UGRID conventions currently adopted.

!! Meta data
integer, parameter :: ug_strLenMeta = 100
type ug_meta
   character(len=ug_strLenMeta) :: institution    
   character(len=ug_strLenMeta) :: source         
   character(len=ug_strLenMeta) :: references     
   character(len=ug_strLenMeta) :: version        
   character(len=ug_strLenMeta) :: modelname      
end type ug_meta

!! Error codes
integer, parameter :: UG_NOERR                 = NF90_NOERR
integer, parameter :: UG_SOMEERR               = 10 !< Some unspecified error.
integer, parameter :: UG_INVALID_MESHNAME      = 11
integer, parameter :: UG_INVALID_MESHDIMENSION = 12
integer, parameter :: UG_INVALID_DATALOCATION  = 13
integer, parameter :: UG_INVALID_CRS           = 30 !< Invalid/missing coordinate reference system (using default)
integer, parameter :: UG_NOTIMPLEMENTED        = 99

!! Location types
integer, parameter :: UG_LOC_NONE = 0 !< Mesh data location: nowhere at all (include only required mesh locations)
integer, parameter :: UG_LOC_NODE = 1 !< Mesh data location: mesh node (corner)
integer, parameter :: UG_LOC_EDGE = 2 !< Mesh data location: mesh edge
integer, parameter :: UG_LOC_FACE = 4 !< Mesh data location: mesh face
integer, parameter :: UG_LOC_VOL  = 8 !< Mesh data location: mesh volume
integer, parameter :: UG_LOC_ALL2D = UG_LOC_NODE + UG_LOC_EDGE + UG_LOC_FACE !< All three possible 2D locations.

!! Dimension types (form a supplement to the preceding location types)
integer, parameter :: UG_DIM_MAXFACENODES = 128 !< The dimension containing the max number of nodes in the face_node_connectivity table.

!! Basics
integer, parameter :: dp=kind(1.0d00)
integer, parameter :: maxMessageLen = 1024
character(len=maxMessageLen) :: ug_messagestr !< Placeholder string for storing diagnostic messages. /see{ug_get_message}

!> Container for information for a NetCDF attribute. Used inside t_crs.
type nc_attribute
   character(len=64) :: attname  !< Name of the attribute.
   integer           :: xtype    !< Type: one of NF90_CHAR, NF90_INT, NF90_FLOAT, NF90_DOUBLE, NF90_BYTE, NF90_SHORT.
   integer           :: len      !< Length of the attribute value (string length/array length)
   character(len=1), allocatable :: strvalue(:) !< Contains value if xtype==NF90_CHAR.
   double precision, allocatable :: dblvalue(:) !< Contains value if xtype==NF90_DOUBLE.
   real,             allocatable :: fltvalue(:) !< Contains value if xtype==NF90_FLOAT.
   integer,          allocatable :: intvalue(:) !< Contains value if xtype==NF90_INT.
   ! TODO: AvD: support BYTE/short as well?
end type nc_attribute

!> Container for information about coordinate reference system in NetCDF-file.
type t_crs
   logical                         :: is_spherical  !< Whether or not spherical (otherwise some projected crs)
   character(len=64)               :: varname = ' ' !< Name of the NetCDF variable containing this CRS
   integer                         :: varvalue      !< Integer value (==epsg code)
   type(nc_attribute), allocatable :: attset(:)     !< General set with all/any attributes about this CRS.
end type t_crs

!> Structure for storing all variable ids for an unstructured mesh.
type t_ug_meshids
   !
   ! Dimensions:
   !
   integer :: id_nodedim         = -1 !< Dimension ID for nodes.
   integer :: id_edgedim         = -1 !< Dimension ID for edges.
   integer :: id_facedim         = -1 !< Dimension ID for faces.
   integer :: id_maxfacenodesdim = -1 !< Dimension ID for max nr of nodes per face.

   !
   ! Coordinate variables
   !
   integer :: id_nodex           = -1 !< Coordinate variable ID for node x-coordinate.
   integer :: id_nodey           = -1 !< Coordinate variable ID for node y-coordinate.
   integer :: id_nodez           = -1 !< Data       variable ID for node z-coordinate.
   integer :: id_edgex           = -1 !< Coordinate variable ID for edge x-coordinate.
   integer :: id_edgey           = -1 !< Coordinate variable ID for edge y-coordinate.
   integer :: id_edgexbnd        = -1 !<            variable ID for edge boundaries' x-coordinate.
   integer :: id_edgeybnd        = -1 !<            variable ID for edge boundaries' y-coordinate.
   integer :: id_facex           = -1 !< Coordinate variable ID for face x-coordinate.
   integer :: id_facey           = -1 !< Coordinate variable ID for face y-coordinate.
   integer :: id_facexbnd        = -1 !<            variable ID for face boundaries' x-coordinate.
   integer :: id_faceybnd        = -1 !<            variable ID for face boundaries' y-coordinate.
   
   !
   ! Topology variables
   !
   integer :: id_meshtopo        = -1 !< Top-level variable ID for mesh topology, collects all related variable names via attributes.
   integer :: id_edgenodes       = -1 !< Variable ID for edge-to-node mapping table.
   integer :: id_facenodes       = -1 !< Variable ID for face-to-node mapping table.
   integer :: id_edgefaces       = -1 !< Variable ID for edge-to-face mapping table (optional, can be -1).
   integer :: id_faceedges       = -1 !< Variable ID for face-to-edge mapping table (optional, can be -1).
   integer :: id_facelinks       = -1 !< Variable ID for face-to-face mapping table (optional, can be -1).

end type t_ug_meshids
!> Structure for storing an entire mesh geometry (topology and coordinates and more).
type t_ug_meshgeom
! TODO: AvD: extend this to 3D (volumes)
   character(len=256) :: meshname           !< Name of this mesh ! TODO: AvD: should this be in this data type?
   integer            :: dim                !< Dimensionality of the mesh (1/2/3)
   integer            :: numnode            !< Number of mesh nodes.
   integer            :: numedge            !< Number of mesh edges.
   integer            :: numface            !< Number of mesh faces.

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
   type (ug_meta), intent (in)   :: meta
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
   ierr = nf90_put_att(ncid, nf90_global,  'source',      trim(meta%source)//' '//trim(meta%version)//'. model: '//trim(meta%modelname))

   call date_and_time(cdate, ctime, czone)
   ierr = nf90_put_att(ncid, nf90_global,  'history', &
      'Created on '//cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)// &
      ', '//trim(meta%source))

   ierr = nf90_put_att(ncid, nf90_global,  'Conventions', trim(UG_CONV_CF)//' '//trim(UG_CONV_UGRID))

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

   meshgeom%crs   => null()

end function ug_new_meshgeom



! -- COORDINATES ------------
!> Adds coordinate attributes according to CF conventions, based on given coordinate projection type.
!! Non-standard attributes (such as long_name) should be set elsewhere.
function ug_addcoordatts(ncid, id_varx, id_vary, crs) result(ierr)
   integer,      intent(in) :: ncid     !< NetCDF dataset id
   integer,      intent(in) :: id_varx  !< NetCDF 'x' variable id
   integer,      intent(in) :: id_vary  !< NetCDF 'y' variable id
   type(t_crs),  intent(in) :: crs      !< Coordinate reference system for the x/y-coordinates variables.
   integer                  :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.

   ierr = UG_NOERR

   if (crs%is_spherical) then
      ierr = nf90_put_att(ncid, id_varx, 'units',       'degrees_east')
      ierr = nf90_put_att(ncid, id_vary, 'units',       'degrees_north')
      ierr = nf90_put_att(ncid, id_varx, 'standard_name', 'longitude')
      ierr = nf90_put_att(ncid, id_vary, 'standard_name', 'latitude')
      ierr = nf90_put_att(ncid, id_varx, 'long_name'   , 'longitude')
      ierr = nf90_put_att(ncid, id_vary, 'long_name'   , 'latitude')
   else
      ierr = nf90_put_att(ncid, id_varx, 'units',       'm')
      ierr = nf90_put_att(ncid, id_vary, 'units',       'm')
      ierr = nf90_put_att(ncid, id_varx, 'standard_name', 'projection_x_coordinate')
      ierr = nf90_put_att(ncid, id_vary, 'standard_name', 'projection_y_coordinate')
      ierr = nf90_put_att(ncid, id_varx, 'long_name'   , 'x')
      ierr = nf90_put_att(ncid, id_vary, 'long_name'   , 'y')
   end if
end function ug_addcoordatts

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
      epsg      = 28992
      epsgstring = 'EPSG:28992'
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

!> Write mesh topoplogy
!! This only writes the mesh topology variable, not the other variables that are part of the mesh.
function ug_write_meshtopology(ncid, meshids, meshName, dim, dataLocsCode, addEdgeFaceConnectivity, addFaceEdgeConnectivity, addFaceFaceConnectivity) result(ierr)
   integer,          intent(in) :: ncid         !< NetCDF dataset id
   type(t_ug_meshids), intent(inout) :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
   character(len=*), intent(in) :: meshName     !< Name for the mesh variable, also used as prefix for all related entities.
   integer,          intent(in) :: dim          !< Dimensionality of the mesh (1/2/3)
   integer,          intent(in) :: dataLocsCode !< Specifies at which mesh locations data may be specified.
   logical,          intent(in) :: addEdgeFaceConnectivity !< Specifies whether edge_face_connectivity should be added.
   logical,          intent(in) :: addFaceEdgeConnectivity !< Specifies whether face_edge_connectivity should be added.
   logical,          intent(in) :: addFaceFaceConnectivity !< Specifies whether face_face_connectivity should be added.
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
      if (addFaceEdgeConnectivity) then
         ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'face_edge_connectivity', prefix//'_face_edges')
      end if
      if (addFaceFaceConnectivity) then
         ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'face_face_connectivity', prefix//'_face_links')
      end if
      ! Note that edge_face_connectivity is not officially part of the UGRID conventions, however it is very similar to e.g. face_edge_connectivity, which is part of the UGRID conventions.
      if (addEdgeFaceConnectivity) then
         ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'edge_face_connectivity', prefix//'_edge_faces')
      end if
   end if

   ! Optionally required if data there:
   if (ug_checklocation(dataLocsCode, UG_LOC_FACE)) then
      ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'face_coordinates', prefix//'_face_x '//prefix//'_face_y')
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
function ug_write_mesh_struct(ncid, meshids, meshgeom) result(ierr)
   integer,             intent(in) :: ncid     !< NetCDF dataset id, should be already open and ready for writing.
   type(t_ug_meshids),  intent(inout) :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
   type(t_ug_meshgeom), intent(in) :: meshgeom !< The complete mesh geometry in a single struct.
   integer                         :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.

   ierr = ug_write_mesh_arrays(ncid, meshids, meshgeom%meshName, meshgeom%dim, UG_LOC_ALL2D, meshgeom%numNode, meshgeom%numEdge, meshgeom%numFace, &
                           meshgeom%edge_nodes, meshgeom%face_nodes, meshgeom%edge_faces, meshgeom%face_edges, meshgeom%face_links, meshgeom%nodex, meshgeom%nodey, & ! meshgeom%nodez, &
                           meshgeom%edgex, meshgeom%edgey, meshgeom%facex, meshgeom%facey, meshgeom%crs, -999, -999d0)
end function ug_write_mesh_struct


!> Writes a complete mesh geometry to an open NetCDF data set based on separate arrays with all mesh data..
!! The mesh geometry is the required starting point for all variables/data defined ON that mesh.
!! This function requires all mesh arrays as input, for the derived type-based function, see ug_write_mesh_struct.
function ug_write_mesh_arrays(ncid, meshids, meshName, dim, dataLocs, numNode, numEdge, numFace, &
                              edge_nodes, face_nodes, edge_faces, face_edges, face_links, xn, yn, xe, ye, xf, yf, &
                              crs, imiss, dmiss) result(ierr)
   integer,          intent(in) :: ncid     !< NetCDF dataset id, should be already open and ready for writing.
   type(t_ug_meshids), intent(inout) :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
   character(len=*), intent(in) :: meshName !< Name for the mesh variable, also used as prefix for all related entities.
   integer,          intent(in) :: dim      !< Dimensionality of the mesh (1/2/3)
   integer,          intent(in) :: dataLocs !< Integer code describing on which topological locations data is/will be used.
   integer,          intent(in) :: numNode  !< Number of nodes in the mesh.
   integer,          intent(in) :: numEdge  !< Number of edges in the mesh.
   integer,          intent(in) :: numFace  !< Number of faces in the mesh.
   integer,          intent(in) :: edge_nodes(:,:) !< Edge-to-node mapping array.
   integer,          intent(in) :: face_nodes(:,:) !< Face-to-node mapping array.
   integer, pointer             :: edge_faces(:,:) !< Edge-to-face mapping array (optional, can be null()).
   integer, pointer             :: face_edges(:,:) !< Face-to-edge mapping array (optional, can be null()).
   integer, pointer             :: face_links(:,:) !< Face-to-face mapping array (optional, can be null()).
   real(kind=dp),    intent(in) :: xn(:), yn(:) !< x,y-coordinates of the mesh nodes.
   real(kind=dp),    intent(in) :: xe(:), ye(:) !< representative x,y-coordinates of the mesh edges.
   real(kind=dp),    intent(in) :: xf(:), yf(:) !< representative x,y-coordinates of the mesh faces.
   type(t_crs),      intent(in) :: crs      !< Coordinate reference system for input coordinates
   integer,          intent(in) :: imiss    !< Fill value used for integer values (e.g. in edge/face_nodes arrays).
   real(kind=dp),    intent(in) :: dmiss    !< Fill value used for double precision values (e.g. in face_x_bnd variable).
   integer                      :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.

   integer :: id_twodim

   real(kind=dp), allocatable :: edgexbnd(:,:), edgeybnd(:,:), facexbnd(:,:), faceybnd(:,:)
   integer :: maxnv, k, n
   character(len=len_trim(meshName)) :: prefix
   integer :: wasInDefine

   ierr = UG_SOMEERR
   wasInDefine = 0

   ierr = nf90_redef(ncid)
   if (ierr == nf90_eindefine) wasInDefine = 1 ! Was still in define mode.

   prefix=trim(meshName)

   ierr = ug_write_meshtopology(ncid, meshids, meshName, dim, dataLocs, ASSOCIATED(edge_faces), ASSOCIATED(face_edges), ASSOCIATED(face_links))
   if (ierr /= UG_NOERR) then
      goto 888
   end if


   ! Dimensions
   ierr = nf90_def_dim(ncid, 'n'//prefix//'_node',        numNode,   meshids%id_nodedim)
   ierr = nf90_def_dim(ncid, 'n'//prefix//'_edge',        numEdge,   meshids%id_edgedim)
   ierr = nf90_def_dim(ncid, 'Two',                         2,       id_twodim)! TODO: AvD: duplicates!
   if (dim == 2 .or. ug_checklocation(dataLocs, UG_LOC_FACE)) then
      maxnv = size(face_nodes, 1)
      ierr = nf90_def_dim(ncid, 'n'//prefix//'_face',        numFace,   meshids%id_facedim)
      ierr = nf90_def_dim(ncid, 'nMax'//prefix//'_face_nodes', maxnv,   meshids%id_maxfacenodesdim)
   end if

   ierr = ug_add_coordmapping(ncid, crs)

   ! Nodes
   ierr = nf90_def_var(ncid, prefix//'_node_x', nf90_double, meshids%id_nodedim, meshids%id_nodex)
   ierr = nf90_def_var(ncid, prefix//'_node_y', nf90_double, meshids%id_nodedim, meshids%id_nodey)
   ierr = ug_addcoordatts(ncid, meshids%id_nodex, meshids%id_nodey, crs)
   ierr = nf90_put_att(ncid, meshids%id_nodex, 'long_name',    'x-coordinate of mesh nodes')
   ierr = nf90_put_att(ncid, meshids%id_nodey, 'long_name',    'y-coordinate of mesh nodes')

   ! Add mandatory lon/lat coords too (only if jsferic==0)
   ! TODO: AvD ierr = ug_add_lonlat_vars(inetfile, 'NetNode', '', (/ id_netnodedim /), id_netnodelon, id_netnodelat, jsferic)

   ierr = ug_def_var(ncid, meshids, meshids%id_nodez, (/ meshids%id_nodedim /), nf90_double, UG_LOC_NODE, &
                     meshName, 'node_z', 'altitude', 'z-coordinate of mesh nodes', 'm', '', crs, dfill=dmiss)
   ! ierr = nf90_put_att(ncid, meshids%id_nodez, 'positive',       'up') ! Not allowed for non-coordinate variables.

   ! Edges
   if (dim == 1 .or. ug_checklocation(dataLocs, UG_LOC_EDGE)) then
      ierr = nf90_def_var(ncid, prefix//'_edge_nodes', nf90_int, (/ id_twodim, meshids%id_edgedim /) , meshids%id_edgenodes)
      ierr = nf90_put_att(ncid, meshids%id_edgenodes, 'cf_role',   'edge_node_connectivity')
      ierr = nf90_put_att(ncid, meshids%id_edgenodes, 'long_name',  'Maps every edge to the two nodes that it connects')
      ierr = nf90_put_att(ncid, meshids%id_edgenodes, 'start_index',  1)
      ierr = nf90_put_att(ncid, meshids%id_edgenodes, '_FillValue',  imiss)
   end if
   if (ug_checklocation(dataLocs, UG_LOC_EDGE)) then
      ierr = nf90_def_var(ncid, prefix//'_edge_x', nf90_double, meshids%id_edgedim, meshids%id_edgex)
      ierr = nf90_def_var(ncid, prefix//'_edge_y', nf90_double, meshids%id_edgedim, meshids%id_edgey)
      ierr = ug_addcoordatts(ncid, meshids%id_edgex, meshids%id_edgey, crs)
      ierr = nf90_put_att(ncid, meshids%id_edgex, 'long_name',    'x-coordinate of the midpoint of the mesh edge')
      ierr = nf90_put_att(ncid, meshids%id_edgey, 'long_name',    'y-coordinate of the midpoint of the mesh edge')

      ierr = nf90_put_att(ncid, meshids%id_edgex, 'bounds',    prefix//'_edge_x_bnd')
      ierr = nf90_put_att(ncid, meshids%id_edgey, 'bounds',    prefix//'_edge_y_bnd')

      ierr = nf90_def_var(ncid, prefix//'_edge_x_bnd', nf90_double, (/ id_twodim, meshids%id_edgedim /), meshids%id_edgexbnd)
      ierr = nf90_def_var(ncid, prefix//'_edge_y_bnd', nf90_double, (/ id_twodim, meshids%id_edgedim /), meshids%id_edgeybnd)
      ierr = ug_addcoordatts(ncid, meshids%id_edgexbnd, meshids%id_edgeybnd, crs)
      ierr = nf90_put_att(ncid, meshids%id_edgexbnd, 'long_name',    'x-coordinate bounds of 2D mesh edge (i.e. end point coordinates)')
      ierr = nf90_put_att(ncid, meshids%id_edgeybnd, 'long_name',    'y-coordinate bounds of 2D mesh edge (i.e. end point coordinates)')

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
      ierr = nf90_put_att(ncid, meshids%id_facenodes, 'long_name',  'Maps every face to its corner nodes (counterclockwise)')
      ierr = nf90_put_att(ncid, meshids%id_facenodes, 'start_index',  1)
      ierr = nf90_put_att(ncid, meshids%id_facenodes, '_FillValue',  imiss)

      ! Face edge connectivity.
      if (ASSOCIATED(face_edges)) then
         ierr = nf90_def_var(ncid, prefix//'_face_edges', nf90_int, (/ meshids%id_maxfacenodesdim, meshids%id_facedim /) , meshids%id_faceedges)
         ierr = nf90_put_att(ncid, meshids%id_faceedges, 'cf_role',     'face_edge_connectivity')
         ierr = nf90_put_att(ncid, meshids%id_faceedges, 'long_name',   'Maps every face to its edges (in counterclockwise order)')
         ierr = nf90_put_att(ncid, meshids%id_faceedges, 'start_index', 1)
         ierr = nf90_put_att(ncid, meshids%id_faceedges, '_FillValue',  imiss)
      end if

      ! Face face connectivity.
      if (ASSOCIATED(face_links)) then
         ierr = nf90_def_var(ncid, prefix//'_face_links', nf90_int, (/ meshids%id_maxfacenodesdim, meshids%id_facedim /) , meshids%id_facelinks)
         ierr = nf90_put_att(ncid, meshids%id_facelinks, 'cf_role',     'face_face_connectivity')
         ierr = nf90_put_att(ncid, meshids%id_facelinks, 'long_name',   'Maps every face to its neighboring faces (in counterclockwise order)')
         ierr = nf90_put_att(ncid, meshids%id_facelinks, 'start_index', 1)
         ierr = nf90_put_att(ncid, meshids%id_facelinks, '_FillValue',  imiss)
         !TODO add flag for "out-of-mesh" value. AK
      end if

      ! Edge face connectivity.
      ! Note that edge_face_connectivity is not officially part of the UGRID conventions, however it is very similar to e.g. face_edge_connectivity, which is part of the UGRID conventions.
      if (ASSOCIATED(edge_faces)) then
         ierr = nf90_def_var(ncid, prefix//'_edge_faces', nf90_int, (/ id_twodim, meshids%id_edgedim /) , meshids%id_edgefaces)
         ierr = nf90_put_att(ncid, meshids%id_edgefaces, 'cf_role',     'edge_face_connectivity')
         ierr = nf90_put_att(ncid, meshids%id_edgefaces, 'long_name',   'Maps every edge to the two faces that it separates')
         ierr = nf90_put_att(ncid, meshids%id_edgefaces, 'start_index', 1)
         ierr = nf90_put_att(ncid, meshids%id_edgefaces, '_FillValue',  imiss)
         !TODO add flag for "out-of-mesh" value. AK
      end if
   end if
   if (ug_checklocation(dataLocs, UG_LOC_FACE)) then
      ierr = nf90_def_var(ncid, prefix//'_face_x', nf90_double, meshids%id_facedim, meshids%id_facex)
      ierr = nf90_def_var(ncid, prefix//'_face_y', nf90_double, meshids%id_facedim, meshids%id_facey)
      ierr = ug_addcoordatts(ncid, meshids%id_facex, meshids%id_facey, crs)
      ierr = nf90_put_att(ncid, meshids%id_facex, 'long_name',    'Characteristic x-coordinate of mesh face')
      ierr = nf90_put_att(ncid, meshids%id_facey, 'long_name',    'Characteristic y-coordinate of mesh face')
      ierr = nf90_put_att(ncid, meshids%id_facex, 'bounds',    prefix//'_face_x_bnd')
      ierr = nf90_put_att(ncid, meshids%id_facey, 'bounds',    prefix//'_face_y_bnd')

      ierr = nf90_def_var(ncid, prefix//'_face_x_bnd', nf90_double, (/ meshids%id_maxfacenodesdim, meshids%id_facedim /), meshids%id_facexbnd)
      ierr = nf90_def_var(ncid, prefix//'_face_y_bnd', nf90_double, (/ meshids%id_maxfacenodesdim, meshids%id_facedim /), meshids%id_faceybnd)
      ierr = ug_addcoordatts(ncid, meshids%id_facexbnd, meshids%id_faceybnd, crs)
      ierr = nf90_put_att(ncid, meshids%id_facexbnd, 'long_name',    'x-coordinate bounds of 2D mesh face (i.e. corner coordinates)')
      ierr = nf90_put_att(ncid, meshids%id_faceybnd, 'long_name',    'y-coordinate bounds of 2D mesh face (i.e. corner coordinates)')
      ierr = nf90_put_att(ncid, meshids%id_facexbnd, '_FillValue',  dmiss)
      ierr = nf90_put_att(ncid, meshids%id_faceybnd, '_FillValue',  dmiss)

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
   wasindefine = 0
   
! -- end of header --
   
   ! Write the actual data
   ! Nodes:
   ierr = nf90_put_var(ncid, meshids%id_nodex,    xn(1:numNode))
   ierr = nf90_put_var(ncid, meshids%id_nodey,    yn(1:numNode))
!   ierr = nf90_put_var(ncid, meshids%id_nodez,    zn(1:numNode))

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

   end if

   ! Faces:
   if (dim == 2 .or. ug_checklocation(dataLocs, UG_LOC_FACE)) then
      ! Write mesh faces (2D cells)
      ierr = nf90_put_var(ncid, meshids%id_facenodes, face_nodes)

      ! Face edge connectivity:
      if (ASSOCIATED(face_edges)) then
         ierr = nf90_put_var(ncid, meshids%id_faceedges, face_edges, count=(/ maxnv, numFace /))
      end if
      ! Face face connectivity:
      if (ASSOCIATED(face_links)) then
         ierr = nf90_put_var(ncid, meshids%id_facelinks, face_links, count=(/ maxnv, numFace /))
      end if
      ! Edge face connectivity:
      if (ASSOCIATED(edge_faces)) then
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
   end if
   
   if (ug_checklocation(dataLocs, UG_LOC_FACE)) then
      ierr = nf90_put_var(ncid, meshids%id_facex,    xf(1:numFace))
      ierr = nf90_put_var(ncid, meshids%id_facey,    yf(1:numFace))
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
   type(t_ug_file), intent(  out) :: ug_file !< UGRID file struct with cached meta information.
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

   meshids%id_meshtopo        = varid              !< Top-level variable ID for mesh topology, collects all related variable names via attributes.

   !
   ! Dimensions:
   !
   call att_to_dimid('node_dimension', meshids%id_nodedim)
   call att_to_dimid('edge_dimension', meshids%id_edgedim)
   call att_to_dimid('face_dimension', meshids%id_facedim)
   
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


!> Gets the size/count of items for the specified topological location.
!! Use this to get the number of nodes/edges/faces/volumes.
function ug_inquire_dimension(ncid, meshids, idimtype, len) result(ierr)
   integer,            intent(in)    :: ncid     !< NetCDF dataset id, should be already open and ready for writing.
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


!> Gets the x,y-coordinates for all nodes in the specified mesh.
!! The output x,y arrays are supposed to be of exact correct length already.
function ug_get_node_coordinates(ncid, meshids, xn, yn) result(ierr)
   integer,            intent(in)  :: ncid    !< NetCDF dataset id, should be already open and ready for writing.
   type(t_ug_meshids), intent(in)  :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
   real(kind=dp),      intent(out) :: xn(:), yn(:) !< Arrays to store x,y-coordinates of the mesh nodes.
   integer                         :: ierr     !< Result status (UG_NOERR==NF90_NOERR if successful).

   ierr = nf90_get_var(ncid, meshids%id_nodex, xn)
   ierr = nf90_get_var(ncid, meshids%id_nodex, yn)
   ! TODO: AvD: some more careful error handling

end function ug_get_node_coordinates


!> Gets the face-node connectvit table for all faces in the specified mesh.
!! The output face_nodes array is supposed to be of exact correct size already.
function ug_get_face_nodes(ncid, meshids, face_nodes) result(ierr)
   integer,            intent(in)  :: ncid    !< NetCDF dataset id, should be already open and ready for writing.
   type(t_ug_meshids), intent(in)  :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
   integer,            intent(out) :: face_nodes(:,:) !< Array to the face-node connectivity table.
   integer                         :: ierr     !< Result status (UG_NOERR==NF90_NOERRif successful).

   ierr = nf90_get_var(ncid, meshids%id_facenodes, face_nodes)
   ! TODO: AvD: some more careful error handling
   
   ! TODO: AvD: also introduce 0-/1-based indexing handling.

end function ug_get_face_nodes

    end module io_ugrid
