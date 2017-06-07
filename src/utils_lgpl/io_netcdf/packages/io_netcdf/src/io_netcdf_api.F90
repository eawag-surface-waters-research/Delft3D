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
!! Basic API for io_netcdf library routines.
!! NOTE: most functionality is in underlying static lib for io_netcdf.
!! TODO: AvD: Check whether DLLEXPORTs from static lib actually end up in DLL.

! NOTE: this module contains mainly wrapper functions around their
! counterpart functions in the static library io_netcdf, and exposes
! the wrappers in the DLL API.
! This is necessary to allow that the static functions can still also
! be called natively when applications use the statically linked library
! (which is based on compiler-dependent mangled names).


!> The io_netcdf dynamic library API functions.
!! Mainly consists of wrappers around underlying static libraries.
module io_netcdf_api
use io_netcdf
use iso_c_binding

implicit none

!-------------------------------------------------------------------------------
contains
!-------------------------------------------------------------------------------


!> Tries to create a NetCDF file and initialize based on its specified conventions.
function ionc_create_dll(c_path, mode, ioncid) result(ierr) bind(C, name="ionc_create")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_create_dll
    character(kind=c_char), intent(in) :: c_path(MAXSTRLEN)      !< File name for netCDF dataset to be opened.
    integer(kind=c_int),    intent(in) :: mode      !< NetCDF open mode, e.g. NF90_NOWRITE.
    integer(kind=c_int),    intent(out):: ioncid   !< inout The io_netcdf dataset id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid.
    integer(kind=c_int)                :: ierr      !< Result status (IONC_NOERR if successful).

    character(len=MAXSTRLEN) :: path

    ! Store the name
    path = char_array_to_string(c_path, strlen(c_path))

    ! We always create ugrid files
    ierr = ionc_create(path, mode, ioncid, IONC_CONV_UGRID)
end function ionc_create_dll


!> Inquire the NetCDF conventions used in the dataset.
function ionc_inq_conventions_dll(ioncid, iconvtype, convversion) result(ierr) bind(C, name="ionc_inq_conventions")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_inq_conventions_dll
   integer(kind=c_int), intent(in)  :: ioncid    !< The IONC data set id.
   integer(kind=c_int), intent(out) :: iconvtype !< The NetCDF conventions type of the dataset.
   real(kind=c_double), intent(out) :: convversion !< The NetCDF conventions version of the dataset.
   integer(kind=c_int)              :: ierr      !< Result status, ionc_noerr if successful.

   ierr = ionc_inq_conventions(ioncid, iconvtype, convversion)
end function ionc_inq_conventions_dll


!> Checks whether the specified data set adheres to a specific set of conventions.
!! Datasets may adhere to multiple conventions at the same time, so use this method
!! to check for individual conventions.
function ionc_adheresto_conventions_dll(ioncid, iconvtype) result(does_adhere) bind(C, name="ionc_adheresto_conventions")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_adheresto_conventions_dll
   integer(kind=c_int), intent(in)  :: ioncid      !< The IONC data set id.
   integer(kind=c_int), intent(in)  :: iconvtype   !< The NetCDF conventions type to check for.
   logical(kind=c_bool)             :: does_adhere !< Whether or not the file adheres to the specified conventions.

   does_adhere = ionc_adheresto_conventions(ioncid, iconvtype)
end function ionc_adheresto_conventions_dll


!> Tries to open a NetCDF file and initialize based on its specified conventions.
function ionc_open_dll(c_path, mode, ioncid, iconvtype, convversion) result(ierr) bind(C, name="ionc_open")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_open_dll
   use iso_c_binding
   character(kind=c_char), intent(in   ) :: c_path(MAXSTRLEN)      !< File name for netCDF dataset to be opened.
   integer(kind=c_int),           intent(in   ) :: mode      !< NetCDF open mode, e.g. NF90_NOWRITE.
   integer(kind=c_int),           intent(  out) :: ioncid    !< The io_netcdf dataset id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid.
   integer(kind=c_int),           intent(inout) :: iconvtype !< The detected conventions in the file.
   real(kind=c_double),           intent(inout) :: convversion !< The detected conventions in the file.
!   integer(kind=c_int), optional, intent(inout) :: chunksize !< (optional) NetCDF chunksize parameter.
   integer(kind=c_int)                          :: ierr      !< Result status (IONC_NOERR if successful).

   character(len=MAXSTRLEN) :: path
   !character(len=strlen(c_path)) :: nc_file
  
   ! Store the name
   path = char_array_to_string(c_path, strlen(c_path)) 

   ierr = ionc_open(path, mode, ioncid, iconvtype, convversion)
end function ionc_open_dll


!> Tries to close an open io_netcdf data set.
function ionc_close_dll(ioncid) result(ierr) bind(C, name="ionc_close")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_close_dll
   integer(kind=c_int),           intent(in   ) :: ioncid    !< The io_netcdf dataset id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid.
   integer(kind=c_int)                          :: ierr      !< Result status (IONC_NOERR if successful).

   ierr = ionc_close(ioncid)
end function ionc_close_dll


!> Gets the number of mesh from a data set.
function ionc_get_mesh_count_dll(ioncid, nmesh) result(ierr) bind(C, name="ionc_get_mesh_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_mesh_count_dll
   integer(kind=c_int),             intent(in)    :: ioncid  !< The IONC data set id.
   integer(kind=c_int),             intent(  out) :: nmesh   !< Number of meshes.
   integer(kind=c_int)                            :: ierr    !< Result status, ionc_noerr if successful.

   ierr = ionc_get_mesh_count(ioncid, nmesh)
end function ionc_get_mesh_count_dll

!> Gets the name of the mesh topology variable in an open dataset.
function ionc_get_mesh_name_dll(ioncid, meshid, c_meshname) result(ierr) bind(C, name="ionc_get_mesh_name")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_mesh_name_dll
   use iso_c_binding
   integer,                      intent(in)    :: ioncid   !< The IONC data set id.
   integer,                      intent(in)    :: meshid   !< The mesh id in the specified data set.
   character(kind=c_char,len=1), intent(  out) :: c_meshname(MAXSTRLEN) !< The name of the mesh topology variable.

   character(len=MAXSTRLEN) :: meshname !< The name of the mesh geometry.
   integer                            :: ierr     !< Result status, ionc_noerr if successful.   
   
   
   meshname = ''
   ierr = ionc_get_mesh_name(ioncid, meshid, meshname)
   c_meshname= string_to_char_array(meshname,len_trim(meshname))
   
end function ionc_get_mesh_name_dll

!> Gets the number of nodes in a single mesh from a data set.
function ionc_get_node_count_dll(ioncid, meshid, nnode) result(ierr) bind(C, name="ionc_get_node_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_node_count_dll
   integer(kind=c_int),             intent(in)    :: ioncid  !< The IONC data set id.
   integer(kind=c_int),             intent(in)    :: meshid  !< The mesh id in the specified data set.
   integer(kind=c_int),             intent(  out) :: nnode   !< Number of nodes.
   integer(kind=c_int)                            :: ierr    !< Result status, ionc_noerr if successful.

   ierr = ionc_get_node_count(ioncid, meshid, nnode)
end function ionc_get_node_count_dll


!> Gets the number of edges in a single mesh from a data set.
function ionc_get_edge_count_dll(ioncid, meshid, nedge) result(ierr) bind(C, name="ionc_get_edge_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_edge_count_dll
   integer(kind=c_int),             intent(in)    :: ioncid  !< The IONC data set id.
   integer(kind=c_int),             intent(in)    :: meshid  !< The mesh id in the specified data set.
   integer(kind=c_int),             intent(  out) :: nedge   !< Number of edges.
   integer(kind=c_int)                            :: ierr    !< Result status, ionc_noerr if successful.

   ierr = ionc_get_edge_count(ioncid, meshid, nedge)
end function ionc_get_edge_count_dll


!> Gets the number of faces in a single mesh from a data set.
function ionc_get_face_count_dll(ioncid, meshid, nface) result(ierr) bind(C, name="ionc_get_face_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_face_count_dll
   integer(kind=c_int),             intent(in)    :: ioncid  !< The IONC data set id.
   integer(kind=c_int),             intent(in)    :: meshid  !< The mesh id in the specified data set.
   integer(kind=c_int),             intent(  out) :: nface   !< Number of faces.
   integer(kind=c_int)                            :: ierr    !< Result status, ionc_noerr if successful.

   ierr = ionc_get_face_count(ioncid, meshid, nface)
end function ionc_get_face_count_dll


!> Gets the maximum number of nodes for any face in a single mesh from a data set.
function ionc_get_max_face_nodes_dll(ioncid, meshid, nmaxfacenodes) result(ierr) bind(C, name="ionc_get_max_face_nodes")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_max_face_nodes_dll
   integer(kind=c_int),             intent(in)    :: ioncid        !< The IONC data set id.
   integer(kind=c_int),             intent(in)    :: meshid        !< The mesh id in the specified data set.
   integer(kind=c_int),             intent(  out) :: nmaxfacenodes !< The maximum number of nodes per face in the mesh.
   integer(kind=c_int)                            :: ierr          !< Result status, ionc_noerr if successful.

   ierr = ionc_get_max_face_nodes(ioncid, meshid, nmaxfacenodes)
end function ionc_get_max_face_nodes_dll


!> Gets the x,y coordinates for all nodes in a single mesh from a data set.
function ionc_get_node_coordinates_dll(ioncid, meshid, c_xptr, c_yptr, nnode) result(ierr) bind(C, name="ionc_get_node_coordinates")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_node_coordinates_dll
   integer(kind=c_int), intent(in)  :: ioncid !< The IONC data set id.
   integer(kind=c_int), intent(in)  :: meshid !< The mesh id in the specified data set.
   type(c_ptr),         intent(out) :: c_xptr !< Pointer to array for x-coordinates
   type(c_ptr),         intent(out) :: c_yptr !< Pointer to array for y-coordinates
   integer(kind=c_int), intent(in)  :: nnode  !< The number of nodes in the mesh. TODO: AvD: remove this somehow, now only required to call c_f_pointer
   integer(kind=c_int)              :: ierr   !< Result status, ionc_noerr if successful.

   double precision, pointer :: xptr(:), yptr(:)! 

   call c_f_pointer(c_xptr, xptr, (/ nnode /))
   call c_f_pointer(c_yptr, yptr, (/ nnode /))
   
   ierr = ionc_get_node_coordinates(ioncid, meshid, xptr, yptr)
end function ionc_get_node_coordinates_dll


!> Puts the x,y coordinates for all nodes in a single mesh from a data set.
function ionc_put_node_coordinates_dll(ioncid, meshid, c_xptr, c_yptr, nnode) result(ierr) bind(C, name="ionc_put_node_coordinates")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_put_node_coordinates_dll
   integer(kind=c_int), intent(in)  :: ioncid !< The IONC data set id.
   integer(kind=c_int), intent(in)  :: meshid !< The mesh id in the specified data set.
   type(c_ptr),         intent(out) :: c_xptr !< Pointer to array containing x-coordinates
   type(c_ptr),         intent(out) :: c_yptr !< Pointer to array containing y-coordinates
   integer(kind=c_int), intent(in)  :: nnode  !< The number of nodes in the mesh. TODO: AvD: remove this somehow, now only required to call c_f_pointer
   integer(kind=c_int)              :: ierr   !< Result status, ionc_noerr if successful.

   double precision, pointer :: xptr(:), yptr(:)! 

   call c_f_pointer(c_xptr, xptr, (/ nnode /))
   call c_f_pointer(c_yptr, yptr, (/ nnode /))
   
   ierr = ionc_put_node_coordinates(ioncid, meshid, xptr, yptr)
end function ionc_put_node_coordinates_dll


!> Gets the edge-node connectivity table for all edges in the specified mesh.
function ionc_get_edge_nodes_dll(ioncid, meshid, c_edge_nodes_ptr, nedge) result(ierr) bind(C, name="ionc_get_edge_nodes")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_edge_nodes_dll
   integer(kind=c_int), intent(in)    :: ioncid  !< The IONC data set id.
   integer(kind=c_int), intent(in)    :: meshid  !< The mesh id in the specified data set.
   type(c_ptr),         intent(  out) :: c_edge_nodes_ptr !< Pointer to array for the edge-node connectivity table.
   integer(kind=c_int), intent(in)    :: nedge  !< The number of edges in the mesh.    
   integer(kind=c_int)                :: ierr    !< Result status, ionc_noerr if successful.

   integer, pointer :: edge_nodes(:,:)

   call c_f_pointer(c_edge_nodes_ptr, edge_nodes, (/ 2 , nedge /))
   
   ierr = ionc_get_edge_nodes(ioncid, meshid, edge_nodes)
end function ionc_get_edge_nodes_dll

!> Gets the face-node connectvit table for all faces in the specified mesh.
!! The output face_nodes array is supposed to be of exact correct size already.
function ionc_get_face_nodes_dll(ioncid, meshid, c_face_nodes_ptr, nface, nmaxfacenodes, fillvalue) result(ierr) bind(C, name="ionc_get_face_nodes")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_face_nodes_dll
   integer(kind=c_int), intent(in)    :: ioncid  !< The IONC data set id.
   integer(kind=c_int), intent(in)    :: meshid  !< The mesh id in the specified data set.
   type(c_ptr),         intent(  out) :: c_face_nodes_ptr !< Pointer to array for the face-node connectivity table.
   integer(kind=c_int), intent(in)    :: nface  !< The number of faces in the mesh. TODO: AvD: remove this somehow, now only required to call c_f_pointer
   integer(kind=c_int), intent(in)    :: nmaxfacenodes  !< The maximum number of nodes per face in the mesh. TODO: AvD: remove this somehow, now only required to call c_f_pointer
   integer(kind=c_int)                :: fillvalue    !< Scalar for getting the fill value parameter for the requested variable.
   integer(kind=c_int)                :: ierr    !< Result status, ionc_noerr if successful.

   integer, pointer :: face_nodes(:,:)

   call c_f_pointer(c_face_nodes_ptr, face_nodes, (/ nmaxfacenodes, nface /))
   
   ierr = ionc_get_face_nodes(ioncid, meshid, face_nodes, fillvalue)
end function ionc_get_face_nodes_dll


!> Gets the epsg code of coordinate system from a data set.
function ionc_get_coordinate_system_dll(ioncid, epsg) result(ierr) bind(C, name="ionc_get_coordinate_system")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_coordinate_system_dll
   integer(kind=c_int),             intent(in)    :: ioncid  !< The IONC data set id.
   integer(kind=c_int),             intent(  out) :: epsg    !< Number of epsg code.
   integer(kind=c_int)                            :: ierr    !< Result status, ionc_noerr if successful.
     
   ierr = ionc_get_coordinate_system(ioncid, epsg)
end function ionc_get_coordinate_system_dll


!> Writes a complete mesh geometry
function ionc_write_geom_ugrid_dll(filename) result(ierr) bind(C, name="ionc_write_geom_ugrid")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_write_geom_ugrid_dll
   character(kind=c_char), intent(in   ) :: filename(MAXSTRLEN)      !< File name for netCDF dataset to be created.
   integer(kind=c_int)                   :: ierr    !< Result status, ionc_noerr if successful.
   character(len=MAXSTRLEN)              :: file
  
   ! Store the name
   file = char_array_to_string(filename, strlen(filename))
   ierr = ionc_write_geom_ugrid(file)
end function ionc_write_geom_ugrid_dll


!> Writes a complete map file
function ionc_write_map_ugrid_dll(filename) result(ierr) bind(C, name="ionc_write_map_ugrid")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_write_map_ugrid_dll
   character(kind=c_char), intent(in   ) :: filename(MAXSTRLEN)      !< File name for netCDF dataset to be created.
   integer(kind=c_int)                   :: ierr    !< Result status, ionc_noerr if successful.
   character(len=MAXSTRLEN)              :: file
  
   ! Store the name
   file = char_array_to_string(filename, strlen(filename))
   ierr = ionc_write_map_ugrid(file)
end function ionc_write_map_ugrid_dll

!> Initializes the io_netcdf library, setting up the logger.
function ionc_initialize_dll(c_msg_callback, c_prgs_callback) result(ierr) bind(C, name="ionc_initialize")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_initialize_dll
   type(c_funptr), value    :: c_msg_callback   !< Set a callback that will be called with new messages
   type(c_funptr), value    :: c_prgs_callback  !< Set a callback that will be called with new messages for progress
   integer                  :: ierr             !< Result status, ionc_noerr if successful.
   
   ierr = ionc_initialize(c_msg_callback, c_prgs_callback)   
end function ionc_initialize_dll


!> Returns the number of variables that are available in the specified dataset on the specified mesh.
!! The location type allows to select on specific topological mesh locations
!! (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
function ionc_get_var_count_dll(ioncid, meshid, iloctype, nvar) result(ierr) bind(C, name="ionc_get_var_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_var_count_dll
   integer(kind=c_int),             intent(in)    :: ioncid   !< The IONC data set id.
   integer(kind=c_int),             intent(in)    :: meshid   !< The mesh id in the specified data set.
   integer(kind=c_int),             intent(in)    :: iloctype !< The topological location on which to select data (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
   integer(kind=c_int),             intent(  out) :: nvar     !< Number of variables defined on the requested location type+mesh+dataset.
   integer(kind=c_int)                            :: ierr     !< Result status, ionc_noerr if successful.

   ierr = ionc_get_var_count(ioncid, meshid, iloctype, nvar)

end function ionc_get_var_count_dll


!> Gets a list of variable IDs that are available in the specified dataset on the specified mesh.
!! The location type allows to select on specific topological mesh locations
!! (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D)
function ionc_inq_varids_dll(ioncid, meshid, iloctype, c_varids_ptr, nmaxvar) result(ierr) bind(C, name="ionc_inq_varids")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_inq_varids_dll
   integer(kind=c_int),             intent(in)    :: ioncid   !< The IONC data set id.
   integer(kind=c_int),             intent(in)    :: meshid   !< The mesh id in the specified data set.
   integer(kind=c_int),             intent(in)    :: iloctype !< The topological location on which to select data (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
   type(c_ptr),                     intent(  out) :: c_varids_ptr !< Pointer to array for the variable ids.
   integer(kind=c_int),             intent(in)    :: nmaxvar  !< The number of variables in the target array. TODO: AvD: remove this somehow, now only required to call c_f_pointer
   integer(kind=c_int)                            :: ierr    !< Result status, ionc_noerr if successful.

   integer, pointer :: varids(:)
   integer :: nvar

   call c_f_pointer(c_varids_ptr, varids, (/ nmaxvar /))

   ! TODO: AvD: extend the interface of this DLL routine, such that it ALSO RETURNS the nvar number of variables found.
   !            Now only the static library does this.
   ierr = ionc_inq_varids(ioncid, meshid, iloctype, varids, nvar)

end function ionc_inq_varids_dll


!> Gets the variable ID for a data variable that is defined in the specified dataset on the specified mesh.
!! The variable is searched based on variable name (without any "meshnd_" prefix), and which :mesh it is defined on.
function ionc_inq_varid_dll(ioncid, meshid, c_varname, varid) result(ierr) bind(C, name="ionc_inq_varid")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_inq_varid_dll
   integer(kind=c_int),             intent(in)    :: ioncid   !< The IONC data set id.
   integer(kind=c_int),             intent(in)    :: meshid   !< The mesh id in the specified data set.
   character(kind=c_char),          intent(in)    :: c_varname(MAXSTRLEN)  !< The name of the variable to be found. Should be without any "meshnd_" prefix.
   integer(kind=c_int),             intent(  out) :: varid    !< The resulting variable id, if found.
   integer(kind=c_int)                            :: ierr     !< Result status, ionc_noerr if successful.

   character(len=MAXSTRLEN) :: varname
   ! Store the name
   varname = char_array_to_string(c_varname, strlen(c_varname))
   
   ! TODO: AvD: some error handling if ioncid or meshid is wrong
   ierr = ionc_inq_varid(ioncid, meshid, varname, varid)

end function ionc_inq_varid_dll


!> Gets the variable ID for the variable in the specified dataset on the specified mesh,
!! that also has the specified value for its ':standard_name' attribute, and 
!! is defined on the specified topological mesh location (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D)
function ionc_inq_varid_by_standard_name_dll(ioncid, meshid, iloctype, c_stdname, varid) result(ierr) bind(C, name="ionc_inq_varid_by_standard_name")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_inq_varid_by_standard_name_dll
   integer(kind=c_int),             intent(in)    :: ioncid   !< The IONC data set id.
   integer(kind=c_int),             intent(in)    :: meshid   !< The mesh id in the specified data set.
   integer(kind=c_int),             intent(in)    :: iloctype !< The topological location on which to select data (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
   character(kind=c_char),          intent(in)    :: c_stdname(MAXSTRLEN)  !< The standard_name value that is searched for.
   integer(kind=c_int),             intent(  out) :: varid    !< The resulting variable id, if found.
   integer(kind=c_int)                            :: ierr     !< Result status, ionc_noerr if successful.

   character(len=MAXSTRLEN) :: stdname
   ! Store the name
   stdname = char_array_to_string(c_stdname, strlen(c_stdname))

   ! TODO: AvD: some error handling if ioncid or meshid is wrong
   ierr = ionc_inq_varid_by_standard_name(ioncid, meshid, iloctype, stdname, varid)

end function ionc_inq_varid_by_standard_name_dll


!> Gets the values for a named variable in the specified dataset on the specified mesh.
!! The location type allows to select the specific topological mesh location.
!! (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D)
function ionc_get_var_1D_EightByteReal_dll(ioncid, meshid, iloctype, c_varname, c_values_ptr, nval, c_fillvalue) result(ierr) bind(C, name="ionc_get_var")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_var_1D_EightByteReal_dll
   integer,                intent(in)    :: ioncid    !< The IONC data set id.
   integer,                intent(in)    :: meshid    !< The mesh id in the specified data set.
   integer,                intent(in)    :: iloctype  !< The topological location on which to select data (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
   character(kind=c_char), intent(in)    :: c_varname(MAXSTRLEN)   !< The name of the variable to be found. Should be without any "meshnd_" prefix.
   type(c_ptr),            intent(  out) :: c_values_ptr !< Pointer to array for the values.
   integer,                intent(in)    :: nval      !< The number of values in the target array. TODO: AvD: remove this somehow, now only required to call c_f_pointer
   real(c_double),         intent(out)   :: c_fillvalue  !< Scalar for getting the fill value parameter for the requested variable.
   integer                               :: ierr      !< Result status, ionc_noerr if successful.

   character(len=MAXSTRLEN) :: varname
   real(kind=kind(1d0)), pointer :: values(:)

   ! Store the name
   varname = char_array_to_string(c_varname, strlen(c_varname))

   call c_f_pointer(c_values_ptr, values, (/ nval /))

   ierr = ionc_get_var_1D_EightByteReal(ioncid, meshid, iloctype, varname, values, c_fillvalue)

end function ionc_get_var_1D_EightByteReal_dll


!> Puts the values for a named variable into the specified dataset on the specified mesh.
!! NOTE: Assumes that the variable already exists in the file (i.e., needs no def_var anymore).
!! The location type allows to select the specific topological mesh location.
!! (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D)
function ionc_put_var_1D_EightByteReal_dll(ioncid, meshid, iloctype, c_varname, c_values_ptr, nval) result(ierr) bind(C, name="ionc_put_var")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_put_var_1D_EightByteReal_dll
   integer,                intent(in)    :: ioncid    !< The IONC data set id.
   integer,                intent(in)    :: meshid    !< The mesh id in the specified data set.
   integer,                intent(in)    :: iloctype  !< The topological location on which to select data (UGRID-compliant, so UG_LOC_FACE/EDGE/NODE/ALL2D).
   character(kind=c_char), intent(in)    :: c_varname(MAXSTRLEN)   !< The name of the variable to be found. Should be without any "meshnd_" prefix.
   type(c_ptr),            intent(in)    :: c_values_ptr !< Pointer to array for the values.
   integer,                intent(in)    :: nval      !< The number of values in the target array. TODO: AvD: remove this somehow, now only required to call c_f_pointer
   integer                               :: ierr      !< Result status, ionc_noerr if successful.

   character(len=MAXSTRLEN) :: varname
   real(kind=kind(1d0)), pointer :: values(:)

   ! Store the name
   varname = char_array_to_string(c_varname, strlen(c_varname))

   call c_f_pointer(c_values_ptr, values, (/ nval /))

   ierr = ionc_put_var_1D_EightByteReal(ioncid, meshid, iloctype, varname, values)

end function ionc_put_var_1D_EightByteReal_dll


! TODO ******* DERIVED TYPE GIVEN BY C/C++/C#-PROGRAM
!> Add the global attributes to a NetCDF file 
function ionc_add_global_attributes_dll(ioncid, meta) result(ierr)  bind(C, name="ionc_add_global_attributes")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_add_global_attributes_dll
   integer(kind=c_int),    intent(in)    :: ioncid      !< The IONC data set id.
   type (t_ug_meta),       intent(in)    :: meta
   integer(kind=c_int)                   :: ierr        !< Result status, ionc_noerr if successful.! 
   character(len=ug_strLenMeta)          :: institution, source, references, version, modelname !< variables to be passed to io_netcdf for construction of the metadata structure 
   
   institution = meta%institution
   source      = meta%source
   references  = meta%references
   version     = meta%version
   modelname   = meta%modelname

   ierr = ionc_add_global_attributes(ioncid, institution,source,references,version,modelname)
   
end function ionc_add_global_attributes_dll


! TODO ******* DERIVED TYPE GIVEN BY C/C++/C#-PROGRAM
!> Writes the complete mesh geometry
!function ionc_write_mesh_struct_dll(ioncid, meshids, meshgeom) result(ierr) bind(C, name="ionc_write_mesh_struct_dll")
!!DEC$ ATTRIBUTES DLLEXPORT :: ionc_write_mesh_struct_dll
!   integer(kind=c_int),             intent(in)    :: ioncid  !< The IONC data set id.
!   type(t_ug_meshids),  intent(inout) :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
!   type(t_ug_meshgeom), intent(in)    :: meshgeom !< The complete mesh geometry in a single struct.
!   integer(kind=c_int)                :: ierr    !< Result status, ionc_noerr if successful.!
!
!   ierr = ionc_write_mesh_struct(ioncid, meshids, meshgeom)
!end function ionc_write_mesh_struct_dll


!
! private routines/functions
!

! Utility functions, move these to interop module
! Make functions pure so they can be used as input arguments.
integer(c_int) pure function strlen(char_array)
 character(c_char), intent(in) :: char_array(MAXSTRLEN)
 integer :: inull, i
 strlen = 0
 do i = 1, size(char_array)
    if (char_array(i) .eq. C_NULL_CHAR) then
       strlen = i-1
       exit
    end if
 end do
end function strlen

pure function char_array_to_string(char_array, length)
 integer(c_int), intent(in) :: length
 character(c_char),intent(in) :: char_array(length)
 character(len=length) :: char_array_to_string
 integer :: i
 do i = 1, length
    char_array_to_string(i:i) = char_array(i)
 enddo
end function char_array_to_string

function string_to_char_array(string, length) result(char_array)
   integer, intent(in) :: length
   character(len=length), intent(in) :: string
   character(kind=c_char,len=1) :: char_array(MAXSTRLEN)
   integer :: i
   do i = 1, len(string)
       char_array(i) = string(i:i)
   enddo
   char_array(len(string)+1) = C_NULL_CHAR
end function string_to_char_array

!
! Network and mesh1d functions
!

function ionc_create_1d_network_dll(ioncid, networkid, c_networkName, nNodes, nBranches, nGeometry) result(ierr) bind(C, name="ionc_create_1d_network")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_create_1d_network_dll
   integer(kind=c_int),    intent(in) :: ioncid  
   character(kind=c_char), intent(in) :: c_networkName(MAXSTRLEN)
   integer(kind=c_int),    intent(in) :: nNodes,nBranches,nGeometry
   integer(kind=c_int), intent(inout) :: networkid     
   integer                            :: ierr
   character(len=MAXSTRLEN)           :: networkName

   ! Store the name
   networkName = char_array_to_string(c_networkName, strlen(c_networkName))   
   ierr = ionc_create_1d_network_ugrid(ioncid, networkid, networkName, nNodes, nBranches, nGeometry)
   
end function ionc_create_1d_network_dll

function ionc_write_1d_network_nodes_dll(ioncid,networkid, c_nodesX, c_nodesY, nodeinfo, nNodes) result(ierr) bind(C, name="ionc_write_1d_network_nodes")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_write_1d_network_nodes_dll
   
   integer(kind=c_int),     intent(in)    :: ioncid,networkid, nNodes
   type(c_ptr),             intent(in)    :: c_nodesX, c_nodesY 
   type(t_ug_charinfo),  intent(in)       :: nodeinfo(nNodes)
   double precision, pointer              :: nodesX(:), nodesY(:)
   integer                                :: ierr, i 
   character(len=ug_idsLen)               :: nodeids(nNodes)
   character(len=ug_idsLongNamesLen)      :: nodeLongnames(nNodes)
   
   call c_f_pointer(c_nodesX, nodesX, (/ nNodes /))
   call c_f_pointer(c_nodesY, nodesY, (/ nNodes /))
   do i=1,nNodes
       nodeids(i)       = nodeinfo(i)%ids
       nodeLongnames(i) = nodeinfo(i)%longnames
   end do
   
   ierr = ionc_write_1d_network_nodes_ugrid(ioncid, networkId, nodesX, nodesY, nodeids, nodeLongnames)
      
end function ionc_write_1d_network_nodes_dll

function ionc_write_1d_network_branches_dll(ioncid,networkid, c_sourcenodeid, c_targetnodeid, branchinfo, c_branchlengths, c_nbranchgeometrypoints, nBranches) result(ierr) bind(C, name="ionc_write_1d_network_branches")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_write_1d_network_branches_dll
  
  integer(kind=c_int), intent(in)    :: ioncid, networkid 
  type(c_ptr),intent(in)             :: c_sourcenodeid, c_targetnodeid,c_nbranchgeometrypoints,c_branchlengths
  type(t_ug_charinfo),  intent(in)   :: branchinfo(nBranches)
  integer(kind=c_int), intent(in)    :: nBranches
  integer, pointer                   :: sourcenodeid(:), targetnodeid(:),nbranchgeometrypoints(:)
  double precision, pointer          :: branchlengths(:)
  character(len=ug_idsLen)           :: branchids(nBranches)
  character(len=ug_idsLongNamesLen)  :: branchlongnames(nBranches)
  integer ::ierr,i
  
  call c_f_pointer(c_sourcenodeid, sourcenodeid, (/ nBranches /))
  call c_f_pointer(c_targetnodeid, targetnodeid, (/ nBranches /))
  call c_f_pointer(c_nbranchgeometrypoints, nbranchgeometrypoints, (/ nBranches /))
  call c_f_pointer(c_branchlengths, branchlengths, (/ nBranches /))

  do i=1,nBranches
       branchids(i)       = branchinfo(i)%ids
       branchlongnames(i) = branchinfo(i)%longnames
   end do

  ierr = ionc_write_1d_network_branches_ugrid(ioncid, networkid, sourcenodeid, targetnodeid, branchids, branchlengths, branchlongnames, nbranchgeometrypoints, nBranches)
  
end function ionc_write_1d_network_branches_dll

function ionc_write_1d_network_branches_geometry_dll(ioncid, networkid, c_geopointsX, c_geopointsY, nGeometry) result(ierr) bind(C, name="ionc_write_1d_network_branches_geometry")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_write_1d_network_branches_geometry_dll
  
  integer(kind=c_int), intent(in)    :: ioncid, networkid  
  type(c_ptr), intent(in)            :: c_geopointsX, c_geopointsY  
  double precision, pointer          :: geopointsX(:), geopointsY(:)
  integer(kind=c_int), intent(in)    :: nGeometry
  integer ::ierr
  
  call c_f_pointer(c_geopointsX, geopointsX, (/ nGeometry /))
  call c_f_pointer(c_geopointsY, geopointsY, (/ nGeometry /))
    
  ierr = ionc_write_1d_network_branches_geometry_ugrid(ioncid, networkid, geopointsX, geopointsY)
  
end function ionc_write_1d_network_branches_geometry_dll

    
function ionc_get_1d_network_nodes_count_dll(ioncid, networkid, nNodes) result(ierr) bind(C, name="ionc_get_1d_network_nodes_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_network_nodes_count_dll
  
  integer(kind=c_int), intent(in)    :: ioncid, networkid 
  integer(kind=c_int), intent(out)   :: nNodes
  integer                            :: ierr
  
  ierr = ionc_get_1d_network_nodes_count_ugrid(ioncid, networkid, nNodes)
  
end function ionc_get_1d_network_nodes_count_dll
    
function ionc_get_1d_network_branches_count_dll(ioncid, networkid, nBranches) result(ierr) bind(C, name="ionc_get_1d_network_branches_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_network_branches_count_dll
  
  integer(kind=c_int), intent(in)    :: ioncid, networkid 
  integer(kind=c_int), intent(out)   :: nBranches
  integer :: ierr
  
  ierr = ionc_get_1d_network_branches_count_ugrid(ioncid, networkid, nBranches)
  
end function ionc_get_1d_network_branches_count_dll
    
function ionc_get_1d_network_branches_geometry_coordinate_count_dll(ioncid, networkid, ngeometrypoints) result(ierr) bind(C, name="ionc_get_1d_network_branches_geometry_coordinate_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_network_branches_geometry_coordinate_count_dll
  
  integer(kind=c_int), intent(in)    :: ioncid, networkid 
  integer(kind=c_int), intent(out) :: ngeometrypoints
  integer :: ierr
  
  ierr = ionc_get_1d_network_branches_geometry_coordinate_count_ugrid(ioncid, networkid, ngeometrypoints)
  
end function ionc_get_1d_network_branches_geometry_coordinate_count_dll
    
function ionc_read_1d_network_nodes_dll(ioncid, networkid, c_nodesX, c_nodesY, nodeinfo, nNodes) result(ierr) bind(C, name="ionc_read_1d_network_nodes")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_read_1d_network_nodes_dll
  
  integer(kind=c_int), intent(in)       :: ioncid, networkid, nNodes !< The dataset where i do want to create the dataset.
  type(t_ug_charinfo), intent(inout)    :: nodeinfo(nNodes)
  type(c_ptr),         intent(inout)    :: c_nodesX, c_nodesY
  double precision,    pointer          :: nodesX(:),  nodesY(:)      
  character(len=ug_idsLen)              :: nodeids(nNodes)
  character(len=ug_idsLongNamesLen)     :: nodelongnames(nNodes)
  integer                               :: ierr,i
  
  call c_f_pointer(c_nodesX, nodesX, (/ nNodes /))
  call c_f_pointer(c_nodesY, nodesY, (/ nNodes /))
  
  ierr = ionc_read_1d_network_nodes_ugrid(ioncid, networkid, nodesX, nodesY, nodeids, nodelongnames)
  
  do i=1,nNodes
       nodeinfo(i)%ids = nodeids(i)        
       nodeinfo(i)%longnames = nodelongnames(i)
  end do
  
end function ionc_read_1d_network_nodes_dll
    
function ionc_read_1d_network_branches_dll(ioncid, networkid, c_sourcenodeid, c_targetnodeid, c_branchlengths, branchinfo, c_nbranchgeometrypoints, nBranches)  result(ierr) bind(C, name="ionc_read_1d_network_branches")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_read_1d_network_branches_dll
  
  integer(kind=c_int), intent(in)       :: ioncid, networkid  !< The dataset where i do want to create the dataset.
  type(c_ptr),intent(inout)             :: c_sourcenodeid, c_targetnodeid, c_nbranchgeometrypoints,c_branchlengths 
  type(t_ug_charinfo),  intent(inout)   :: branchinfo(nBranches)
  integer,pointer                       :: sourcenodeid(:), targetnodeid(:),nbranchgeometrypoints(:) 
  character(len=ug_idsLen)              :: branchids(nBranches)
  character(len=ug_idsLongNamesLen)     :: branchlongnames(nBranches)
  double precision ,pointer             :: branchlengths(:)
  integer                               :: ierr, i, nBranches
  
  call c_f_pointer(c_sourcenodeid, sourcenodeid, (/ nBranches /))
  call c_f_pointer(c_targetnodeid, targetnodeid, (/ nBranches /))
  call c_f_pointer(c_nbranchgeometrypoints, nbranchgeometrypoints, (/ nBranches /))
  call c_f_pointer(c_branchlengths, branchlengths, (/ nBranches /))
  
  ierr = ionc_read_1d_network_branches_ugrid(ioncid, networkid, sourcenodeid, targetnodeid, branchids, branchlengths, branchlongnames, nbranchgeometrypoints)

  do i=1,nBranches
    branchinfo(i)%ids = branchids(i)        
    branchinfo(i)%longnames = branchlongnames(i)
  end do
    
end function ionc_read_1d_network_branches_dll

function ionc_read_1d_network_branches_geometry_dll(ioncid, networkid, c_geopointsX, c_geopointsY, ngeometrypoints) result(ierr) bind(C, name="ionc_read_1d_network_branches_geometry")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_read_1d_network_branches_geometry_dll
  integer(kind=c_int), intent(in)    :: ioncid, networkid  
  type(c_ptr), intent(inout)         :: c_geopointsX, c_geopointsY
  double precision,pointer           :: geopointsX(:),geopointsY(:)
  integer                            :: ierr, ngeometrypoints

  call c_f_pointer(c_geopointsX, geopointsX, (/ ngeometrypoints /))
  call c_f_pointer(c_geopointsY, geopointsY, (/ ngeometrypoints /))
  
  ierr = ionc_read_1d_network_branches_geometry_ugrid(ioncid, networkid, geopointsX, geopointsY)

end function ionc_read_1d_network_branches_geometry_dll

function ionc_create_1d_mesh_dll(ioncid, meshid, c_meshname, nmeshpoints, nmeshedges) result(ierr) bind(C, name="ionc_create_1d_mesh")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_create_1d_mesh_dll
  integer(kind=c_int)   , intent(in) :: ioncid, meshid,nmeshpoints, nmeshedges
  character(kind=c_char), intent(in) :: c_meshname(MAXSTRLEN)
  integer ::ierr
  character(len=MAXSTRLEN)           :: meshname
  
  ! Store the name
  meshname = char_array_to_string(c_meshName, strlen(c_meshname))  
   
  ierr = ionc_create_1d_mesh_ugrid(ioncid, meshid, meshname, nmeshpoints, nmeshedges) 
  
end function ionc_create_1d_mesh_dll


function ionc_write_1d_mesh_discretisation_points_dll(ioncid, meshid, c_branchidx, c_offset, nmeshpoints) result(ierr) bind(C, name="ionc_write_1d_mesh_discretisation_points")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_write_1d_mesh_discretisation_points_dll
  integer(kind=c_int), intent(in)     :: ioncid, meshid, nmeshpoints  
  type(c_ptr), intent(in)             :: c_branchidx,c_offset
  integer,pointer                     :: branchidx(:)
  double precision,pointer            :: offset(:)
  integer                             :: ierr
  
  call c_f_pointer(c_branchidx, branchidx, (/ nmeshpoints /))
  call c_f_pointer(c_offset, offset, (/ nmeshpoints /))
  
  ierr = ionc_write_1d_mesh_discretisation_points_ugrid(ioncid, meshid, branchidx, offset) 
  
end function ionc_write_1d_mesh_discretisation_points_dll


function ionc_get_1d_mesh_discretisation_points_count_dll(ioncid, meshid, nmeshpoints) result(ierr) bind(C, name="ionc_get_1d_mesh_discretisation_points_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_mesh_discretisation_points_count_dll
  integer(kind=c_int), intent(in)    :: ioncid, meshid
  integer(kind=c_int), intent(inout) :: nmeshpoints 
  integer ::ierr
  
  ierr = ionc_get_1d_mesh_discretisation_points_count_ugrid(ioncid, meshid, nmeshpoints)   
  
end function ionc_get_1d_mesh_discretisation_points_count_dll


function ionc_read_1d_mesh_discretisation_points_dll(ioncid, meshid, c_branchidx, c_offset,nmeshpoints) result(ierr) bind(C, name="ionc_read_1d_mesh_discretisation_points")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_read_1d_mesh_discretisation_points_dll
  integer(kind=c_int), intent(in)   :: ioncid, meshid, nmeshpoints 
  type(c_ptr), intent(inout)        :: c_branchidx, c_offset
  double precision,pointer          :: offset(:)
  integer,pointer                   :: branchidx(:)
  integer                           :: ierr
  
  call c_f_pointer(c_branchidx, branchidx, (/ nmeshpoints /))
  call c_f_pointer(c_offset, offset, (/ nmeshpoints /))
  
  ierr = ionc_read_1d_mesh_discretisation_points_ugrid(ioncid, meshid, branchidx, offset)
  
end function ionc_read_1d_mesh_discretisation_points_dll

!
! mesh links
!

function ionc_def_mesh_contact_dll(ioncid, contactsmesh, c_contactmeshname, ncontacts, idmesh1, idmesh2, locationType1Id, locationType2Id) result(ierr) bind(C, name="ionc_def_mesh_contact")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_def_mesh_contact_dll

   integer, intent(in)                :: ioncid, ncontacts, idmesh1, idmesh2,locationType1Id,locationType2Id   
   character(kind=c_char), intent(in) :: c_contactmeshname(MAXSTRLEN)
   character(len=MAXSTRLEN)           :: contactmeshname 
   integer, intent(inout)             :: contactsmesh
   integer                            :: ierr 
   
   ! Store the name
   contactmeshname = char_array_to_string(c_contactmeshname, strlen(c_contactmeshname))  
  
   ierr = ionc_def_mesh_contact_ugrid(ioncid, contactsmesh, contactmeshname, ncontacts, idmesh1, idmesh2, locationType1Id, locationType2Id)

end function ionc_def_mesh_contact_dll 

function ionc_get_contacts_count_dll(ioncid, contactsmesh, ncontacts) result(ierr) bind(C, name="ionc_get_contacts_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_contacts_count_dll
   integer, intent(in)                :: ioncid, contactsmesh
   integer, intent(inout)             :: ncontacts
   integer                            :: ierr
   
   ierr = ionc_get_contacts_count_ugrid(ioncid, contactsmesh, ncontacts) 
   
end function ionc_get_contacts_count_dll

function ionc_put_mesh_contact_dll(ioncid, contactsmesh, c_mesh1indexes, c_mesh2indexes, contactsinfo, ncontacts) result(ierr) bind(C, name="ionc_put_mesh_contact")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_put_mesh_contact_dll
   integer, intent(in)                   :: ioncid, contactsmesh, ncontacts
   type(c_ptr), intent(in)               :: c_mesh1indexes, c_mesh2indexes
   type(t_ug_charinfo),  intent(in)      :: contactsinfo(ncontacts)
   integer,pointer                       :: mesh1indexes(:), mesh2indexes(:)
   character(len=ug_idsLen)              :: contactsids(ncontacts)
   character(len=ug_idsLongNamesLen)     :: contactslongnames(ncontacts)
   integer                               :: ierr,i
   
   call c_f_pointer(c_mesh1indexes, mesh1indexes, (/ ncontacts /))
   call c_f_pointer(c_mesh2indexes, mesh2indexes, (/ ncontacts /))

   do i=1,ncontacts
      contactsids(i) = contactsinfo(i)%ids        
      contactslongnames(i) = contactsinfo(i)%longnames
   end do
   
   ierr = ionc_put_mesh_contact_ugrid(ioncid, contactsmesh, mesh1indexes, mesh2indexes, contactsids, contactslongnames) 
   
end function ionc_put_mesh_contact_dll

function ionc_get_mesh_contact_dll(ioncid, contactsmesh, c_mesh1indexes, c_mesh2indexes, contactsinfo, ncontacts) result(ierr) bind(C, name="ionc_get_mesh_contact")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_mesh_contact_dll
   integer, intent(in)                   :: ioncid, contactsmesh, ncontacts
   type(c_ptr), intent(inout)            :: c_mesh1indexes, c_mesh2indexes
   integer,    pointer                   :: mesh1indexes(:), mesh2indexes(:)  
   character(len=ug_idsLen)              :: contactsids(ncontacts)
   character(len=ug_idsLongNamesLen)     :: contactslongnames(ncontacts)
   type(t_ug_charinfo),  intent(inout)   :: contactsinfo(ncontacts)
   integer                               :: ierr, i
   
   call c_f_pointer(c_mesh1indexes, mesh1indexes, (/ ncontacts /))
   call c_f_pointer(c_mesh2indexes, mesh2indexes, (/ ncontacts /))
      
   ierr = ionc_get_mesh_contact_ugrid(ioncid, contactsmesh, mesh1indexes, mesh2indexes, contactsids, contactslongnames) 
   
   do i=1,ncontacts
     contactsinfo(i)%ids = contactsids(i)        
     contactsinfo(i)%longnames = contactslongnames(i)
   end do
   
end function ionc_get_mesh_contact_dll

!
! Clone functions
!

function ionc_clone_mesh_definition_dll( ncidin, ncidout, meshidin, meshidout )  result(ierr) bind(C, name="ionc_clone_mesh_definition")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_clone_mesh_definition_dll
   integer, intent(in)    :: ncidin, ncidout, meshidin
   integer, intent(inout)    :: meshidout
   integer                :: ierr

   ierr = ionc_clone_mesh_definition_ugrid( ncidin, ncidout, meshidin, meshidout )

end function ionc_clone_mesh_definition_dll

function ionc_clone_mesh_data_dll( ncidin, ncidout, meshidin, meshidout )  result(ierr) bind(C, name="ionc_clone_mesh_data")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_clone_mesh_data_dll
   integer, intent(in)    :: ncidin, ncidout, meshidin
   integer, intent(in)    :: meshidout
   integer                :: ierr

   ierr = ionc_clone_mesh_data_ugrid( ncidin, ncidout, meshidin, meshidout )

end function ionc_clone_mesh_data_dll

function ionc_get_lib_versionversion_dll( ncidin, c_version_string)  result(ierr) bind(C, name="ionc_get_lib_fullversion")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_lib_versionversion_dll
   integer, intent(in)    :: ncidin
   character(kind=c_char), intent(out) :: c_version_string(MAXSTRLEN)      !< String to contain the full version string of this io_netcdf library.
   integer                :: ierr
   
   character(len=MAXSTRLEN) :: version_string
   
   version_string = ' '
   ierr = ionc_getfullversionstring_io_netcdf(version_string)
   c_version_string= string_to_char_array(version_string, len_trim(version_string))
    
end function ionc_get_lib_versionversion_dll

!
! Get the mesh ids
!

function ionc_get_1d_network_id_dll(ioncid, networkid) result(ierr) bind(C, name="ionc_get_1d_network_id")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_network_id_dll
   integer, intent(in)    :: ioncid
   integer, intent(inout) :: networkid
   integer                :: ierr
   
   ierr = ionc_get_1d_network_id_ugrid(ioncid, networkid) 
   
end function ionc_get_1d_network_id_dll


function ionc_get_1d_mesh_id_dll(ioncid, meshid) result(ierr) bind(C, name="ionc_get_1d_mesh_id")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_1d_mesh_id_dll
   integer, intent(in)    :: ioncid
   integer, intent(inout) :: meshid
   integer                :: ierr
   
   ierr = ionc_get_1d_mesh_id_ugrid(ioncid, meshid)
   
end function ionc_get_1d_mesh_id_dll


function ionc_get_2d_mesh_id_dll(ioncid, meshid) result(ierr) bind(C, name="ionc_get_2d_mesh_id")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_2d_mesh_id_dll
   integer, intent(in)    :: ioncid
   integer, intent(inout) :: meshid
   integer                :: ierr
   
   ierr = ionc_get_2d_mesh_id_ugrid(ioncid, meshid)
   
end function ionc_get_2d_mesh_id_dll

function ionc_get_3d_mesh_id_dll(ioncid, meshid)  result(ierr) bind(C, name="ionc_get_3d_mesh_id")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_3d_mesh_id_dll
   integer, intent(in)    :: ioncid
   integer, intent(inout) :: meshid
   integer                :: ierr
   
   ierr = ionc_get_3d_mesh_id_ugrid(ioncid, meshid)
   
end function ionc_get_3d_mesh_id_dll

end module io_netcdf_api
