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

contains

subroutine main() bind(C, name="main")
   !DEC$ ATTRIBUTES DLLEXPORT :: main
   ! Somehow intel fortran compiler expects a main routine in the dll, it is required since interactor is used (and win calls)
implicit none

end subroutine main

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


!> Inquire the NetCDF conventions used in the dataset.
function ionc_inq_conventions_dll(ioncid, iconvtype) result(ierr) bind(C, name="ionc_inq_conventions")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_inq_conventions_dll
   integer(kind=c_int), intent(in)  :: ioncid    !< The IONC data set id.
   integer(kind=c_int), intent(out) :: iconvtype !< The NetCDF conventions type of the dataset.
   integer(kind=c_int)              :: ierr      !< Result status, ionc_noerr if successful.

   ierr = ionc_inq_conventions(ioncid, iconvtype)

end function ionc_inq_conventions_dll


!> Tries to open a NetCDF file and initialize based on its specified conventions.
function ionc_open_dll(c_path, mode, ioncid, iconvtype) result(ierr) bind(C, name="ionc_open")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_open_dll
   character(kind=c_char), intent(in   ) :: c_path(*)      !< File name for netCDF dataset to be opened.
   integer(kind=c_int),           intent(in   ) :: mode      !< NetCDF open mode, e.g. NF90_NOWRITE.
   integer(kind=c_int),           intent(  out) :: ioncid    !< The io_netcdf dataset id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid.
   integer(kind=c_int),           intent(inout) :: iconvtype !< The detected conventions in the file.
!   integer(kind=c_int), optional, intent(inout) :: chunksize !< (optional) NetCDF chunksize parameter.
   integer(kind=c_int)                          :: ierr      !< Result status (IONC_NOERR if successful).

  character(len=MAXSTRLEN) :: path
   ! TODO: AvD: Handle string length
  !path = char_array_to_string(c_path)
  
  ierr = ionc_open(path, mode, ioncid, iconvtype)

end function ionc_open_dll


!> Tries to close an open io_netcdf data set.
function ionc_close_dll(ioncid) result(ierr) bind(C, name="ionc_close")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_close_dll
   integer(kind=c_int),           intent(in   ) :: ioncid    !< The io_netcdf dataset id (this is not the NetCDF ncid, which is stored in datasets(ioncid)%ncid.
   integer(kind=c_int)                          :: ierr      !< Result status (IONC_NOERR if successful).

   ierr = ionc_close(ioncid)
end function ionc_close_dll


!
! UGRID specifics:
!

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
   integer(kind=c_int),             intent(  out) :: nmaxfacenodes !< Number of faces.
   integer(kind=c_int)                            :: ierr          !< Result status, ionc_noerr if successful.

   ierr = ionc_get_max_face_nodes(ioncid, meshid, nmaxfacenodes)

end function ionc_get_max_face_nodes_dll


!> Inquire the NetCDF conventions used in the dataset.
function ionc_get_node_coordinates_dll(ioncid, meshid, c_xptr, c_yptr, nnode) result(ierr) bind(C, name="ionc_get_node_coordinates")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_node_coordinates_dll
   integer(kind=c_int), intent(in)  :: ioncid !< The IONC data set id.
   integer(kind=c_int), intent(in)  :: meshid !< The mesh id in the specified data set.
   type(c_ptr),         intent(out) :: c_xptr !< Pointer to array for x-coordinates
   type(c_ptr),         intent(out) :: c_yptr !< Pointer to array for y-coordinates
   integer(kind=c_int), intent(in)  :: nnode  !< The number of nodes in the mesh. TODO: AvD: remove this somehow, now only required to call c_f_pointer
   integer(kind=c_int)              :: ierr   !< Result status, ionc_noerr if successful.

   double precision, pointer :: xptr(:), yptr(:)
   call c_f_pointer(c_xptr, xptr, (/ nnode /))
   call c_f_pointer(c_yptr, yptr, (/ nnode /))
   ierr = ionc_get_node_coordinates(ioncid, meshid, xptr, yptr)

end function ionc_get_node_coordinates_dll


!> Gets the face-node connectvit table for all faces in the specified mesh.
!! The output face_nodes array is supposed to be of exact correct size already.
function ionc_get_face_nodes_dll(ioncid, meshid, c_face_nodes_ptr, nface, nmaxfacenodes) result(ierr) bind(C, name="ionc_get_face_nodes")
!DEC$ ATTRIBUTES DLLEXPORT :: ionc_get_face_nodes_dll
   integer(kind=c_int), intent(in)    :: ioncid  !< The IONC data set id.
   integer(kind=c_int), intent(in)    :: meshid  !< The mesh id in the specified data set.
   type(c_ptr),         intent(  out) :: c_face_nodes_ptr !< Pointer to array for the face-node connectivity table.
   integer(kind=c_int), intent(in)    :: nface  !< The number of faces in the mesh. TODO: AvD: remove this somehow, now only required to call c_f_pointer
   integer(kind=c_int), intent(in)    :: nmaxfacenodes  !< The maximum number of nodes per face in the mesh. TODO: AvD: remove this somehow, now only required to call c_f_pointer
   integer(kind=c_int)                :: ierr    !< Result status, ionc_noerr if successful.

   integer, pointer :: face_nodes(:,:)

   call c_f_pointer(c_face_nodes_ptr, face_nodes, (/ nmaxfacenodes, nface /))

   ierr = ug_get_face_nodes(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), face_nodes)

end function ionc_get_face_nodes_dll

end module io_netcdf_api