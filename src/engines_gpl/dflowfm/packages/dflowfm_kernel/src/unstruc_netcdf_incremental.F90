!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2018.
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
module unstruc_netcdf_incremental
use precision, only : hp
use precision_basics, only : comparereal
use m_flow, only : s1, hs
use m_flowtimes, only : incr_classes_wl, incr_classes_wd, ti_incre, refdat
use m_flowgeom, only : ndx, ndxi
use m_cell_geometry, only : ndx2d
use unstruc_model, only : md_incrfile
use unstruc_netcdf, only : check_error, t_unc_mapids, unc_create, ug_meta_fm, unc_def_var_nonspatial, &
       UNC_LOC_S, unc_def_var_map, unc_write_flowgeom_filepointer_ugrid, unc_put_var_map_byte
use io_ugrid, only : ug_addglobalatts
use netcdf
use MessageHandling, only : mess, LEVEL_ERROR, LEVEL_INFO, LEVEL_FATAL
use m_flowparameters, only : eps10

implicit none

private

public :: write_incrementals_ugrid
type(t_unc_mapids), public :: m_incids

integer, parameter :: int8 = 1     ! also local storage compact in 1 byte

integer, parameter :: type_very_compact       = 1  ! store -1, 0, -1 for whole grid
integer, parameter :: type_new_class          = 2  ! store new class for whole grid
integer, parameter :: output_type             = type_new_class

integer, parameter :: incremental_deflate = 5
integer, parameter :: incremental_chunksize_ndx = 2000
integer, parameter :: incremental_chunksize_time = 10

integer :: id_nodeId = -1, id_incr_s1 = -1, id_incr_hs = -1
integer :: id_jumps_s1, id_jumps_hs
integer :: id_class_dim_s1, id_class_dim_hs
integer :: time_index = 0

#ifdef NetCDF4
integer,          parameter :: open_mode = NF90_HDF5
character(len=*), parameter :: nc_file_type = 'NetCDF-4'
#else
integer,          parameter :: open_mode = NF90_64BIT_OFFSET
character(len=*), parameter :: nc_file_type = 'NetCDF-3 (64bit)'
#endif

integer(kind=int8), pointer, save :: previous_s1(:) => null()
integer(kind=int8), pointer, save :: previous_hs(:) => null()
integer(kind=int8), pointer :: current_s1(:), current_hs(:)

contains

subroutine write_incrementals_ugrid(incids, tim)
   type(t_unc_mapids), intent(inout) :: incids   !< class file and other NetCDF ids.
   real(kind=hp), intent(in) :: tim

   integer :: ierr, ndim
   integer, parameter :: jabndnd_ = 0 !< Whether to include boundary nodes (1) or not (0). Default: no.
   integer :: id_class_s1, id_class_hs
   character(len=:), allocatable :: errmsg, tmpstr

   ierr = nf90_noerr

   if (incids%ncid == 0) then
      ! opening NetCDF file:
      ierr = unc_create(md_incrfile, open_mode, incids%ncid)
      call mess(LEVEL_INFO, 'opening incremental file as ' // nc_file_type // ' file.')
   endif

   ! Use nr of dimensions in netCDF file a quick check whether vardefs were written before in previous calls.
   ndim = 0
   ierr = nf90_inquire(incids%ncid, nDimensions=ndim)

   ! Only write net and flow geometry data the first time, or for a separate map file.
   if (ndim == 0) then

      ierr = ug_addglobalatts(incids%ncid, ug_meta_fm)
      call unc_write_flowgeom_filepointer_ugrid(incids, jabndnd_)

      !
      ! define dimensions:
      ierr = nf90_def_dim(incids%ncid, 'time', nf90_unlimited, incids%id_timedim)
      if (size(incr_classes_wl) > 0 .and. ierr == nf90_noerr) then
         ierr = nf90_def_dim(incids%ncid, 'class_s1', size(incr_classes_wl), id_class_dim_s1)
      endif
      if (size(incr_classes_wd) > 0 .and. ierr == nf90_noerr) then
         ierr = nf90_def_dim(incids%ncid, 'class_hs', size(incr_classes_wd), id_class_dim_hs)
      endif
      call check_error(ierr, 'definition phase dimensions of incrementals')

      ! define variables:
      tmpstr = 'seconds since '//refdat(1:4)//'-'//refdat(5:6)//'-'//refdat(7:8)//' 00:00:00'
      ierr = unc_def_var_nonspatial(incids%ncid, incids%id_time, nf90_double, [incids%id_timedim], 'time', 'time', ' ', tmpstr)

      if (size(incr_classes_wl) > 0 .and. ierr == nf90_noerr) then
         ierr = def_var_incremental_ugrid('s1', incids%ncid, id_class_s1, id_jumps_s1, incids)
      endif
      if (size(incr_classes_wd) > 0 .and. ierr == nf90_noerr) then
         ierr = def_var_incremental_ugrid('hs', incids%ncid, id_class_hs, id_jumps_hs, incids)
      endif
      if (ierr == nf90_noerr) ierr = nf90_enddef(incids%ncid)
      call check_error(ierr, 'definition phase variables of incrementals')
      time_index = 1
   else
      time_index = time_index + 1
   endif

   if (size(incr_classes_wl) > 0) then
      allocate(current_s1(ndx))
      call put_in_classes(incr_classes_wl, s1, current_s1)
   endif
   if (size(incr_classes_wd) > 0) then
      allocate(current_hs(ndx))
      call put_in_classes(incr_classes_wd, hs, current_hs)
   endif

   if (ndim == 0) then
      if (size(incr_classes_wl) > 0) then
         if (ierr == nf90_noerr) ierr = nf90_put_var(incids%ncid, id_class_s1, incr_classes_wl)
         if (ierr == nf90_noerr) ierr = write_initial_classes(incids, current_s1, 's1', id_jumps_s1)
         previous_s1 => current_s1
      endif
      if (size(incr_classes_wd) > 0) then
         if (ierr == nf90_noerr) ierr = nf90_put_var(incids%ncid, id_class_hs, incr_classes_wd)
         if (ierr == nf90_noerr) ierr = write_initial_classes(incids, current_hs, 'hs', id_jumps_hs)
         previous_hs => current_hs
      endif
   else
      if (size(incr_classes_wl) > 0) then
         if (ierr == nf90_noerr) ierr = write_changed_classes_update_previous(incids, previous_s1, current_s1, 's1', id_jumps_s1)
      endif
      if (size(incr_classes_wd) > 0) then
         if (ierr == nf90_noerr) ierr = write_changed_classes_update_previous(incids, previous_hs, current_hs, 'hs', id_jumps_hs)
      endif
   endif
   if (ierr == nf90_noerr) ierr = nf90_put_var(incids%ncid, incids%id_time, tim, start=[time_index])
   call check_error(ierr, 'actual writing of incrementals')

   if (comparereal(tim, ti_incre, eps10) /= -1) then
      if (ierr == nf90_noerr) ierr = nf90_close(incids%ncid)
      if (associated(previous_s1)) deallocate(previous_s1)
      if (associated(previous_hs)) deallocate(previous_hs)
   else
      if (ierr == nf90_noerr) ierr = nf90_sync(incids%ncid)  ! flush output to file
   endif

   if (ierr /= nf90_noerr) then
      errmsg = 'error writing to incremental output file'
      call check_error(ierr, errmsg)
      call mess(LEVEL_ERROR, errmsg)
   endif
end subroutine write_incrementals_ugrid

function def_var_incremental_ugrid(name, ncid, var_id_class_bnds, var_id_jumps, incids) result(ierr)
   type(t_unc_mapids), intent(inout) :: incids   !< class file and other NetCDF ids.
   character(len=*), intent(in)      :: name
   integer,          intent(in)      :: ncid
   integer,          intent(out)     :: var_id_class_bnds, var_id_jumps
   integer                           :: ierr

   integer :: id_class, actual_chunksize, ids(4), ndims(2), i

   if (name == 's1') then
      ierr = unc_def_var_map(incids, incids%id_s1, nf90_byte, UNC_LOC_S, 's1',         'sea_surface_height',                'Water level', 'm')
      id_class = id_class_dim_s1
      ids = incids%id_s1
   else if (name == 'hs') then
      ierr = unc_def_var_map(incids, incids%id_hs, nf90_byte, UNC_LOC_S, 'waterdepth', 'sea_floor_depth_below_sea_surface', 'Water depth at pressure points', 'm')
      id_class = id_class_dim_hs
      ids = incids%id_hs
   else
      call mess(LEVEL_FATAL, 'programming error in def_var_incremental_ugrid')
   endif

   if (ierr == nf90_noerr) ierr = nf90_def_var(ncid, 'classes_'//name , nf90_double, [id_class] , var_id_class_bnds)
#ifdef NetCDF4
   ndims(1) = ndxi - ndx2d
   ndims(2) = ndx2d
   do i = 1, 2
      if (ndims(i) > 0) then
         actual_chunksize = min(incremental_chunksize_ndx, ndims(i))
         if (ierr == nf90_noerr) ierr = nf90_def_var_deflate(ncid, ids(i), 0, 1, incremental_deflate)
         if (ierr == nf90_noerr) ierr = nf90_def_var_chunking(ncid, ids(i), NF90_CHUNKED, [actual_chunksize, incremental_chunksize_time])
      endif
   enddo
   if (ierr == nf90_noerr) then
      call mess(LEVEL_INFO, 'successfully defined incremental_' // name // ' with deflate_level and chunksizes =', incremental_deflate, actual_chunksize, incremental_chunksize_time)
   endif
#endif
   if (output_type == type_very_compact) then
      if (ierr == nf90_noerr) ierr = nf90_def_var(ncid, 'jumps_'//name , nf90_int, [incids%id_timedim] , var_id_jumps)
   endif
end function def_var_incremental_ugrid

subroutine put_in_classes(incr_classes, full_field, classes)
   real(kind=hp), intent(in) :: incr_classes(:), full_field(:)
   integer(kind=int8), intent(out) :: classes(:)

   integer :: i, j, num_classes

   num_classes = size(incr_classes) + 1

   do i = 1, size(full_field)
      classes(i) = num_classes
      do j = 1, size(incr_classes)
         if (full_field(i) < incr_classes(j)) then
            classes(i)  = j
            exit
         endif
      enddo
   enddo
end subroutine put_in_classes

function write_initial_classes(incids, classes, field, varid_jumps) result(ierr)
   integer, intent(in) :: varid_jumps
   integer(kind=int8), intent(in) :: classes(:)
   character(len=*), intent(in) :: field
   type(t_unc_mapids), intent(inout) :: incids
   integer :: ierr

   integer :: var_ids(4)

   incids%idx_curtime = 1
   var_ids = get_varids(field, incids)
   ierr = unc_put_var_map_byte(incids, var_ids, UNC_LOC_S, classes)

   if (ierr == 0 .and. output_type == type_very_compact) then
      ierr = nf90_put_var(incids%ncid, varid_jumps, [0], [time_index])
   endif
end function write_initial_classes

function write_changed_classes_update_previous(incids, previous, current, field, varid_jumps) result(ierr)
   integer, intent(in) :: varid_jumps
   integer(kind=int8), pointer, intent(inout) :: previous(:), current(:)
   character(len=*), intent(in) :: field
   type(t_unc_mapids), intent(inout) :: incids
   integer :: ierr

   integer :: i, cnt, dim, var_ids(4)
   integer(kind=int8), allocatable :: diff(:)

   dim = size(previous)
   ierr = nf90_noerr

   allocate(diff(dim))
   cnt = 0
   if (output_type == type_very_compact) then
      do i = 1, dim
         diff(i) = current(i) - previous(i)
         if (abs(diff(i)) > 1) then
            cnt = cnt + 1
            diff(i) = max(-1, min(1, diff(i)))
         endif
      enddo
   endif

   var_ids = get_varids(field, incids)

   incids%idx_curtime = time_index
   if (output_type == type_very_compact) then
      ierr = unc_put_var_map_byte(incids, var_ids, UNC_LOC_S, diff)
      if (ierr == 0) ierr = nf90_put_var(incids%ncid, varid_jumps, [cnt], [time_index])
   else
      ierr = unc_put_var_map_byte(incids, var_ids, UNC_LOC_S, current)
   endif

   if (output_type == type_very_compact) then
      deallocate(current)
      do i = 1, dim
         previous(i) = previous(i) + diff(i)
      enddo
   else
      deallocate(previous)
      previous => current
   endif
   nullify(current)

   deallocate(diff)

end function write_changed_classes_update_previous

function get_varids(name, incids) result(var_ids)
   character(len=*),   intent(in) :: name
   type(t_unc_mapids), intent(in) :: incids
   integer                        :: var_ids(4)

   if (name == 's1') then
      var_ids = incids%id_s1
   else if (name == 'hs') then
      var_ids = incids%id_hs
   else
      call mess(LEVEL_FATAL, 'programming error in get_varids')
   endif
end function get_varids

end module unstruc_netcdf_incremental
