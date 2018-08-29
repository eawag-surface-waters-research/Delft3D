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
use m_flowtimes, only : incr_classes_wl, incr_classes_wd, ti_incre
use m_flowgeom, only : ndx
use unstruc_model, only : md_incrfile
use unstruc_netcdf, only : check_error
use netcdf
use MessageHandling, only : mess, LEVEL_ERROR, LEVEL_INFO
use m_flowparameters, only : eps10

implicit none

private

public :: write_incrementals

integer, parameter :: int8 = 1     ! also local storage compact in 1 byte

logical :: netcdf_output

integer, parameter :: type_very_compact       = 1  ! store -1, 0, -1 for whole grid
integer, parameter :: type_new_class          = 2  ! store new class for whole grid
integer, parameter :: type_only_changed_celss = 3  ! not yet implemented: store index of changed cells
integer            :: output_type             = type_new_class

integer, parameter :: incremental_deflate = 5
integer, parameter :: incremental_chunksize = 20

integer :: id_nodeId = -1, id_time = -1, id_incr_s1 = -1, id_incr_hs = -1
integer :: id_jumps_s1, id_jumps_hs
integer :: time_index = 0

contains

subroutine write_incrementals(tim)
   real(kind=hp), intent(in) :: tim

   logical, save :: isFirst = .true.
   integer, save :: unit = 0
   integer :: ierr
   integer(kind=int8), pointer, save :: previous_s1(:) => null()
   integer(kind=int8), pointer, save :: previous_hs(:) => null()
   integer(kind=int8), pointer :: current_s1(:), current_hs(:)
   character(len=:), allocatable :: errmsg

   ierr = nf90_noerr

   time_index = time_index + 1
   if (isFirst) then
      netcdf_output = (index(md_incrfile, '.nc') > 0 .or. index(md_incrfile, '.NC') > 0)

      ! temp for testing with one exe
      if (index(md_incrfile, '1')) then
         output_type = 1
      else if (index(md_incrfile, '2')) then
         output_type = 2
      endif

      if (netcdf_output) then
#ifdef NetCDF4
                                 ierr = nf90_create(md_incrfile, NF90_HDF5, unit)
                                 call mess(LEVEL_INFO, 'opening incremental file as NetCDF-4 file.')
#else
                                 ierr = nf90_create(md_incrfile, NF90_64BIT_OFFSET, unit)
                                 call mess(LEVEL_INFO, 'opening incremental file as NetCDF-3 (64bit) file.')
#endif
         if (ierr == nf90_noerr) ierr = nf90_def_dim(unit, 'nodeId', ndx, id_nodeId)
         if (ierr == nf90_noerr) ierr = nf90_def_dim(unit, 'time', nf90_unlimited, id_time)
         if (size(incr_classes_wl) > 0) then
            if (ierr == nf90_noerr) then
#ifdef NetCDF4
               ierr = nf90_def_var(unit, 'incremental_s1' , nf90_byte, [id_nodeId, id_time] , id_incr_s1, deflate_level = incremental_deflate)
               if (ierr == nf90_noerr) ierr = nf90_def_var_chunking(unit, id_incr_s1, NF90_CHUNKED, [ndx, incremental_chunksize])
               if (ierr == nf90_noerr) then
                  call mess(LEVEL_INFO, 'successfully defined incremental_s1 with deflate_level and chunksize =', incremental_deflate, incremental_chunksize)
               endif
#else
               ierr = nf90_def_var(unit, 'incremental_s1' , nf90_byte, [id_nodeId, id_time] , id_incr_s1)
#endif
            endif
            if (output_type == type_very_compact) then
               if (ierr == nf90_noerr) ierr = nf90_def_var(unit, 'jumps_s1' , nf90_int, [id_time] , id_jumps_s1)
            endif
         endif
         if (size(incr_classes_wd) > 0) then
            if (ierr == nf90_noerr) then
#ifdef NetCDF4
               ierr = nf90_def_var(unit, 'incremental_hs' , nf90_byte, [id_nodeId, id_time] , id_incr_hs, deflate_level = incremental_deflate)
               if (ierr == nf90_noerr) ierr = nf90_def_var_chunking(unit, id_incr_hs, NF90_CHUNKED, [ndx, incremental_chunksize])
               if (ierr == nf90_noerr) then
                  call mess(LEVEL_INFO, 'successfully defined incremental_hs with deflate_level and chunksize =', incremental_deflate, incremental_chunksize)
               endif
#else
               ierr = nf90_def_var(unit, 'incremental_hs' , nf90_byte, [id_nodeId, id_time] , id_incr_hs)
#endif
            endif
            if (output_type == type_very_compact) then
               if (ierr == nf90_noerr) ierr = nf90_def_var(unit, 'jumps_hs' , nf90_int, [id_time] , id_jumps_hs)
            endif
         endif
         if (ierr == nf90_noerr) ierr = nf90_enddef(unit)
      else
         open(newunit=unit, file=md_incrfile)
      endif
   endif

   if (size(incr_classes_wl) > 0) then
      allocate(current_s1(ndx))
      call put_in_classes(incr_classes_wl, s1, current_s1)
   endif
   if (size(incr_classes_wd) > 0) then
      allocate(current_hs(ndx))
      call put_in_classes(incr_classes_wd, hs, current_hs)
   endif

   if (isFirst) then
      if (size(incr_classes_wl) > 0) then
         if (ierr == nf90_noerr) ierr = write_initial_classes(unit, current_s1, 's1', [id_incr_s1, id_jumps_s1])
         previous_s1 => current_s1
      endif
      if (size(incr_classes_wd) > 0) then
         if (ierr == nf90_noerr) ierr = write_initial_classes(unit, current_hs, 'hs', [id_incr_hs, id_jumps_hs])
         previous_hs => current_hs
      endif
      isFirst = .false.
   else
      if (size(incr_classes_wl) > 0) then
         if (ierr == nf90_noerr) ierr = write_changed_classes_update_previous(unit, previous_s1, current_s1, 's1', [id_incr_s1, id_jumps_s1])
      endif
      if (size(incr_classes_wd) > 0) then
         if (ierr == nf90_noerr) ierr = write_changed_classes_update_previous(unit, previous_hs, current_hs, 'hs', [id_incr_hs, id_jumps_hs])
      endif
   endif

   if (comparereal(tim, ti_incre, eps10) /= -1) then
      if (netcdf_output) then
         if (ierr == nf90_noerr) ierr = nf90_close(unit)
      else
         close(unit)
      endif
      if (associated(previous_s1)) deallocate(previous_s1)
      if (associated(previous_hs)) deallocate(previous_hs)
   else if (netcdf_output) then
      if (ierr == nf90_noerr) ierr = nf90_sync(unit)  ! flush output to file
   endif

   if (ierr /= nf90_noerr) then
      errmsg = 'error writing to incremental output file'
      call check_error(ierr, errmsg)
      call mess(LEVEL_ERROR, errmsg)
   endif
end subroutine write_incrementals

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

function write_initial_classes(unit, classes, field, varids) result(ierr)
   integer, intent(in) :: unit, varids(2)
   integer(kind=int8), intent(in) :: classes(:)
   character(len=*), intent(in) :: field
   integer :: ierr

   if (netcdf_output) then
      ierr = nf90_put_var(unit, varids(1), classes, [1, time_index])
      if (ierr == 0 .and. output_type == type_very_compact) then
         ierr = nf90_put_var(unit, varids(2), [0], [time_index])
      endif
   else
                     write(unit,*,iostat=ierr) 'initial classes:', field
      if (ierr == 0) write(unit,'(10(i0,x))', iostat=ierr) classes
   endif
end function write_initial_classes

function write_changed_classes_update_previous(unit, previous, current, field, varids) result(ierr)
   integer, intent(in) :: unit, varids(2)
   integer(kind=int8), pointer, intent(inout) :: previous(:), current(:)
   character(len=*), intent(in) :: field
   integer :: ierr

   integer :: i, cnt, dim
   integer(kind=int8), allocatable :: diff(:)

   dim = size(previous)
   ierr = nf90_noerr

   if ( .not. netcdf_output) then
      if (ierr == 0) write(unit,*,iostat=ierr) 'changes in classes:', field, ' at t = ', time_index
   endif

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

   if (netcdf_output) then
      if (output_type == type_very_compact) then
         if (ierr == 0) ierr = nf90_put_var(unit, varids(1), diff, [1, time_index])
         if (ierr == 0) ierr = nf90_put_var(unit, varids(2), [cnt], [time_index])
      else
         if (ierr == 0) ierr = nf90_put_var(unit, varids(1), current, [1, time_index])
      endif
   else
      if (output_type == type_very_compact) then
         if (ierr == 0) write(unit,'(10(i0,x))', iostat=ierr) diff
         if (ierr == 0 .and. cnt > 0) then
            write(unit,*,iostat=ierr) 'more than 1 change in class in ', cnt, ' cells.'
         endif
      else
         if (ierr == 0) write(unit,'(10(i0,x))', iostat=ierr) current
      endif
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

end module unstruc_netcdf_incremental
