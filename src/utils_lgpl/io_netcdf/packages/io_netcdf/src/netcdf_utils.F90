!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2021.
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

!> Utility module for additional manipulation/inquiry of NetCDF files, on top of the basic nf90* primitives.
module netcdf_utils
use netcdf
implicit none

private

public :: ncu_format_to_cmode
public :: ncu_inq_var_fill, ncu_copy_atts, ncu_copy_chunking_deflate
public :: ncu_clone_vardef

! Copied from official NetCDF: typeSizes.f90
integer, parameter ::   OneByteInt = selected_int_kind(2), &
                        TwoByteInt = selected_int_kind(4), &
                       FourByteInt = selected_int_kind(9), &
                      EightByteInt = selected_int_kind(18)
integer, parameter ::                                          &
                      FourByteReal = selected_real_kind(P =  6, R =  37), &
                     EightByteReal = selected_real_kind(P = 13, R = 307)

!> Compatibility function: returns the fill settings for a variable in a netCDF-3 file.
interface ncu_inq_var_fill
   module procedure ncu_inq_var_fill_int4
   module procedure ncu_inq_var_fill_real8
end interface ncu_inq_var_fill

   contains


!> Returns the NetCDF creation mode flag value, given the colloquial
!! format number (3 or 4).
!! Use this returned value as cmode argument to nf90_create calls.
!!
!! NOTE: the input number is *not* equivalent with the library's
!! NF90_FORMAT_* constants!
pure function ncu_format_to_cmode(iformatnumber) result(cmode)
   integer, intent(in) :: iformatnumber !< The NetCDF format version (3 or 4, colloquially speaking)
   integer             :: cmode         !< Return value (for example NF90_CLASSIC_MODEL or NF90_NETCDF4), ready for use in nf90_create calls.

   select case(iformatnumber)
   case(3)
      cmode = NF90_CLASSIC_MODEL
   case(4)
      cmode = NF90_NETCDF4
   case default
      cmode = NF90_CLOBBER ! 0: use library default
   end select
end function ncu_format_to_cmode

      
!> Copy all attributes from a variable or dataset into another variable/dataset.
!! Returns:
!     nf90_noerr if all okay, otherwise an error code
!!
!! Note: The variable in the output file must already exist.
function ncu_copy_atts( ncidin, ncidout, varidin, varidout ) result(ierr)
   integer, intent(in)            :: ncidin   !< ID of the input NetCDF file
   integer, intent(in)            :: ncidout  !< ID of the output NetCDF file
   integer, intent(in)            :: varidin  !< ID of the variable in the input file, or NF90_GLOBAL for global attributes.
   integer, intent(in)            :: varidout !< ID of the variable in the output file, or NF90_GLOBAL for global attributes.

   integer                        :: ierr
   integer                        :: i

   character(len=nf90_max_name)   :: attname
   integer                        :: natts

   ierr = -1

   ierr = nf90_inquire_variable( ncidin, varidin, nAtts=natts )
   if ( ierr == nf90_enotvar ) then
      ierr = nf90_inquire( ncidin, nAttributes=natts )
   endif
   if ( ierr /= nf90_noerr ) then
      return
   endif

   do i = 1,natts
      ierr = nf90_inq_attname( ncidin, varidin, i, attname )
      if ( ierr /= nf90_noerr ) then
         return
      endif

      ierr = nf90_copy_att( ncidin, varidin, attname, ncidout, varidout )
      if ( ierr /= nf90_noerr ) then
         return
      endif
   enddo

   ierr = nf90_noerr
end function ncu_copy_atts


!> Clones a NetCDF variable definition.
!!
!! The cloned variable will appear under a new name in the (output) file.
!! No data will be copied. Optionally, different standard_name, long_name
!! and units may directly be specified.
function ncu_clone_vardef(ncidin, ncidout, varidin, newname, varidout, &
                          newstdname, newlongname, newunits) result(ierr)
   integer,                    intent(in   ) :: ncidin      !< ID of the input NetCDF file
   integer,                    intent(in   ) :: ncidout     !< ID of the output NetCDF file (can be the same as input NetCDF file)
   integer,                    intent(in   ) :: varidin     !< ID of the variable in the input file.
   character(len=*),           intent(in   ) :: newname     !< Variable name for the new variable.
   integer,                    intent(  out) :: varidout    !< ID of the variable in the output file.
   character(len=*), optional, intent(in   ) :: newstdname  !< New standard_name for the new variable.
   character(len=*), optional, intent(in   ) :: newlongname !< New long_name for the new variable.
   character(len=*), optional, intent(in   ) :: newunits    !< New units for the new variable.


   integer                        :: ierr
   integer                        :: i

   character(len=nf90_max_name)   :: attname
   integer                        :: natts
   integer :: ndims, xtype
   integer, allocatable ::dimids(:)

   ierr = -1

   ierr = nf90_inquire_variable(ncidin, varidin, xtype = xtype, nDims=ndims, nAtts = natts)
   if ( ierr /= nf90_noerr ) then
      return
   endif

   ! Currently skipped properties in nf90_inquire_variable:
   !  logical, optional, intent(out) :: contiguous
   !  integer, optional, dimension(:), intent(out) :: chunksizes
   !  integer, optional, intent(out) :: deflate_level
   !  logical, optional, intent(out) :: shuffle, fletcher32
   !  integer, optional, intent(out) :: endianness

   allocate(dimids(ndims))
   ierr = nf90_inquire_variable(ncidin, varidin, dimids=dimids)

   ierr = nf90_def_var(ncidout, newname, xtype, dimids, varidout)

   ! TODO: AvD: consider copying all attributes
   !do i = 1,natts
   !   ierr = nf90_inq_attname( ncidin, varidin, i, attname )
   !   if ( ierr /= nf90_noerr ) then
   !      return
   !   endif
   !
   !   ierr = nf90_copy_att( ncidin, varidin, attname, ncidout, varidout )
   !   if ( ierr /= nf90_noerr ) then
   !      return
   !   endif
   !enddo

   if (present(newstdname)) then
      ierr = nf90_put_att(ncidout, varidout, 'standard_name', newstdname)
   end if

   if (present(newlongname)) then
      ierr = nf90_put_att(ncidout, varidout, 'long_name', newlongname)
   end if

   if (present(newunits)) then
      ierr = nf90_put_att(ncidout, varidout, 'units', newunits)
   end if

   ierr = nf90_noerr
end function ncu_clone_vardef


!> Compatibility function: returns the fill settings for a variable in a netCDF-3 file.
function ncu_inq_var_fill_int4( ncid, varid, no_fill, fill_value) result(ierr)
   integer,                   intent(in)  :: ncid        !< ID of the NetCDF dataset
   integer,                   intent(in)  :: varid       !< ID of the variable in the data set
   integer,                   intent(out) :: no_fill     !< An integer that will always get 1 (for forward compatibility).
   integer(kind=FourByteInt), intent(out) :: fill_value  !< This will get the fill value for this variable.

   integer :: ierr ! Error status, nf90_noerr = if successful.

   no_fill = 1

   ierr = nf90_get_att(ncid, varid, '_FillValue', fill_value)
   if (ierr /= nf90_noerr) then
      fill_value = nf90_fill_int
      ierr = nf90_noerr
   end if
end function ncu_inq_var_fill_int4


!> Compatibility function: returns the fill settings for a variable in a netCDF-3 file.
function ncu_inq_var_fill_real8( ncid, varid, no_fill, fill_value) result(ierr)
   integer,                   intent(in)  :: ncid        !< ID of the NetCDF dataset
   integer,                   intent(in)  :: varid       !< ID of the variable in the data set
   integer,                   intent(out) :: no_fill     !< An integer that will always get 1 (for forward compatibility).
   real(kind=EightByteReal),  intent(out) :: fill_value  !< This will get the fill value for this variable.

   integer :: ierr ! Error status, nf90_noerr = if successful.
   
   no_fill = 1

   ierr = nf90_get_att(ncid, varid, '_FillValue', fill_value)
   if (ierr /= nf90_noerr) then
      fill_value =  nf90_fill_double
      ierr = nf90_noerr
   end if
end function ncu_inq_var_fill_real8

!> Copy chunking and deflate settings from a variable or dataset into another variable/dataset.
!! Returns:
!     nf90_noerr if all okay, otherwise an error code
!!
!! Note: The variable in the output file must already exist.
function ncu_copy_chunking_deflate( ncidin, ncidout, varidin, varidout, ndx ) result(ierr)
   integer, intent(in)            :: ncidin   !< ID of the input NetCDF file
   integer, intent(in)            :: ncidout  !< ID of the output NetCDF file
   integer, intent(in)            :: varidin  !< ID of the variable in the input file, or NF90_GLOBAL for global attributes.
   integer, intent(in)            :: varidout !< ID of the variable in the output file, or NF90_GLOBAL for global attributes.
   integer, intent(in), optional  :: ndx      !< Number of flow nodes (internal + boundary) for output file

   integer                        :: ierr

   character(len=nf90_max_name)   :: name
   integer                        :: storage, ndims, shuffle, deflate, deflate_level
   integer, allocatable           :: chunksizes(:)

   ierr = -1

   if (varidin /= NF90_GLOBAL) then
      !
      ! copy chuncking settings, if available
      !
      ierr = nf90_inquire_variable( ncidin, varidin, nDims=ndims, name=name )
      if (ierr == nf90_noerr .and. ndims > 0) then
         allocate(chunksizes(ndims))
         ierr = nf90_inq_var_chunking(ncidin, varidin, storage, chunksizes)
         if (ierr == 0 .and. storage == nf90_chunked) then
            !
            ! chuncking is on for this variable
            !
            if (present(ndx)) then
               !
               ! first dimension is ndx, update with global ndx
               !
               chunksizes(1) = ndx
            endif
            ierr = nf90_def_var_chunking(ncidout, varidout, storage, chunksizes)
            if (ierr /= 0) write(*,*) 'nf90_def_var_chunking failed for var ', trim(name)
         endif
         deallocate(chunksizes)
      endif
      !
      ! copy deflation settings, if available
      !
      ierr = nf90_inq_var_deflate(ncidin, varidin, shuffle, deflate, deflate_level)
      if (ierr == nf90_noerr .and. deflate == 1) then
         ierr = nf90_def_var_deflate(ncidout, varidout, shuffle, deflate, deflate_level)
         if (ierr /= 0) write(*,*) 'nf90_def_var_deflate failed for var ', trim(name)
      endif
   endif

   ierr = nf90_noerr
end function ncu_copy_chunking_deflate

end module netcdf_utils
