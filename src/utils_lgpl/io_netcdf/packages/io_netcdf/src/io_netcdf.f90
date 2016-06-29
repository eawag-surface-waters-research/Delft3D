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


!> I/O module for reading and writing NetCDF files based on selected NetCDF conventions (UGRID, and more in the future).
!! @see io_ugrid
module io_netcdf
use netcdf
use io_ugrid
implicit none

!
! Parameters
!
public :: IONC_CONV_NULL
public :: IONC_CONV_UGRID
public :: IONC_CONV_SGRID
public :: IONC_CONV_OTHER

!
! Types
!
public :: t_ionc

!
! Subroutines
!
! public :: ionc_open
public :: ionc_inq_conventions

!private

!
! NetCDF conventions support. Based on the conventions used in the file,
! this library supports a bigger or smaller amount of intelligent inquiry functions.
!
integer, parameter :: IONC_CONV_NULL  = 0  !< Dataset conventions not yet detected
integer, parameter :: IONC_CONV_UGRID = 1  !< Dataset based on UGRID-conventions
integer, parameter :: IONC_CONV_SGRID = 2  !< Dataset based on SGRID-conventions
integer, parameter :: IONC_CONV_OTHER = 99 !< Dataset based on unknown or unsupported conventions (user should fall back to NetCDF native API calls)

integer, parameter :: MAXSTRLEN = 255 !< Max string length (e.g. for inquiring attribute values.

!
! Error statuses
!
integer, parameter :: IONC_NOERR = 0 !< Successful.
integer, parameter :: IONC_BADID = 1 !< Not a valid IONC dataset id.

!> Data type with reference to a NetCDF dataset
type t_ionc
   integer                  :: ncid      =  0               !< The underlying native NetCDF data set id.
   integer                  :: iconvtype =  IONC_CONV_OTHER !< Detected type of the conventions used in this dataset.
   type(t_ug_file), pointer :: ug_file   => null()          !< For UGRID-type files, the underlying file structure.
end type t_ionc

type(t_ionc), allocatable :: datasets(:) !< List of available datasets, maintained in global library state, indexed by the unique ionc_id.
integer                   :: ndatasets   !< Number of available datasets. May be smaller than array size of datasets(:).

   contains

!> Inquire the NetCDF conventions used in the dataset.
function ionc_inq_conventions(ioncid, iconvtype) result(ierr)
   integer, intent(in)  :: ioncid    !< The IONC data set id.
   integer, intent(out) :: iconvtype !< The NetCDF conventions type of the dataset.
   integer              :: ierr      !< Result status, ionc_noerr if successful.

   ierr = IONC_NOERR

   if (ioncid < 1 .or. ioncid > ndatasets) then
      ierr = IONC_BADID
      goto 999
   end if

   ! Either get conventions from previously detected value, or detect them now.
   if (datasets(ioncid)%iconvtype == IONC_CONV_NULL) then
      ierr = detect_conventions(ioncid, datasets(ioncid)%iconvtype)
      if (ierr /= IONC_NOERR) then
         goto 999
      end if
   end if
   iconvtype = datasets(ioncid)%iconvtype

   ! Successful
   return

999 continue
   ! Some error (status was set earlier)
end function ionc_inq_conventions


!> Gets the x,y coordinates for all nodes in a single mesh from a data set.
function ionc_get_node_coordinates(ioncid, meshid, xarr, yarr) result(ierr)
   integer,             intent(in)  :: ioncid  !< The IONC data set id.
   integer,             intent(in)  :: meshid  !< The mesh id in the specified data set.
   double precision,    intent(out) :: xarr(:) !< Array for storing x-coordinates
   double precision,    intent(out) :: yarr(:) !< Array for storing y-coordinates
   integer                          :: ierr    !< Result status, ionc_noerr if successful.

   ierr = ug_get_node_coordinates(datasets(ioncid)%ncid, datasets(ioncid)%ug_file%meshids(meshid), xarr, yarr)

end function ionc_get_node_coordinates

!
! -- Private routines -----------------------------------------------------
!

!> Detect the conventions used in the given dataset.
!!
!! Detection is based on the :Conventions attribute in the file/data set.
function detect_conventions(ioncid, iconvtype) result(ierr)
   integer, intent(in)  :: ioncid    !< The IONC data set id.
   integer, intent(out) :: iconvtype !< The NetCDF conventions type of the dataset.
   integer              :: ierr      !< Result status, ionc_noerr if successful.

   character(len=MAXSTRLEN) :: convstring

   ierr = IONC_NOERR

   if (ioncid < 1 .or. ioncid > ndatasets) then
      ierr = IONC_BADID
      goto 999
   end if

   convstring = ''
   ierr = nf90_get_att(datasets(ioncid)%ncid, nf90_global, 'Conventions', convstring)
   if (ierr /= nf90_noerr) then
      goto 999
   end if

   if (index(convstring, 'UGRID') > 0) then
      iconvtype = IONC_CONV_UGRID
   else
      iconvtype = IONC_CONV_OTHER
   end if

   ! Successful
   return

999 continue
   ! Some error (status was set earlier)
end function detect_conventions


!> Re-allocates an array of datasets to a bigger size.
subroutine realloc(arr, uindex, lindex, stat, keepExisting)
   implicit none
   type(t_ionc), allocatable, intent(inout)     :: arr(:)       !< List of opened datasets, maintained in global library state, indexed by the unique ionc_id.
   integer, intent(in)                          :: uindex       !< New upper index of the array (i.e., size, if lindex==1)
   integer, intent(in), optional                :: lindex       !< (optional) New lower index of the array. Default: 1
   integer, intent(out), optional               :: stat         !< Result status of the realloc operation.
   logical, intent(in), optional                :: keepExisting !< Whether or not to keep the original array contents.

   type(t_ionc), allocatable :: b(:)
   integer        :: uind, lind, muind, mlind, lindex_

   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = 1
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr,1)
      lind = lbound(arr,1)
      equalSize = (uindex == uind) .and. (lindex_ == lind)
      if (equalSize .and. docopy) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind, lindex_)
         muind = min(uind, uindex)
         allocate (b(mlind:muind))
         b(mlind:muind) = arr(mlind:muind)
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_:uindex), stat = localErr)
   endif
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      arr(mlind:muind) = b(mlind:muind)
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr

end subroutine realloc

end module io_netcdf