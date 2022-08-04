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

!> I/O module for writing ACDD information (Attribute Convention for Dataset Discovery)
!! into NetCDF files.
!! ACDD Conventions website: https://wiki.esipfed.org/Category:Attribute_Conventions_Dataset_Discovery

module io_netcdf_acdd
use meshdata, only: ug_strLenMeta
use io_netcdf, only: IONC_NOERR
use netcdf
use netcdf_utils, only: ncu_append_atts

implicit none

character(len=8),  parameter :: IONC_CONV_ACDD     = 'ACDD-1.3'      !< Version of ACDD conventions currently adopted (Attribute Convention for Dataset Discovery).

contains

!> Add geospatial bounding box global attributes to an open NetCDF dataset.
function ionc_add_geospatial_bounds(ncid, lat_min, lat_max, lon_min, lon_max) result(ierr)
   integer,          intent(in   ) :: ncid    !< Already opened NetCDF id to put global attributes into.
   double precision, intent(in   ) :: lat_min !< Geospatial_lat_min specifies the southernmost latitude covered by the dataset.
   double precision, intent(in   ) :: lat_max !< Geospatial_lat_max specifies the northernmost latitude covered by the dataset.
   double precision, intent(in   ) :: lon_min !< Geospatial_lon_min specifies the westernmost longitude covered by the dataset.
   double precision, intent(in   ) :: lon_max !< Geospatial_lon_max specifies the easternmost longitude covered by the dataset.
   integer                         :: ierr    !< Result status (IONC_NOERR==NF90_NOERR) if successful.

   integer :: jaInDefine
   character(len=ug_strLenMeta) :: geospatial_lat_units = "degrees_north"
   character(len=ug_strLenMeta) :: geospatial_lon_units = "degrees_east"

   ierr = IONC_NOERR

   ! Put dataset in define mode (possibly again) to add global attributes
   jaInDefine = 0
   ierr = nf90_redef(ncid)
   if (ierr == nf90_eindefine) then
      jaInDefine = 1 ! Was still in define mode.
   end if

   ierr = nf90_put_att(ncid, nf90_global, 'geospatial_lat_min',   lat_min)
   ierr = nf90_put_att(ncid, nf90_global, 'geospatial_lat_max',   lat_max)
   ierr = nf90_put_att(ncid, nf90_global, 'geospatial_lat_units', trim(geospatial_lat_units))
   ierr = nf90_put_att(ncid, nf90_global, 'geospatial_lon_min',   lon_min)
   ierr = nf90_put_att(ncid, nf90_global, 'geospatial_lon_max',   lon_max)
   ierr = nf90_put_att(ncid, nf90_global, 'geospatial_lon_units', trim(geospatial_lon_units))

   ! Record this also in the :Conventions attribute
   ierr = ncu_append_atts(ncid, nf90_global, 'Conventions', ' '//trim(IONC_CONV_ACDD))

   ! Leave the dataset in the same mode as we got it.
   if (jaInDefine == 0) then
      ierr = nf90_enddef(ncid)
   end if

end function ionc_add_geospatial_bounds

end module io_netcdf_acdd

