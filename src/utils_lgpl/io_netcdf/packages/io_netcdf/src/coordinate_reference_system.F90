!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2019.
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

!  $Id$
!  $HeadURL$

!> Module for utility types and functions for working with coordinates in different coordinate systems.
module coordinate_reference_system
   use messagehandling

   implicit none

   character(len=48),  parameter :: WGS84_PROJ_STRING         = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
         !< Projection string for WGS84 system. See http://www.spatialreference.org/ref/epsg/4326/proj4/

   character(len=226), parameter :: RIJKSDRIEHOEK_PROJ_STRING = '+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel +units=m' &
      // ' +towgs84=565.4174,50.3319,465.5542,-0.398957388243134,0.343987817378283,-1.87740163998045,4.0725 +no_defs'
         !< Projection string for Dutch RijksDriehoek system. See https://publicwiki.deltares.nl/display/NETCDF/Coordinates :
         !! "note that the default proj4 (epsg) string for the Dutch RD system (EPSG:28992 & EPSG:7415) is wrong, it contains an erroneous ellipse reference, hence the full ellipse values need to be supplied."


   !> Container for information for a NetCDF attribute. Used inside t_crs.
   type nc_attribute
      character(len=64)             :: attname     !< Name of the attribute.
      integer                       :: xtype       !< Type: one of NF90_CHAR, NF90_INT, NF90_FLOAT, NF90_DOUBLE, NF90_BYTE, NF90_SHORT.
      integer                       :: len         !< Length of the attribute value (string length/array length)
      character(len=1), allocatable :: strvalue(:) !< Contains value if xtype==NF90_CHAR.
      double precision, allocatable :: dblvalue(:) !< Contains value if xtype==NF90_DOUBLE.
      real,             allocatable :: fltvalue(:) !< Contains value if xtype==NF90_FLOAT.
      integer,          allocatable :: intvalue(:) !< Contains value if xtype==NF90_INT.
      ! TODO: AvD: support BYTE/short as well?
   end type nc_attribute

   !> Container for information about coordinate reference system in a NetCDF-file.
   type t_crs
      character(len=64)               :: varname = ' ' !< Name of the NetCDF variable containing this CRS
      integer                         :: epsg_code     !< EPSG code (more info: http://spatialreference.org/)
      type(nc_attribute), allocatable :: attset(:)     !< General set with all/any attributes about this CRS.
   end type t_crs

   contains

#ifdef HAVE_PROJ
   !> Returns a projection object for the given projection string.
   !! This function uses the proj4 library.
   function get_projection(proj_string) result(projection)
      use proj

      implicit none

      character(len=*), intent(in) :: proj_string !< proj4 string describing coordinate system.

      type(pj_object)     :: projection !< coordinate system object.
      character(len=1024) :: message !< Temporary variable for writing log messages.

      write(message, *) 'Initializing projection for proj string: "', proj_string, '"'
      call mess(LEVEL_INFO, trim(message))
      projection = pj_init_plus(trim(proj_string)//char(0))
      if (.not. pj_associated(projection)) then
         write(message, *) 'Cannot initialize projection for proj string: "', proj_string, '"'
         call mess(LEVEL_ERROR, trim(message))
         return
      endif
   end function

   !> Transforms the given coordinates from the given source coordinate system to the given destination coordinate system.
   !! This subroutine uses the proj4 library for coordinate transformations.
   subroutine transform(src_projection, dst_projection, src_x, src_y, dst_x, dst_y)
      use proj

      implicit none

      type(pj_object), intent(in)                        :: src_projection !< source coordinate system object.
      type(pj_object), intent(in)                        :: dst_projection !< destination coordinate system object.
      real(kind=kind(1.0d00)), dimension(:), intent(in)  :: src_x          !< x coordinates to transform in degrees/meters.
      real(kind=kind(1.0d00)), dimension(:), intent(in)  :: src_y          !< y coordinates to transform in degrees/meters.
      real(kind=kind(1.0d00)), dimension(:), intent(out) :: dst_x          !< transformed x coordinates in degrees/meters.
      real(kind=kind(1.0d00)), dimension(:), intent(out) :: dst_y          !< transformed y coordinates in degrees/meters.

      integer             :: error
      character(len=1024) :: message !< Temporary variable for writing log messages.

      ! Copy coordinates to dst_x, dst_y. ! This code assumes that dst_x and dst_y have already been allocated and have the same length as src_x and src_y.
      dst_x = src_x
      dst_y = src_y

      if (pj_is_latlong(src_projection)) then ! If source is spherical coordinate system.
         ! Convert degrees to radians.
         dst_x = dst_x*pj_deg_to_rad
         dst_y = dst_y*pj_deg_to_rad
      end if

      ! Transform coordinates in place in arrays dst_x, dst_y (in radians/meters).
      error = pj_transform_f(src_projection, dst_projection, dst_x, dst_y)
      if (error /= 0) then ! If error.
         ! Put back original coordinates.
         dst_x = src_x
         dst_y = src_y
         write(message, *) 'Error (', error, ') while transforming coordinates.'
         call mess(LEVEL_ERROR, trim(message))
         return
      endif

      if (pj_is_latlong(dst_projection)) then ! If destination is spherical coordinate system.
         ! Convert radians to degrees.
         dst_x = dst_x*pj_rad_to_deg
         dst_y = dst_y*pj_rad_to_deg
      end if
   end subroutine

   !> Transforms the given coordinates from the given source coordinate system to the given destination coordinate system.
   !! This subroutine uses the proj4 library for coordinate transformations.
   subroutine transform_coordinates(src_proj_string, dst_proj_string, src_x, src_y, dst_x, dst_y)
      use proj

      implicit none

      character(len=*),                      intent(in)  :: src_proj_string !< proj4 string describing source coordinate system.
      character(len=*),                      intent(in)  :: dst_proj_string !< proj4 string describing destination coordinate system.
      real(kind=kind(1.0d00)), dimension(:), intent(in)  :: src_x           !< x coordinates to transform in degrees/meters.
      real(kind=kind(1.0d00)), dimension(:), intent(in)  :: src_y           !< y coordinates to transform in degrees/meters.
      real(kind=kind(1.0d00)), dimension(:), intent(out) :: dst_x           !< transformed x coordinates in degrees/meters.
      real(kind=kind(1.0d00)), dimension(:), intent(out) :: dst_y           !< transformed y coordinates in degrees/meters.

      type(pj_object) :: src_projection !< source coordinate system object.
      type(pj_object) :: dst_projection !< destination coordinate system object.

      ! Get projections.
      src_projection = get_projection(src_proj_string)
      dst_projection = get_projection(dst_proj_string)

      call transform(src_projection, dst_projection, src_x, src_y, dst_x, dst_y)

      call pj_free(src_projection)
      call pj_free(dst_projection)
   end subroutine
#endif

end module coordinate_reference_system
