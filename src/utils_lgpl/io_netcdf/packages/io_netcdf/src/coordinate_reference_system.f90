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

!  $Id$
!  $HeadURL$

!> Module for utility types and functions for working with coordinates in different coordinate systems.
module coordinate_reference_system
   implicit none

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
      logical                         :: is_spherical  !< Whether or not spherical (otherwise some projected crs).
      character(len=64)               :: varname = ' ' !< Name of the NetCDF variable containing this CRS
      integer                         :: epsg_code     !< EPSG code (more info: http://spatialreference.org/)
      type(nc_attribute), allocatable :: attset(:)     !< General set with all/any attributes about this CRS.
   end type t_crs

contains

end module coordinate_reference_system
