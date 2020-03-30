!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2020.                                
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
! $HeadURL: 

!> simple_geometry module for subroutines related to simple geometry
module simple_geometry
use netcdf
implicit none
!
! Error codes
!
public :: SGEOM_NOERR


!
! Subroutines
!
public :: sgeom_def_geometry_variables

private
!
! Error statuses
!
integer, parameter :: SGEOM_NOERR         = 0     !< Successful

!-------------------------------------------------------------------------------
   contains
!-------------------------------------------------------------------------------

!> Define geometry related variables, including the geometry container variable and the related coordinates variables
subroutine sgeom_def_geometry_variables(ifile, var_name, geo_type, att_node_count, att_node_coordx, att_node_coordy, dim_name, dim, id_node_count_dim, node_count_long_name, node_coord_long_namex, node_coord_long_namey, id_node_count, id_node_coordx, id_node_coordy, ierr)
   integer,          intent(in   ) :: ifile                 !< The file id
   character(len=*), intent(in   ) :: var_name              !< For geometry container variabel: name
   character(len=*), intent(in   ) :: geo_type              !<                                : type
   character(len=*), intent(in   ) :: att_node_count        !<                                : attribute node_count
   character(len=*), intent(in   ) :: att_node_coordx       !<                                : attribute node_coordinate for x
   character(len=*), intent(in   ) :: att_node_coordy       !<                                : attribute node_coordinate for y   
   character(len=*), intent(in   ) :: dim_name              !< For dimesion of x-/y-coordinate variables: name of the dimension
   integer,          intent(in   ) :: dim                   !<                                          : value of the dimension
   integer,          intent(in   ) :: id_node_count_dim     !< For the node_count variable: dim id (id of instance dim)
   character(len=*), intent(in   ) :: node_count_long_name  !<                            : long name
   character(len=*), intent(in   ) :: node_coord_long_namex !< Long name for node x-coordinate variable
   character(len=*), intent(in   ) :: node_coord_long_namey !< Long name for node y-coordinate variable
   integer,          intent(  out) :: id_node_count         !< Id of node_count variable
   integer,          intent(  out) :: id_node_coordx        !< Id of node x-coordinate variable
   integer,          intent(  out) :: id_node_coordy        !< Id of node y-coordinate variable
   integer,          intent(  out) :: ierr                  !< Result status, ionc_noerr if successful
   
   integer :: id_var, i, id_node_dim
   character(len=256) :: string
   
   ierr = SGEOM_NOERR
   ! Define the dimension of the geometry variables
   ierr = nf90_def_dim(ifile, trim(dim_name), dim, id_node_dim)
   
   ! Define the geometry container variable
   ierr = nf90_def_var(ifile, trim(var_name), nf90_int, id_var)
   ierr = nf90_put_att(ifile, id_var, 'geometry_type', trim(geo_type))
   ierr = nf90_put_att(ifile, id_var, 'node_count', trim(att_node_count))
   string = trim(att_node_coordx) // ' ' //trim(att_node_coordy)
   ierr = nf90_put_att(ifile, id_var, 'node_coordinates', trim(string))
   
   ! Define variable of node_count
   ierr = nf90_def_var(ifile, trim(att_node_count), nf90_int, (/id_node_count_dim/), id_node_count)
   ierr = nf90_put_att(ifile, id_node_count, 'long_name', trim(node_count_long_name))
   
   ! Define variables of node_coordinate
   ierr = nf90_def_var(ifile, trim(att_node_coordx), nf90_double, id_node_dim, id_node_coordx)
   ierr = nf90_put_att(ifile, id_node_coordx, 'long_name', trim(node_coord_long_namex))
   ierr = nf90_put_att(ifile, id_node_coordx, 'units', 'm')
   ierr = nf90_put_att(ifile, id_node_coordx, 'standard_name', 'projection_x_coordinate')
   ierr = nf90_put_att(ifile, id_node_coordx, 'axis', 'X')
   ierr = nf90_put_att(ifile, id_node_coordx, 'cf_role', 'geometry_x_node')
      
   ierr = nf90_def_var(ifile, trim(att_node_coordy), nf90_double, id_node_dim, id_node_coordy)
   ierr = nf90_put_att(ifile, id_node_coordy, 'long_name', trim(node_coord_long_namey))
   ierr = nf90_put_att(ifile, id_node_coordy, 'units', 'm')
   ierr = nf90_put_att(ifile, id_node_coordy, 'standard_name', 'projection_y_coordinate')
   ierr = nf90_put_att(ifile, id_node_coordy, 'axis', 'Y')
   ierr = nf90_put_att(ifile, id_node_coordy, 'cf_role', 'geometry_y_node')

end subroutine sgeom_def_geometry_variables

end module simple_geometry
