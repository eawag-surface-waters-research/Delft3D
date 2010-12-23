subroutine dad_register_polygon(link_ptr, pol_ptr, idcount, totpoints, &
                              & keyword, type, mustbeunique, gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
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
!!--description-----------------------------------------------------------------
!
! Search for a polygon in a tree, given it's name. Stop with an error message
!    if it is not found.
! If the polygon does not has an id, add a unique one.
! If the polygon already has an id, and the usage of the polygon should be
!    unique, stop with an error message.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
    integer , pointer :: lundia
!
! Global variables
!
    type(tree_data), pointer              :: link_ptr
    type(tree_data), pointer              :: pol_ptr
    integer                               :: idcount
    integer                               :: totpoints
    character(len=*)        , intent(in)  :: keyword
    character(len=*)        , intent(in)  :: type
    logical                 , intent(in)  :: mustbeunique
!
! Local variables
!
    integer                                     :: id
    integer           , dimension(1:1)          :: values
    character(len=1)  , dimension(:)  , pointer :: data_ptr
    character(len=10)                           :: idstring
    character(len=200)                          :: message
    character(len=80)                           :: name
    character(len=80)                           :: node_type
    type(tree_data)                   , pointer :: polygon_ptr
    type(tree_data)                   , pointer :: node_ptr
!
!! executable statements -------------------------------------------------------
!
    lundia  => gdp%gdinout%lundia
    !
    ! Initialize
    !
    values = 0
    !
    ! Get the name of the requested polygon
    !
    name = ''
    call prop_get_string(link_ptr, '*', keyword, name)
    !
    ! Get corresponding polygon node
    !
    call tree_get_node_by_name(pol_ptr, name, polygon_ptr )
    if ( .not. associated(polygon_ptr) ) then
       write (message,'(3a)') 'polygon ', trim(name),' is missing'
       call prterr(lundia, 'U021', trim(message), gdp)
       call d3stop(1, gdp)
    endif
    !
    ! Create the polygon's ID
    !
    id = 0
    call prop_get_integer(polygon_ptr, '*', trim(type)//'id', id)
    if (id == 0) then
       idcount = idcount + 1
       !
       ! This is a new area ... add the ID
       !
       call tree_create_node( polygon_ptr, trim(type)//'id', node_ptr )
       write(idstring,'(i0)') idcount
       call tree_put_data( node_ptr, transfer(trim(idstring),node_value), 'STRING' )
       !
       ! Get number of points in this polygon
       !
       call tree_get_data_ptr( polygon_ptr, data_ptr, node_type )
       values = transfer( data_ptr, values )
    else
       if ( mustbeunique ) then
          write (message,'(3a)') trim(type)//' area ', trim(name), &
                                 ' is specified more than once in dad-file'
          call prterr(lundia, 'U021', trim(message), gdp)
          call d3stop(1, gdp)
       endif
    endif
    totpoints = totpoints + values(1)
end subroutine dad_register_polygon
