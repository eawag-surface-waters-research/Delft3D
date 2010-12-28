subroutine dad_read_polygon_data(polygon_ptr, idcoord, start, number, &
                               & xcoord, ycoord, type, indx, gdp )
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
! Read polygon points
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
    type(tree_data), pointer              :: polygon_ptr
    integer                               :: idcoord
    integer                 , intent(out) :: start
    integer                 , intent(out) :: number
    real(fp), dimension(:)  , intent(out) :: xcoord
    real(fp), dimension(:)  , intent(out) :: ycoord
    character(len=*)        , intent(in)  :: type
    integer                 , intent(in)  :: indx
!
! Local variables
!
    integer                                 :: ip
    integer , dimension(1:2)                :: inputivals
    real(fp), dimension(1:2)                :: inputvals
    real(fp)                                :: misvalue
    character(len=1), pointer, dimension(:) :: data_ptr
    character(len=30)                       :: node_type
    character(len=30)                       :: parname
    character(len=200)                      :: message
    type(tree_data), pointer                :: node_ptr
!
!! executable statements -------------------------------------------------------
!
    lundia  => gdp%gdinout%lundia
    !
    ! Record start and number of polygon points
    !
    misvalue = -999.999
    start    = idcoord
    number   = -1
    call tree_get_data_ptr( polygon_ptr, data_ptr, node_type )
    inputivals = transfer( data_ptr, inputivals )
    number     = inputivals(1)
    if (number == -1) then
       write(message,'(a,a,i0)') 'Unable to read the number of points in ', &
            &                    trim(type), ' polygon of area',indx
       call prterr(lundia, 'U021', message)
       call d3stop(1, gdp)
    endif
    !write (lundia,'(a,i0,a,i0)') 'Number of points in '//trim(type)// &
    !      &                      ' polygon of area ', indx,': ',number
    !
    ! read the polygon points
    !
    do ip = 1, number
       inputvals = misvalue
       write (parname,'(a,i0)')'row_',ip
       call tree_get_node_by_name( polygon_ptr, parname, node_ptr )
       call tree_get_data_ptr( node_ptr, data_ptr, node_type )
       !
       ! inputvals is of type real(fp)
       ! the data to be retrieved is in real(sp)
       ! call transfer with a real(sp) constant as second parameter
       !
       inputvals = transfer( data_ptr, 0., 2 )
       if (  comparereal(inputvals(1),misvalue) == 0 .or. &
           & comparereal(inputvals(2),misvalue) == 0        ) then
          write(message,'(a,i0,a,i0)') 'Unable to read '//trim(type)// &
               &                       ' polygon point ', ip,' of area ',indx
          call prterr(lundia, 'U021', message)
          call d3stop(1, gdp)
       endif
       xcoord(start+ip-1) = inputvals(1)
       ycoord(start+ip-1) = inputvals(2)
       !write(lundia,'(a,i3,a,i3,a,f13.5,f13.5)') 'Point ',ip,', &
       !     &   '//trim(type)//' area ',indx,':',xcoord(start+ip-1), ycoord(start+ip-1)
       idcoord = idcoord + 1
    enddo
end subroutine dad_read_polygon_data
