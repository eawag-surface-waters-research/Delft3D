subroutine get_real_prop(link_ptr, keyword, text, value, id, gdp )
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
! Reads a real from the input tree.
! Stops with an error message if it fails
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
   character(len=*)        , intent(in)  :: keyword
   character(len=*)        , intent(in)  :: text
   real(fp)                , intent(out) :: value
   integer                 , intent(in)  :: id
!
! Local variables
!
    real(fp)          :: misvalue
    character(80)     :: message
!
!! executable statements -------------------------------------------------------
!
    lundia  => gdp%gdinout%lundia
    !
    misvalue = -999.999
    value    = misvalue
    call prop_get(link_ptr, '*', keyword, value)
    if (comparereal(value,misvalue) == 0) then
       write(message,'(a,i0)') 'Unable to read '//trim(text)// ' ', id
       call prterr(lundia, 'U021', message, gdp)
       call d3stop(1, gdp)
    endif
end subroutine get_real_prop
