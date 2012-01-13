subroutine dfexchg ( iptr, ks, ke, itype, gdp )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
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
!  $Id$
!  $HeadURL$
!!--description-----------------------------------------------------------------
!
!   Exchanges halo values of field array between neighbouring subdomains
!
!!--pseudo code and references--------------------------------------------------
!
!   if not parallel, return
!   actual update of field array based on its type
!
!
!!--declarations----------------------------------------------------------------
    use dfparall
    use globaldata
    !
    implicit none
    !
    type(globdat), target    :: gdp
!
! Global variables
!
    integer, intent(inout) :: iptr  ! pointer to first element of field array
    integer, intent(in)    :: itype ! type of data
    integer, intent(in)    :: ke    ! last index in vertical direction
    integer, intent(in)    :: ks    ! first index in vertical direction
!
! Local variables
!
    integer, pointer :: lundia
    character(80) :: msgstr ! string to pass message
!
!! executable statements -------------------------------------------------------
!
    !
    ! if not parallel, return
    !
    if (.not.parll) return
    !
    lundia => gdp%gdinout%lundia
    !
    ! actual update of field array based on its type
    !
    if ( itype == dfint ) then
       call dfupdi ( iptr, ks, ke, gdp )
    elseif ( itype == dfreal ) then
       call dfupdr ( iptr, ks, ke, gdp )
    elseif ( itype == dfdble ) then
       call dfupdd ( iptr, ks, ke, gdp )
    else
       write (msgstr,'(a,i3)') 'Unknown type of field array to be update: ',itype
       call prterr(lundia, 'U021', trim(msgstr))
       call d3stop(1, gdp)
    endif

end subroutine dfexchg
