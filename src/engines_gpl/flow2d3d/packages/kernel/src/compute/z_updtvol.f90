subroutine z_updtvol(nmmax     ,kmax      ,kcs       , gsqs      ,dzs1      , &
                   & volum1    ,gdp       ) 
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
! Update volumes
! (to maintain compatibility with Delft3D-WAQ)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
!
! Global variables
!
    integer                                     , intent(in)  :: kmax
    integer                                     , intent(in)  :: nmmax
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kcs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: gsqs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzs1
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: volum1
!
! Local variables
!
    integer :: k
    integer :: nm
!
!! executable statements -------------------------------------------------------
!
do k = 1, kmax
   do nm = 1, nmmax
      volum1(nm, k) = dzs1(nm, k)*gsqs(nm)*min(1,kcs(nm))
   enddo
enddo
end subroutine z_updtvol
