subroutine rdmor0(ilun      ,morfac    ,tmor      ,thresh    ,morupd    , &
                & eqmbc     ,densin    ,aksfac    ,rwave     ,rouse     , &
                & alfabs    ,alfabn    ,sus       ,bed       ,susw      , &
                & bedw      ,sedthr    ,thetsd    ,hmaxth    ,fwfac     )
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
!  $Id$
!  $HeadURL$
!!--description-----------------------------------------------------------------
!
! Reads morphology input version 0 (or no version number found)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    integer, intent(in)  :: ilun
    logical, intent(out) :: densin !  Description and declaration in morpar.igs
    logical, intent(out) :: eqmbc !  Description and declaration in morpar.igs
    logical, intent(out) :: morupd !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: aksfac !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: alfabn !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: alfabs !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: bed !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: bedw !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: fwfac !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: hmaxth !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: morfac !  Description and declaration in morpar.igs
    logical, intent(out)    :: rouse !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: rwave !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: sedthr !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: sus !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: susw !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: thetsd !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: thresh !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: tmor !  Description and declaration in morpar.igs
!
!
!! executable statements -------------------------------------------------------
!
    ! morphological timescale factor
    read (ilun, *) morfac
    ! start for calculating morphological changes
    read (ilun, *) tmor
    ! calculation of ITMOR has been moved to TRICOM
    ! threshold value for slowing erosion near a fixed layer (m)
    read (ilun, *) thresh
    ! flag for doing bottom updates
    read (ilun, *) morupd
    ! flag for setting equilibrium sediment concentration profiles
    ! at the open boundaries for sand sediment
    read (ilun, *) eqmbc
    ! flag for including sediment in fluid density calculations
    read (ilun, *) densin
    ! factor for setting aks height
    read (ilun, *) aksfac
    ! factor for calculating wave-related roughness from ripple dimensions
    read (ilun, *) rwave
    ! flag for setting equilibrium concentrations to standard Rouse profiles
    read (ilun, *) rouse
    ! factor for longitudinal bed load transport
    read (ilun, *) alfabs
    ! factor for transverse bed load transport
    read (ilun, *) alfabn
    ! factor for calculating suspended load transport
    read (ilun, *) sus
    ! factor for calculating bed load transport
    read (ilun, *) bed
    ! wave-related suspended sediment factor
    read (ilun, *) susw
    ! wave-related bed-load sediment factor
    read (ilun, *) bedw
    ! minimum depth for sediment calculations
    read (ilun, *) sedthr
    ! global / maximum dry cell erosion factor
    read (ilun, *) thetsd
    ! maximum depth for variable dry cell erosion factor
    read (ilun, *) hmaxth
    ! factor for adjusting intensity of energy dissipation in wave boundary layer
    read (ilun, *) fwfac
end subroutine rdmor0
