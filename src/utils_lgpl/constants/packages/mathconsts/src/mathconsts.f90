module mathconsts
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation; either                 
!  version 2.1 of the License.                                                  
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
!!--description-----------------------------------------------------------------
!
! This module defines some general mathematical constants like pi and
! conversion factors from degrees to radians and from (earth) days to
! seconds.
!
! This module does NOT include physical constants like earth radius and
! gravity, or application dependent constants like missing values.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use precision

!
! flexible precision constants
!

real(fp) :: pi      ! pi = 3.141592...
real(fp) :: degrad  ! conversion factor from degrees to radians  
real(fp) :: raddeg  ! conversion factor from radians to degrees
real(fp) :: daysec  ! conversion factor from earth day to seconds
real(fp) :: eps_fp  ! epsilon for fp

!
! high precision constants
!
real(hp) :: pi_hp       ! pi = 3.14159265358979...
real(hp) :: degrad_hp   ! conversion factor from degrees to radians
real(hp) :: raddeg_hp   ! conversion factor from radians to degrees
real(hp) :: daysec_hp   ! conversion factor from earth day to seconds
real(hp) :: yearsec_hp  ! conversion factor from earth year to seconds
real(hp) :: eps_hp      ! epsilon for hp

contains

subroutine init_mathconsts()
    pi         = 4.0_fp*atan(1.0_fp)
    degrad     = pi/180.0_fp
    raddeg     = 180.0_fp/pi
    daysec     = 24.0_fp*60.0_fp*60.0_fp
    eps_fp     = epsilon(1.0_fp)
    !
    pi_hp      = 4.0_hp*datan(1.0_hp)
    degrad_hp  = pi_hp/180.0_hp
    raddeg_hp  = 180.0_hp/dpi
    daysec_hp  = 24.0_hp*60.0_hp*60.0_hp
    yearsec_hp = 31536000.0d0
    eps_hp     = epsilon(1.0_hp)
end subroutine init_mathconsts

end module mathconsts
