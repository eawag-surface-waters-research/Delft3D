module mathconsts
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
   implicit none
   
   !
   ! single precision constants
   !
   real(sp), save :: ee_sp      = exp(1.0_sp)                      !< ee = 2.718281...
   real(sp), save :: pi_sp      = 4.0_sp*atan(1.0_sp)              !< pi = 3.141592...
   real(sp), save :: twopi_sp   = 8.0_sp*atan(1.0_sp)              !< 2pi
   real(sp), save :: sqrt2_sp   = sqrt(2.0_sp)                     !< sqrt(2)
   real(sp), save :: degrad_sp  = 4.0_sp*atan(1.0_sp)/180.0_sp     !< conversion factor from degrees to radians (pi/180)
   real(sp), save :: raddeg_sp  = 180.0_sp/(4.0_sp*atan(1.0_sp))   !< conversion factor from radians to degrees (180/pi)
   real(sp), save :: daysec_sp  = 24.0_sp*60.0_sp*60.0_sp          !< conversion factor from earth day to seconds
   real(sp), save :: yearsec_sp = 365.0_sp*24.0_sp*60.0_sp*60.0_sp !< conversion factor from earth year to seconds (non-leap)
   real(sp), save :: eps_sp     = epsilon(1.0_sp)                  !< epsilon for sp
   
   !
   ! flexible precision constants
   !
   real(fp), save :: ee         = exp(1.0_fp)                      !< ee = 2.718281.../2.71828182845904...
   real(fp), save :: pi         = 4.0_fp*atan(1.0_fp)              !< pi = 3.141592.../3.14159265358979...
   real(fp), save :: twopi      = 8.0_fp*atan(1.0_fp)              !< 2pi
   real(fp), save :: sqrt2      = sqrt(2.0_fp)                     !< sqrt(2)
   real(fp), save :: degrad     = 4.0_fp*atan(1.0_fp)/180.0_fp     !< conversion factor from degrees to radians (pi/180)
   real(fp), save :: raddeg     = 180.0_fp/(4.0_fp*atan(1.0_fp))   !< conversion factor from radians to degrees (180/pi)
   real(fp), save :: daysec     = 24.0_fp*60.0_fp*60.0_fp          !< conversion factor from earth day to seconds
   real(fp), save :: yearsec    = 365.0_fp*24.0_fp*60.0_fp*60.0_fp !< conversion factor from earth year to seconds (non-leap)
   real(fp), save :: eps_fp     = epsilon(1.0_fp)                  !< epsilon for fp
   
   !
   ! high precision constants
   !
   real(hp), save :: ee_hp      = exp(1.0_hp)                      !< ee = 2.71828182845904...
   real(hp), save :: pi_hp      = 4.0_hp*atan(1.0_hp)              !< pi = 3.14159265358979...
   real(hp), save :: twopi_hp   = 8.0_hp*atan(1.0_hp)              !< 2pi
   real(hp), save :: sqrt2_hp   = sqrt(2.0_hp)                     !< sqrt(2)
   real(hp), save :: degrad_hp  = 4.0_hp*atan(1.0_hp)/180.0_hp     !< conversion factor from degrees to radians (pi/180)
   real(hp), save :: raddeg_hp  = 180.0_hp/(4.0_hp*atan(1.0_hp))   !< conversion factor from radians to degrees (180/pi)
   real(hp), save :: daysec_hp  = 24.0_hp*60.0_hp*60.0_hp          !< conversion factor from earth day to seconds
   real(hp), save :: yearsec_hp = 365.0_hp*24.0_hp*60.0_hp*60.0_hp !< conversion factor from earth year to seconds (non-leap)
   real(hp), save :: eps_hp     = epsilon(1.0_hp)                  !< epsilon for hp
   
   contains
      
      !> Obsolete initialization method.
      subroutine init_mathconsts()
          !
      end subroutine init_mathconsts
      
end module mathconsts
