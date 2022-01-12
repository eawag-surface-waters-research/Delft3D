module sed_support_routines
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2022.                                
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
!-------------------------------------------------------------------------------

implicit none

private

public shld
public ruessink_etal_2012

contains

!> determines Shields parameter according to Shields curve
function shld(dstar)
    use precision
    implicit none
!
! arguments
!
    real(fp), intent(in) :: dstar !< critical dimensionless grain size parameter
    real(fp)             :: shld  !< corresponding Shields parameter
!
!! executable statements -------------------------------------------------------
!
    if (dstar <= 4.0_fp) then
       shld = 0.240_fp / dstar
    elseif (dstar <= 10.0_fp) then
       shld = 0.140_fp / dstar**0.64_fp
    elseif (dstar <= 20.0_fp) then
       shld = 0.040_fp / dstar**0.10_fp
    elseif (dstar <= 150.0_fp) then
       shld = 0.013_fp * dstar**0.29_fp
    else
       shld = 0.055_fp
    endif
end function shld

subroutine ruessink_etal_2012(k, hs, h, sk, as, phi_phase, urs, bm)
    use precision
    use mathconsts, only: pi
    implicit none
!
! arguments
!
    real(fp)  , intent(in)  :: k         !< wave number                   [rad/m]
    real(fp)  , intent(in)  :: hs        !< significant wave height       [m]
    real(fp)  , intent(in)  :: h         !< water depth                   [m]

    real(fp)  , intent(out) :: sk        !< skewness                      [-]
    real(fp)  , intent(out) :: as        !< asymmetry                     [-]
    real(fp)  , intent(out) :: phi_phase !< acceration skewness           [rad]
    real(fp)  , intent(out) :: urs       !< Ursell number                 [-]
    real(fp)  , intent(out) :: bm        !< ...
!
! local variables
!
    real(fp)               :: aw
    real(fp)               :: p1
    real(fp)               :: p2
    real(fp)               :: p3
    real(fp)               :: p4
    real(fp)               :: p5
    real(fp)               :: p6
    real(fp)               :: psi
!
!! executable statements -------------------------------------------------------
!
!   parameters
    p1 = 0.0_fp    ! a =  0
    p2 = 0.857_fp  ! b =  0.857 +/- 0.016
    p3 =-0.471_fp  ! c = -0.471 +/- 0.025
    p4 = 0.297_fp  ! d =  0.297 +/- 0.021
    p5 = 0.815_fp  ! e =  0.815 +/- 0.055
    p6 = 0.672_fp  ! f =  0.672 +/- 0.073

!   asymmetry Ruessink et al. (here based on hs and h)
    aw = 0.5_fp*hs
!   asymmetry & skewness based on Ruessink & van Rijn (based on XBeach code) Ursell number (eq. 6)
    if (comparereal(k,0.0_fp) == 0) then
        urs = 0.0_fp
        bm  = 0.0_fp
        psi = 0.0_fp
    else
        urs = 0.75_fp * aw * k / ((k*h)**3)
        urs = max(urs,1e-12_fp)                                             ! Ursell number > 20 -> Use shape with care
!       Boltzmann sigmoid (eq 9)
        bm  = p1 + (p2-p1)/(1.0_fp+exp((p3-log10(urs))/p4))
        psi = 0.5_fp * pi * (tanh(p5/(urs**p6)) - 1.0_fp)
    endif
!   skewness (beteen eq. 8 and 9)
    sk = bm * cos(psi)
!   asymmetry (beteen eq. 8 and 9)
    as = bm * sin(psi)
!   Ruessink et al. (2012), eq. (12)
    phi_phase = -psi - (pi/2.0_fp)
end subroutine ruessink_etal_2012

end module sed_support_routines