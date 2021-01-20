subroutine santoss_rrr12(g, hs, tp, d, sk, as, phi_ab, r_ab, ur, km)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2021.                                
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
!   The SANTOSS practical sand transport model, version 2.08  
!   Computations of the orbital motions in the nearshore morphodynamical model
!   using the formula of Abreu et al. (2010)
!
!   Computation of parameterization of wave orbital motion
!   by Ruessink et al. (2012)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts, only: pi
    implicit none
!
! call variables
!
    real(fp)  , intent(in)  :: g       ! gravity acceleration          [m/s^2]
    real(fp)  , intent(in)  :: hs      ! significant wave height       [m]
    real(fp)  , intent(in)  :: tp      ! peak wave period              [s]
    real(fp)  , intent(in)  :: d       ! water depth                   [m]
!
    real(fp)  , intent(out) :: sk      ! skewness                      [-]
    real(fp)  , intent(out) :: as      ! asymmetry                     [-]
    real(fp)  , intent(out) :: phi_ab  ! acceration skewness for abreu [rad]
    real(fp)  , intent(out) :: r_ab    ! skewness parameter for abreu  [-]
    real(fp)  , intent(out) :: ur      ! Ursell number                 [-]
    real(fp)  , intent(out) :: km      ! wave number                   [rad/m]
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
    real(fp)               :: k
    real(fp)               :: bm
    real(fp)               :: psi
    real(fp)               :: b
    real(fp)               :: omega
    real(fp)               :: k0
    real(fp)               :: k1
    real(fp)               :: c1
    real(fp)               :: s1
    real(fp)               :: a1
    real(fp)               :: a2
    real(fp)               :: a3
!
!! executable statements -------------------------------------------------------
!
!   determine wave number (k)
    if (comparereal(tp,0.0_fp)==0) then
        omega=0.0_fp
    else
        omega=2.0_fp*pi/tp
    endif
    k0 = omega*omega/g
    if (d>3.0_fp*tp*tp) then                    ! deep water
       k = k0
    elseif (k0*d/(2.0_fp*pi) < 0.0001_fp) then  ! shallow water
       k = 2.0_fp*pi/(sqrt(g*d)*tp)
    else                                        ! intermediate water
       k1 = k0/sqrt(tanh(k0*d))
       s1 = sinh(k1*d)
       c1 = cosh(k1*d)
       a1 = d/(c1*c1)-k1*d*d/(c1*c1*c1)*s1
       a2 = s1/c1+k1*d/(c1*c1)
       a3 = k1*s1/c1-k0
       k  = k1+(-a2+sqrt(a2*a2-4.*a1*a3))/(2.0_fp*a1)
    endif
    km = k

!   parameters
    p1 = 0.0_fp    ! a =  0
    p2 = 0.857_fp  ! b =  0.857 +/- 0.016
    p3 =-0.471_fp  ! c = -0.471 +/- 0.025
    p4 = 0.297_fp  ! d =  0.297 +/- 0.021
    p5 = 0.815_fp  ! e =  0.815 +/- 0.055
    p6 = 0.672_fp  ! f =  0.672 +/- 0.073

!   asymmetry Ruessink et al. (here based on hs, tp and d)
    aw = 0.5_fp*hs
!   asymmetry & skewness based on Ruessink & van Rijn (based on XBeach code) Ursell number (eq. 6)
    if (comparereal(k,0.0_fp)==0) then
        ur  = 0.0_fp
        bm  = 0.0_fp
        psi = 0.0_fp
    else
        ur  = 0.75_fp*aw*k/((k*d)**3)
!       Boltzmann sigmoid (eq 9)
        bm  = p1+(p2-p1)/(1.0_fp+exp((p3-log10(ur))/p4))
        psi = 0.5_fp*pi*(tanh(p5/(ur**p6))-1.0_fp)
    endif
!   skewness (beteen eq. 8 and 9)
    sk = bm*cos(psi)
!   asymmetry (beteen eq. 8 and 9)
    as = bm*sin(psi)
!   Ruessink et al. (2012), eq. (12)
    phi_ab = -psi-(pi/2)
!   Veen (2014), eq. (B.13)
    r_ab   = 0.0517_fp*bm**3 - 0.4095_fp*bm**2 + 1.0853_fp*bm - 0.0099_fp
end subroutine santoss_rrr12
