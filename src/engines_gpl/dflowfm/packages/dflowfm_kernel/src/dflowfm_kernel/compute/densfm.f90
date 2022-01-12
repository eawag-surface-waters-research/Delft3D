!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2022.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! $Id$
! $HeadURL$

double precision function densfm(sal,temp)
use m_physcoef
use m_flow
double precision           :: sal, temp
double precision, external :: rho_Eckart, rho_Unesco

if (idensform == 0) then               ! Uniform density
    densfm = rhomean
    return
else if (abs(idensform) == 1) then     ! Carl Henry Eckart, 1958
    densfm = rho_Eckart(sal,temp)
else if (abs(idensform) == 2) then     ! Unesco
    densfm = rho_Unesco(sal,temp)
else if (abs(idensform) == 3) then     ! Baroclinic instability
    densfm = 1025d0 + 0.78d0*(sal - 33.73d0)
else if (abs(idensform) == 4) then     ! Test baroclni pressure term 'dicht.mdu'
    densfm = 2d0*rhomean
else if (abs(idensform) == 5) then     ! For Deltares flume experiment IJmuiden , Kees Kuipers saco code 1
    densfm = 999.904d0          + 4.8292d-2*temp - 7.2312d-3*temp**2 + &
             2.9963d-5*temp**3  + 7.6427d-1*sal  -                     &
             3.1490d-3*sal*temp + 3.1273d-5*sal*temp**2
endif
end function densfm
