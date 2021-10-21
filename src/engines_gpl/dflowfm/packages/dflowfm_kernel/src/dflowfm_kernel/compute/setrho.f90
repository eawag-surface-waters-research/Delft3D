!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2021.                                
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

double precision function setrho(k)
use m_physcoef
use m_flow
use m_sediment
use sediment_basics_module, only: SEDTYP_NONCOHESIVE_TOTALLOAD
use m_transport
use m_turbulence, only: rhowat

implicit none
integer :: k, j, l, lsed
double precision, external :: densfm
double precision           :: rhom, sal, temp

if (jasal > 0) then
   saL = max(0d0, constituents(isalt, k))
else
   saL = backgroundsalinity
endif

if (jatem > 0) then
   temp = max(0d0, constituents(itemp,k))
else
   temp = backgroundwatertemperature
endif

setrho = densfm(sal,temp)

if (jased > 0 .and. stm_included) then
   rhom = setrho                      ! UNST-5170 for mor, only use salt+temp, not sediment effect
   rhom = min(rhom, 1250d0)           ! check overshoots at thin water layers
   rhom = max(rhom,  990d0)           !
   rhowat(k) = rhom
   if (stmpar%morpar%densin) then     ! sediment effects
      l = ISED1
      rhom = setrho
      do lsed = 1,stmpar%lsedtot
         if (stmpar%sedpar%sedtyp(lsed) /= SEDTYP_NONCOHESIVE_TOTALLOAD) then  ! suspended sand or mud
            setrho = setrho + constituents(l,k)*(stmpar%sedpar%rhosol(lsed) - rhom)/stmpar%sedpar%rhosol(lsed)
            l = l+1
         end if
      end do
   end if
else if (jaseddenscoupling > 0) then  ! jased < 4
   rhom = setrho
   do j = 1,mxgr
      setrho = setrho + sed(j,k)*(rhosed(j) - rhom)/rhosed(j)
   enddo
end if

setrho = min(setrho, 1250d0)          ! check overshoots at thin water layers
setrho = max(setrho,  990d0)          !

end function setrho
