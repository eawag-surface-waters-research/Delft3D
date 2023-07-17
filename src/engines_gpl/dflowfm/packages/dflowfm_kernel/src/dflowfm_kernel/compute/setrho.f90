!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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

! 
! 

subroutine setrhokk(kk)    ! fill rho of one column  
use m_flow, only : rho, idensform, kmxn
implicit none
integer :: kk 
integer :: kb,kt,k
 
double precision, external :: setrho 
double precision           :: p0

call getkbotktop(kk,kb,kt)
if (kt<kb) return

if (idensform < 10) then  ! no pressure dependency 
   do k = kb,kt
      rho(k)  = setrho(k,p0)
   enddo
else                      ! with pressure dependency
   p0 = 0d0               ! surface value is 0 bar in unesco, not 1 bar
   do k = kt,kb,-1
      rho(k)  = setrho(k,p0)
   enddo
endif

do k = kt+1 , kb + kmxn(kk) - 1
   rho(k) = rho(kt)
enddo
end subroutine setrhokk

double precision function setrho(k,p0)
use m_physcoef
use m_flow
use m_sediment
use sediment_basics_module, only: has_advdiff
use m_transport
use m_turbulence, only: rhowat

implicit none
integer          :: k
double precision :: p0           ! in as cell ceiling pressure, out as cell floorpressure (pascal) 
double precision :: rhok         ! in as previous density, reduces required nr of iterations 

integer                         :: j, l, lsed, i    
double precision, external      :: densfm
double precision                :: rhom, sal, temp, p1, dzz

call getsaltemk(k,sal, temp)

if (idensform < 10) then 
   setrho = densfm(sal,temp,p0)
else 
   dzz  = zws(k) - zws(k-1)
   rhok = rho(k)
   do i  = 1,Maxitpresdens 
      p1 = p0 + ag*dzz*rhok
      rhok =  densfm(sal,temp,0.5d0*(p1+p0) )
   enddo
   setrho = rhok
   p0     = p1
endif

if (jased > 0 .and. stm_included) then
   rhom = setrho                     ! UNST-5170 for mor, only use salt+temp, not sediment effect
   rhom = min(rhom, 1250d0)           ! check overshoots at thin water layers
   rhom = max(rhom,  990d0)           !
   rhowat(k) = rhom
   if (stmpar%morpar%densin) then     ! sediment density effects
      l = ISED1
      rhom = setrho
      do lsed = 1,stmpar%lsedtot
         if (has_advdiff(stmpar%sedpar%tratyp(lsed))) then ! has suspended component
            setrho = setrho + constituents(l,k)*(stmpar%sedpar%rhosol(lsed) - rhom)/stmpar%sedpar%rhosol(lsed)
            l = l+1
         end if
      end do
   end if
else if (jaseddenscoupling > 0) then  ! jased < 4
   rhom = setrho
   do j = 1,mxgr
      setrho = setrho + sed(j,k)*(rhosed(j) - rhom)/rhosed(j) ! good to see this is also adopted officially above %
   enddo
end if

setrho = min(setrho, 1250d0)          ! check overshoots at thin water layers
setrho = max(setrho,  990d0)          !

end function setrho

double precision function setrhofixedp(k,p0)

implicit none
integer          :: k
double precision :: p0           ! some given pressure 
double precision, external      :: densfm

double precision :: sal, temp

call getsaltemk(k,sal, temp)

setrhofixedp = densfm(sal,temp,p0)

end function setrhofixedp


subroutine getsaltemk(k,sal, temp)
use m_flow
use m_transport

implicit none
integer          :: k
double precision :: sal, temp 

if (jasal > 0) then
   saL = max(0d0, constituents(isalt, k))
else
   saL = backgroundsalinity
endif

if (jatem > 0) then
   temp = max(0d0, constituents(itemp,k))  ! should be changed to -2d0 at some point in future
else
   temp = backgroundwatertemperature
endif
end subroutine getsaltemk
