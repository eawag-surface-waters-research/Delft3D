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

subroutine connecthangingnodes()
use m_netw
use m_flowgeom
use m_missing
use gridoperations
implicit none

integer :: mout, np, ih, kk, k, kk3, kkx, k1,k2,lnu, km, kp, i

call findcells(0)
call newfil(mout, 'hang.xyz')

lnu =numL
do np  = 1,nump
   kk3 = 0
   kkx = netcell(np)%n
   if (kkx <= 4 .or. kkx == 6) then
      cycle
   endif
   do kk = 1,netcell(np)%n
      k  =netcell(np)%nod(kk)
      if (nmk(k) == 3) then
         km = kk - 1; if (km < 1  ) km = km + kkx
         kp = kk + 1; if (kp > kkx) kp = kp - kkx
         km = netcell(np)%nod(km)
         kp = netcell(np)%nod(kp)
         if (yk(km) == yk(k) .and. yk(kp) == yk(k) ) then
            km  = kk - 2; if (km < 1)   km = km + kkx
            kp  = kk + 2; if (kp > kkx) kp = kp - kkx
            km  = netcell(np)%nod(km)
            kp  = netcell(np)%nod(kp)
            lnu = lnu + 1
            kn(1,lnu) = k ; kn(2,lnu) = km; kn(3,lnu) = 2
            lnu = lnu + 1
            kn(1,lnu) = k ; kn(2,lnu) = kp; kn(3,lnu) = 2
            !call connectdbn(k,km,lnu)
            !call connectdbn(k,kp,lnu)
         endif
      endif
   enddo
enddo
numL = Lnu
call doclose(mout)
call findcells(0)

end subroutine connecthangingnodes
