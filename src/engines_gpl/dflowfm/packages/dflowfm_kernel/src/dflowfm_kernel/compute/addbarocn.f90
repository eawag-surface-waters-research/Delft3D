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

 subroutine addbarocn(n)
 use m_flowgeom
 use m_flow

 implicit none
 integer, intent(in) :: n

 integer             :: k,kb,kt
 double precision    :: rhosw(0:kmxx) ! rho at pressure point layer interfaces
 double precision    :: fzu , fzd, alf, pu, pd, gr, dzz, rvn

 call getkbotktop(n,kb,kt)
 ! if (kt < kb) return
 if (zws(kt) - zws(kb-1) < epshu) then
     grn(kb:kt)  = 0d0
     rvdn(kb:kt) = 1d-10
     return
 endif

 if (kt > kb) then
    do k = kb, kt-1
       fzu           = (zws(k+1) - zws(k)) / (zws(k+1) - zws(k-1)) ; fzd = 1d0 - fzu
       rhosw(k-kb+1) = fzu*rho(k+1) + fzd*rho(k) - rhomean
    enddo
    rhosw(0)       = 2d0*(rho(kb) - rhomean) - rhosw(1)
    rhosw(kt-kb+1) = 2d0*(rho(kt) - rhomean) - rhosw(kt-kb)
 else
    rhosw(0)       = rho(kb) - rhomean
    rhosw(1)       = rhosw(0)
 endif

 grn(kt)  = 0d0
 rvdn(kt) = 0d0
 pd       = 0d0
 rvn      = 0d0
 do k = kt, kb, -1
    dzz     = zws(k) - zws(k-1)
    rvn     = rvn + 0.5d0*( rhosw(k-kb+1) + rhosw(k-kb) ) * dzz
    rvdn(k) = rvn
    alf = rhosw(k-kb) - rhosw(k-kb+1)
    pu  = pd
    pd  = pu + rhosw(k-kb+1)*dzz + 0.5d0*alf*dzz
    gr  = pu*dzz + 0.5d0*rhosw(k-kb+1)*dzz*dzz + alf*dzz*dzz/6d0             ! your left  wall
    grn(k) = gr
 enddo

 end subroutine addbarocn
