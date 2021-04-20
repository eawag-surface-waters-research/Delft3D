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

 subroutine which2Dnetlinkwascrossed(NC1,K1,K2,L) ! find the crossed 2D link
 use m_flowgeom
 use m_netw
 use geometry_module, only: cross
 use m_missing, only: dmiss
 use m_sferic, only: jsferic

 implicit none
 integer          :: NC1,K1,K2,LL
 integer          :: nn,kk,kku,jacros,k3,k4,L
 double precision :: SL,SM,XCR,YCR,CRP

 LL = 0
 nn = NETCELL(nc1)%N

 do kk  = 1,nn
    L   = NETCELL(Nc1)%lin(kk)
    K3  = kn(1,L)
    K4  = kn(2,L)

    call CROSS(xk(k1), yk(k1), xk(k2), yk(k2), xk(k3), yk(k3),  xk(k4), yk(k4), JACROS,SL,SM,XCR,YCR,CRP, jsferic, dmiss)
    if (jacros == 1) then
       LL = L
       return
    endif
 enddo

 end subroutine which2Dnetlinkwascrossed ! TEMP STORE CROSSED 2d LINK IN LC
