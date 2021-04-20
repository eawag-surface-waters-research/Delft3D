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

 !> Gets the tangential direction vector for a 1D flow node,
 !! based on direction of the last regular 1D link connected to it.
 !! NOTE: only makes sense when there's only one single regular 1D
 !! link connected (i.e., 1D endpoint, maybe connected with kcu=3
 !! type link to 2D grid cell).
 subroutine get1Ddir(n1,xt,yt)
 use m_flowgeom
 use m_flow
 use m_sferic, only : jsferic, jasfer3D
 use m_missing, only :dxymis
 use geometry_module, only: normalin

 implicit none
 integer          :: n1, n2, k, L, LL, ka, kb, k1, k2
 double precision :: xt, yt

 xt = 0d0 ; yt = 0d0; ka = 0; kb = 0
 do k = 1, size(nd(n1)%ln)
    LL  = nd(n1)%ln(k)
    L   = iabs(LL)
    if (kcu(L) .ne. 3) then
       k1 = ln(1,L) ; k2 = ln(2,L)
       n2 = k2
       if (k1 .ne. n1) n2 = k1
       if (ka == 0) then
          ka = n2
       else
          kb = n2
       endif
    endif
 enddo
 if (kb == 0) kb = n1

 call normalin(xz(n1), yz(n1), xz(n2), yz(n2), xt, yt, xu(L), yu(L), jsferic, jasfer3D, dxymis)
 end subroutine get1Ddir
