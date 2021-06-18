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

 double precision function Qsum(k)                   ! sum of Q out of k (m3/s)
 use m_flow
 use m_flowgeom
 implicit none

 integer :: k                                        ! for node k,

 ! locals
 integer :: LL, LLL, LLLL                            ! for links LL,

 Qsum = 0d0

 do LL   = 1, nd(k)%lnx                              ! loop over all attached links
    LLL  = nd(k)%ln(LL)
    LLLL = iabs(LLL)

    if ( q1(LLLL) == 0d0 ) then                      ! skip, this is link L itself, net result = 0

    else if (LLL > 0) then                           ! incoming link
       Qsum = Qsum - q1(LLLL)
    else
       Qsum = Qsum + q1(LLLL)
    endif

 enddo

 end function Qsum
