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

 double precision function QucPerpure1D(n12,L)       ! sum of (Q*uc cell centre upwind normal) at side n12 of link L
 use m_flow                                          ! advect the cell center velocities (dimension: m4/s2)
 use m_flowgeom                                      ! leaving the cell = +
 implicit none

 integer :: L                                        ! for link L,
 integer :: n12                                      ! find normal velocity components of the other links

 ! locals
 integer :: LL, L2, L2a                              ! for links LL,
 integer :: k12      , kup                           ! relevant node, 1 or 2, L/R
 double precision ::  ucin, ucinx, uciny, ucupin, cs, sn

 integer :: nn12

 double precision, external:: lin2nodx, lin2nody, nod2linx, nod2liny

 QucPerpure1D = 0d0
 cs           = csu(L)
 sn           = snu(L)

 k12  = ln(n12,L)
 do LL   = 1, nd(k12)%lnx                            ! loop over all attached links
    L2   = nd(k12)%ln(LL)
    L2a  = iabs(L2)

    if ( qa(L2a) == 0d0) then                       ! include own link

    else

       if (nd(k12)%lnx == 2 .and. u1Du(L2a) .ne. 0d0) then
          ! ucin = ucxu(L2a)*cs + ucyu(L2a)*sn
          ucin = isnblin(n12,L) 
          ucin = sign(abs(u1Du(L2a)), ucin) - u1(L)
       else
          ucin = ucxu(L2a)*cs + ucyu(L2a)*sn  - u1(L)
       endif

       if (L2 > 0) then                             ! incoming link
          QucPerpure1D = QucPerpure1D - qa(L2a)*ucin
       else
          QucPerpure1D = QucPerpure1D + qa(L2a)*ucin
       endif

    endif

 enddo

 end function QucPerpure1D
