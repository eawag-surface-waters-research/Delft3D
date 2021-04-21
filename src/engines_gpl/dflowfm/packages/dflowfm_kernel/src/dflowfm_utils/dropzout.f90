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

 subroutine dropzout(idir)
 use m_polygon
 use m_flowgeom
 use m_flow
 use m_missing, only: dmiss, jins
 use geometry_module, only: dbpinpol
 implicit none
 integer,          intent(in) :: idir !< direction (1 for up, -1 for down)

 ! locals
 integer           :: n, nn, in, ncol, k, kb, kt
 double precision :: dropstep, s10

 if (ndx == 0) return

 dropstep = idir*sdropstep

 if (npl > 2) then
    in   = -1
    do n = 1,ndx
       CALL DBPINPOL( xz(n), yz(n), IN, dmiss, jins, NPL, xpl, ypl, zpl)
       if (in == 1) then
          call getkbotktop(n,kb,kt)
          if (idir == 1) then
             kb = kb + kplot - 1
          endif
          do k = kb,kt
             sam1tot = sam1tot - sa1(k)*vol0(k)
             sa1(k)  = max(0d0, sa1(k) + dropstep)
             sam1tot = sam1tot + sa1(k)*vol1(k)
             call isocol(sa1(n),ncol)
             nn = size( nd(n)%x )
             call pfiller(nd(n)%x, nd(n)%y, nn, ncol, 30)
          enddo
       endif
    enddo

 else

    n = nplot
    call getkbotktop(n,kb,kt)
    k = kb + kplot - 1
    sam1tot = sam1tot - sa1(k)*vol0(k)
    sa1(k)  = max(0d0, sa1(k) + dropstep)
    sam1tot = sam1tot + sa1(k)*vol1(k)
    call isocol(sa1(n),ncol)
    nn = size( nd(n)%x )
    call pfiller(nd(n)%x, nd(n)%y, nn, ncol, 30)
 endif

 if (kmx > 0) then
    call setkbotktop(1) ! drop
 endif

 end subroutine dropzout