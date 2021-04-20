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

 subroutine getdxofconnectedkcu1(Lf,wuL)    ! width of connection link has lenght of connected 1D links
 use m_flowgeom
 use m_flow
 use m_netw
 implicit none
 integer          :: Lf, L, LL, k, kk, n, k1, k2, n1, n2
 double precision :: wu1, wu2, wuL
 wu1 = 0d0 ; n = 0

 !if (kcs(ln(1,L) ) == 21)  k = ln(2,L)
 !if (kcs(ln(2,L) ) == 21)  k = ln(1,L)
 !do kk = 1,nd(k)%lnx
 !   if (kcu(L) == 1) then
 !      n   = n + 1
 !      LL  = iabs( nd(k)%ln(kk) )
 !      wu1 = wu1 + dx(L)
 !   endif
 ! enddo

 L  = ln2lne(Lf)
 k1 = kn(1,L)
 k2 = kn(2,L)
 if (nmk(k1) == 1) k = k2
 if (nmk(k2) == 1) k = k1
 do kk = 1, nmk(k)
    LL = iabs(nod(k)%lin(kk))
    if (kn(3,LL) == 1 .or. kn(3,LL) == 6) then    ! on second thought, only true 1D links should influence lateral inflow width ! .or. kn(3,LL) == 4) then
        n   = n + 1
        wu1 = wu1 + dx(lne2ln(LL))
    endif
 enddo

 if (n > 0) then
    wu1 = wu1 / n
 endif

 n1 = ln(1,Lf) ; n2 = ln(2,Lf)
 if (kcs(n1) == 21) then
    wu2 = sqrt(ba(n1))
 else if (kcs(n2) == 21) then
    wu2 = sqrt(ba(n2))
 endif

 wuL = min (wu1, wu2)     ! both 1D sides flood at the same moment, no division by 2
 end subroutine getdxofconnectedkcu1
