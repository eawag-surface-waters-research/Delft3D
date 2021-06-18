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

 subroutine sortnetlinks()
 use m_netw
 use sorting_algorithms, only: indexxi
 implicit none
 integer, allocatable :: n1(:), in(:), ni(:)
 integer              :: L1, k, kk, L

 if (numL == 0) return

 allocate ( n1(numL), in(numL), ni(numL) )

 L1   = numl1D+1
 do L = L1, numL
    n1(L) = lne(1,L)
 enddo

 call indexxi(numL-L1+1, n1(L1:), IN(L1:) )

 do L = 1,numl1D
    in(L) = L
 enddo

 do L = L1, numL
   in(L) = in(L) + numL1D
 enddo

 do L = 1,numL
    ni(in(L)) = L
 enddo

 do L = L1, numL
    n1(L)    = lne(1,L)
 enddo
 do L = L1, numL
    lne(1,L) = n1(in(L))
 enddo

 do L = L1, numL
    n1(L)   = lne(2,L)
 enddo
 do L = L1, numL
    lne(2,L) = n1(in(L))
 enddo

  do L = L1, numL
    n1(L)    = kn(1,L)
 enddo
 do L = L1, numL
    kn(1,L) = n1(in(L))
 enddo

 do L = L1, numL
    n1(L)   = kn(2,L)
 enddo
 do L = L1, numL
    kn(2,L) = n1(in(L))
 enddo

 do L = L1, numL
    n1(L)   = kn(3,L)
 enddo
 do L = L1, numL
    kn(3,L) = n1(in(L))
 enddo

 do L = L1, numL
    n1(L)  = lnn(L)
 enddo
 do L = L1, numL
    lnn(L) = n1(in(L))
 enddo

 do k = 1,numk
    do kk = 1,nmk(k)
       NOD(K)%LIN(kk) = ni(NOD(K)%LIN(kk))
    enddo
 enddo

 do k = 1,nump
    do kk = 1,netcell(k)%n
       netcell(K)%LIN(kk) = ni(netcell(K)%LIN(kk))
    enddo
 enddo

 deallocate(n1, in, ni)

 end  subroutine sortnetlinks
