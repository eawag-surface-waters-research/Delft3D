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

subroutine remove_unused_nodes_and_links()
 use m_netw
 use m_flowgeom
 use m_missing, only : xymis

 logical, dimension(:), allocatable :: nod_used
 integer, dimension(:), allocatable :: k2knew
 integer, dimension(:), allocatable :: l2lnew
 logical, dimension(:), allocatable :: lin_used
 
 integer :: i
 integer :: ic
 integer :: knew
 integer :: l
 integer :: l2
 integer :: numk_new
 integer :: numl_new
 
 allocate(nod_used(numk))
 allocate(lin_used(numl))
 
 nod_used = .false.
 do ic = 1, nump
    do i = 1,netcell(ic)%n
        nod_used(netcell(ic)%nod(i)) = .true.
    enddo
 enddo
 
 lin_used = .false.
 do l = 1, numl
     if (nod_used(kn(1,l)) .and. nod_used(kn(2,l))) then
         lin_used(l) = .true.
     endif
 enddo

 allocate(k2knew(numk))
 k2knew = 0
 knew = 0
 do k = 1, numk
    if (nod_used(k)) then
       knew = knew+1
       k2knew(k) = knew
    endif
 enddo
 numk_new = knew
 
 allocate(l2lnew(numl))
 l2lnew = 0
 lnew = 0
 do l = 1, numl
    if (lin_used(l)) then
       lnew = lnew+1
       l2lnew(l) = lnew
    endif
 enddo
 numl_new = lnew
 
 do k = 1, numk
    knew = k2knew(k)
    if (knew > 0) then
       l2 = 0
       do l = 1,nmk(k)
           lnew = l2lnew(nod(k)%lin(l))
           if (lnew > 0) then
               l2 = l2 + 1
               nod(k)%lin(l2) = lnew
           endif
       enddo
       if (l2 < nmk(k)) then
           call realloc(nod(k)%lin, l2)
           nmk(k) = l2
       endif
       !
       if (knew < k) then
          xk(knew) = xk(k)
          yk(knew) = yk(k)
          kc(knew) = kc(k)
          !
          nmk(knew) = nmk(k)
          call realloc(nod(knew)%lin, nmk(k))
          nod(knew)%lin = nod(k)%lin
       endif
    endif
 enddo
 
 do k = numk_new+1, numk
    xk(k) = xymis
    yk(k) = xymis
 enddo
 
 do l = 1, numl
     lnew = l2lnew(l)
     if (lnew > 0) then
         kn(1,l) = k2knew(kn(1,l))
         kn(2,l) = k2knew(kn(2,l))
         !
         if (lnew < l) then
            lnn(lnew) = lnn(l)
            lne(:,lnew) = lne(:,l)
            kn(:,lnew) = kn(:,l)
         endif
     endif
 enddo

 do ic = 1, nump
    do i = 1,netcell(ic)%n
        netcell(ic)%nod(i) = k2knew(netcell(ic)%nod(i))
        netcell(ic)%lin(i) = l2lnew(netcell(ic)%lin(i))
    enddo
 enddo
 
 numk = numk_new
 deallocate(nod_used, k2knew)

 numl = numl_new
 deallocate(lin_used, l2lnew)

end subroutine remove_unused_nodes_and_links