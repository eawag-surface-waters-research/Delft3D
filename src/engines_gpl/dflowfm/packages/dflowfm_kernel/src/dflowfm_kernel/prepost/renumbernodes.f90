!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2022.                                
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

    !> Renumber net nodes by RCM reordering with kn links as input.
    !! Only called by the user. The netlinks kn are NOT reordered, as this will
    !! be done in renumberFlowNodes anyway.
    subroutine renumberNodes()
    use network_data
    use unstruc_messages
    use gridoperations
    implicit none

    integer , allocatable :: adj_row(:)
    integer , allocatable :: adj(:)
    integer , allocatable :: perm(:), perm_inv(:)
    integer, allocatable           :: i1(:)
    double precision, allocatable  :: dp1(:)

    integer, allocatable :: adj_tmp(:,:), adj_tmp2(:)
    integer, external :: adj_bandwidth, adj_perm_bandwidth

    integer :: numltot, numlcur, j, k, kk, k1, k2, km, L, bw, bwrn, sumdiff, sumdiffrn

    call readyy('Renumber nodes', 0d0 )

    numltot = 2*numl ! Undirected links: 2 adjacency elements per link.

    allocate(adj_row(numk+1), adj(numltot), perm(numk), perm_inv(numk), i1(numl), dp1(numk))
    allocate(adj_tmp(20, numk))
    allocate(adj_tmp2(numk))

    ! Build adjacency list by hand: fill 2D array adj_tmp(20, numk) and
    ! flatten it afterwards to adj(numltot).
    adj_tmp2 = 0
    do L=1,numl
        k1 = kn(1,L)
        k2 = kn(2,L)
        km = adj_tmp2(k1)
        ! For node k1, store its neighbours in sorted order.
        do k=1,km
            if (adj_tmp(k, k1) > k2) then
                exit
            end if
        end do
        adj_tmp(k+1:km+1,k1) = adj_tmp(k:km,k1)
        adj_tmp(k, k1) = k2
        adj_tmp2(k1) = km+1

        ! Same for node k2
        km = adj_tmp2(k2)
        do k=1,km
            if (adj_tmp(k, k2) > k1) then
                exit
            end if
        end do
        adj_tmp(k+1:km+1,k2) = adj_tmp(k:km,k2)
        adj_tmp(k, k2) = k1
        adj_tmp2(k2) = km+1
    end do
    call readyy('Renumber nodes', .3d0 )

    ! Flatten the adjacency list.
    j = 1
    do k=1,numk
        adj_row(k) = j
        do kk=1,adj_tmp2(k)
            adj(j) = adj_tmp(kk, k)
            j = j + 1
        end do
    end do
    adj_row(numk+1) = j

    call readyy('Renumber nodes', .35d0 )

    bw = adj_bandwidth(numk, numltot, adj_row, adj)

    ! Find a renumbering (permutation)
    call genrcm(numk, numltot, adj_row, adj, perm)
    call perm_inverse3(numk, perm, perm_inv)
    call readyy('Renumber nodes', .75d0 )

    ! Now apply the permutation to net node numbers
    dp1 = xk
    do k=1,numk
        xk(k) = dp1(perm(k))
    end do
    dp1 = yk
    do k=1,numk
        yk(k) = dp1(perm(k))
    end do
    dp1 = zk
    do k=1,numk
        zk(k) = dp1(perm(k))
    end do

    sumdiff = 0
    do L=1,numl
        sumdiff = sumdiff + abs(kn(1,L)-kn(2,L))
    end do

    i1 = kn(1,:)
    do L=1,numl
        kn(1,L) = perm_inv(i1(L))
    end do
    i1 = kn(2,:)
    do L=1,numl
        kn(2,L) = perm_inv(i1(L))
    end do

    ! No numbering necessary for kn(3,:) (Links are still in same order)

    ! Finish with some stats
    sumdiffrn = 0
    do L=1,numl
        sumdiffrn = sumdiffrn + abs(kn(1,L)-kn(2,L))
    end do

    call readyy('Renumber nodes', 1d0 )
    bwrn = adj_perm_bandwidth(numk, numltot, adj_row, adj, perm, perm_inv)

    write(msgbuf,*) 'Renumber nodes...'
    call dbg_flush()
    write(msgbuf,*) 'Sumdiff:', sumdiff, '  Sumdiff renum: ', sumdiffrn
    call dbg_flush()
    write(msgbuf,*) 'Bandwidth:', bw, '  Bandwidth renum: ', bwrn
    call dbg_flush()

    deallocate(adj_row, adj, perm, perm_inv, i1, dp1)
    deallocate(adj_tmp,adj_tmp2)

    call readyy('Renumber nodes', -1d0 )
    CALL SETNODADM(0)

    end subroutine renumberNodes
