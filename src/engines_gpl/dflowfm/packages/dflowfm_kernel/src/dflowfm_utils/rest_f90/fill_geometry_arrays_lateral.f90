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

!> Fills in the geometry arrays of laterals for history output
subroutine fill_geometry_arrays_lateral()
   use m_wind
   use m_alloc
   use m_partitioninfo
   use m_cell_geometry, only: xz, yz
   implicit none

   integer,          allocatable :: nodeCountLatGat(:), nlatndGat(:), recvCount(:), displs(:)
   double precision, allocatable :: xGat(:), yGat(:) ! Coordinates that are gatherd data from all subdomains
   integer                       :: i, j, k, k1, ierror, is, ie, n, nnode, ii, nlatndMPI
   integer,          allocatable :: nodeCountLatMPI(:)  ! Count of nodes per lateral after mpi communication.
   double precision, allocatable :: geomXLatMPI(:)      ! [m] x coordinates of laterals after mpi communication.
   double precision, allocatable :: geomYLatMPI(:)      ! [m] y coordinates of laterals after mpi communication.

   ! Allocate and construct geometry variable arrays (on one subdomain)
   call realloc(nodeCountLat,   numlatsg, keepExisting = .false., fill = 0  )
   call realloc(geomXLat,       nlatnd,   keepExisting = .false., fill = 0d0)
   call realloc(geomyLat,       nlatnd,   keepExisting = .false., fill = 0d0)

   j = 1
   do i = 1, numlatsg
      do k1=n1latsg(i),n2latsg(i)
         k = nnlat(k1)
         if (k > 0) then
            geomXLat(j) = xz(k)
            geomYLat(j) = yz(k)
            j = j + 1
         end if
      end do
      nodeCountLat(i) = n2latsg(i)-n1latsg(i)+1
   end do

   ! For parallel simulation: since only process 0000 writes the history output, the related arrays
   ! are only made on 00000.
   if (jampi > 0) then
      call reduce_int_sum(nlatnd, nlatndMPI) ! Get total number of nodes among all subdomains

      if (my_rank == 0) then
         ! Allocate history output arrays
         call realloc(nodeCountLatMPI, numlatsg,  keepExisting = .false., fill = 0  )
         call realloc(geomXLatMPI,     nlatndMPI, keepExisting = .false., fill = 0d0)
         call realloc(geomyLatMPI,     nlatndMPI, keepExisting = .false., fill = 0d0)

         ! Allocate arrays that gather information from all subdomains
         ! Data on all subdomains will be gathered in a contiguous way
         call realloc(nodeCountLatGat, numlatsg*ndomains, keepExisting = .false., fill = 0  )
         call realloc(xGat,            nlatndMPI,         keepExisting = .false., fill = 0d0)
         call realloc(yGat,            nlatndMPI,         keepExisting = .false., fill = 0d0)
         call realloc(displs,          ndomains,          keepExisting = .false., fill = 0  )
         call realloc(nlatndGat,       ndomains,          keepExisting = .false., fill = 0  )
      end if

      ! Gather integer data, where the same number of data, i.e. numlatsg, are gathered from each subdomain to process 0000
      call gather_int_data_mpi_same(numlatsg, nodeCountLat, numlatsg*ndomains, nodeCountLatGat, numlatsg, 0, ierror)

      if (my_rank == 0) then
         ! To use mpi gather call, construct displs, and nlatndGat (used as receive count for mpi gather call)
         displs(1) = 0
         do i = 1, ndomains
            is = (i-1)*numlatsg+1 ! Starting index in nodeCountLatGat
            ie = is+numlatsg-1    ! Endding index in nodeCountLatGat
            nlatndGat(i) = sum(nodeCountLatGat(is:ie)) ! Total number of nodes on subdomain i
            if (i > 1) then
               displs(i) = displs(i-1) + nlatndGat(i-1)
            end if
         end do
      end if

      ! Gather double precision data, here, different number of data are gatherd from different subdomains to process 0000
      call gatherv_double_data_mpi_dif(nlatnd, geomXLat, nlatndMPI, xGat, ndomains, nlatndGat, displs, 0, ierror)
      call gatherv_double_data_mpi_dif(nlatnd, geomYLat, nlatndMPI, yGat, ndomains, nlatndGat, displs, 0, ierror)

      if (my_rank == 0) then
         ! Construct nodeCountLatMPI for history output
         do i = 1, numlatsg
            do n = 1, ndomains
               k = (n-1)*numlatsg+i
               nodeCountLatMPI(i) = nodeCountLatMPI(i) + nodeCountLatGat(k) ! Total number of nodes for later i among all subdomains
            end do
         end do

         ! Construct geomXLatMPI and geomyLatMPI for history output
         j = 1
         do i = 1, numlatsg    ! for each lateral
            do n = 1, ndomains ! on each sudomain
               k = (n-1)*numlatsg+i        ! index in nodeCountLatGat
               nnode = nodeCountLatGat(k)  ! lateral i on sumdomain n has nnode nodes
               if (nnode > 0) then
                  ii = (n-1)*numlatsg
                  is = sum(nlatndGat(1:n-1)) + sum(nodeCountLatGat(ii+1:ii+i-1))! starting index in xGat
                  do k1 = 1, nnode
                     geomXLatMPI(j) = xGat(is+k1)
                     geomyLatMPI(j) = yGat(is+k1)
                     j = j + 1
                  end do
               end if
            end do
         end do
         ! Copy the MPI-arrays to nodeCoutLat, geomXLat and geomYLat for the his-output
         nlatnd = nlatndMPI
         nodeCountLat(1:numlatsg) = nodeCountLatMPI(1:numlatsg)
         call realloc(geomXLat, nlatnd, keepExisting = .false., fill = 0d0)
         call realloc(geomyLat, nlatnd, keepExisting = .false., fill = 0d0)
         geomXLat(1:nlatnd) = geomXLatMPI(1:nlatnd)
         geomYLat(1:nlatnd) = geomYLatMPI(1:nlatnd)
      end if
   end if
end subroutine fill_geometry_arrays_lateral
