!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2018.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------

! $Id$
! $HeadURL$

! Module for grid operations on 1d networks
   
module odugrid
!use xxx

implicit none

!user defined data types
   contains
!
! Calculates a
! 
function odu_get_xy_coordinates(branchids, branchoffsets, geopointsX, geopointsY, nbranchgeometrynodes, branchlengths, meshXCoords, meshYCoords) result(ierr)

   use m_ggeo_sferic
   integer, intent(in)               :: branchids(:), nbranchgeometrynodes(:)
   double precision, intent(in)      :: branchoffsets(:), geopointsX(:), geopointsY(:), branchlengths(:)
   double precision, intent(inout)   :: meshXCoords(:), meshYCoords(:)

   integer                           :: angle, i, ierr, ind, branchid, idxstart, idxend, idxbr, idxgeostart, idxgeoend, nsegments
   double precision, allocatable     :: branchSegmentLengths(:)
   double precision, allocatable     :: xincrement(:), yincrement(:), zincrement(:)
   double precision, allocatable     :: deltaX(:), deltaY(:), deltaZ(:)
   double precision, allocatable     :: cartMeshXCoords(:), cartMeshYCoords(:), cartMeshZCoords(:)
   double precision, allocatable     :: cartGeopointsX(:), cartGeopointsY(:), cartGeopointsZ(:)
   double precision, allocatable     :: meshZCoords(:)
   double precision, allocatable     :: geopointsZ(:)  !returned by sphertocart3D 
   double precision                  :: totallength, previousLength, afac, fractionbranchlength, maxlat

   ierr = 0
   ! the number of geometry segments is always equal to number of geopoints - 1
   allocate(branchSegmentLengths(size(geopointsX,1) - 1))
   
   allocate(deltaX(size(geopointsX,1) - 1))
   allocate(deltaY(size(geopointsX,1) - 1))
   allocate(deltaZ(size(geopointsX,1) - 1))
   
   allocate(xincrement(size(geopointsX,1) - 1))
   allocate(yincrement(size(geopointsX,1) - 1))
   allocate(zincrement(size(geopointsX,1) - 1))
   
   allocate(cartGeopointsX(size(geopointsX,1)))
   allocate(cartGeopointsY(size(geopointsX,1)))
   allocate(cartGeopointsZ(size(geopointsX,1)))
   
   allocate(cartMeshXCoords(size(meshXCoords,1)))
   allocate(cartMeshYCoords(size(meshXCoords,1)))
   allocate(cartMeshZCoords(size(meshXCoords,1)))
   
   !If the coordinates are spherical, trasform them in cartesian, so we operate in linear space
   maxlat = 0
   if (jsferic == 1) then
      do i = 1, size(geopointsX,1)
         maxlat = max(maxlat,geopointsX(i))
         call sphertocart3D(geopointsX(i),geopointsY(i),cartGeopointsX(i),cartGeopointsY(i), cartGeopointsZ(i)) 
      end do
   else
      cartGeopointsX(:) = geopointsX(:)
      cartGeopointsY(:) = geopointsY(:)
      cartGeopointsZ(:) = 0
   endif
   
   ! initialization
   branchid       = branchids(1)
   idxstart       = 1
   idxend         = 1
   idxbr          = 1
   idxgeostart    = 1
   idxgeoend      = 1
   do while (idxbr<=size(branchlengths,1))
      !calculate the starting and ending indexses of the mesh points
      do i = idxstart + 1, size(branchoffsets,1)
         if (branchids(i).ne.branchid) then
            branchid = branchids(i)
            idxend = i-1;
            exit
         endif
         if (i ==  size(branchoffsets,1)) then
         idxend = i;
         endif
      end do
      !number of geometry segments for the current branch
      nsegments = nbranchgeometrynodes(idxbr) -1
      idxgeoend = idxgeostart + nsegments
      !calculate the branch lenghts
      totallength = 0.0D0
      do i = idxgeostart, idxgeoend -1
         deltaX(i) = cartGeopointsX(i+1) - cartGeopointsX(i)
         deltaY(i) = cartGeopointsY(i+1) - cartGeopointsY(i)
         deltaZ(i) = cartGeopointsZ(i+1) - cartGeopointsZ(i)
         branchSegmentLengths(i)= sqrt(deltaX(i)**2+deltaY(i)**2+deltaZ(i)**2)
         totallength = totallength + branchSegmentLengths(i)
      enddo
      !correct for total segment length
      afac = branchlengths(idxbr)/totallength
      branchSegmentLengths(idxgeostart: idxgeoend -1) = branchSegmentLengths(idxgeostart: idxgeoend -1) * afac
      !calculate the increments
      do i = idxgeostart, idxgeoend -1
         if (branchSegmentLengths(i) > epsilon(0.D0)) then
            xincrement(i)  = deltaX(i)/branchSegmentLengths(i)
            yincrement(i)  = deltaY(i)/branchSegmentLengths(i)
            zincrement(i)  = deltaZ(i)/branchSegmentLengths(i)
         else
            xincrement(i)  = 0.D0
            yincrement(i)  = 0.D0   
            zincrement(i)  = 0.D0   
         endif
      enddo
      !now loop over the mesh points
      ind = idxgeostart
      totallength = branchSegmentLengths(ind)
      previousLength = 0
      do i = idxstart, idxend
         if(branchoffsets(i) > totallength) then
            previousLength = totallength
            ind = ind +1
            totallength = totallength + branchSegmentLengths(ind)
         endif
            fractionbranchlength =  branchoffsets(i) - previousLength
            cartMeshXCoords(i) = cartGeopointsX(ind) + fractionbranchlength * xincrement(ind)
            cartMeshYCoords(i) = cartGeopointsY(ind) + fractionbranchlength * yincrement(ind)
            !TODO: this function should also return meshZCoords (it is relevant if coordinates are spheric) 
            cartMeshZCoords(i) = cartGeopointsZ(ind) + fractionbranchlength * zincrement(ind)
      enddo
      !update indexses
      idxgeostart = idxgeoend + 1
      idxstart    = idxend + 1
      idxbr       = idxbr + 1
   enddo
   
   if (jsferic == 1) then
      do i = 1, size(meshXCoords,1)
         call Cart3Dtospher(cartMeshXCoords(i),cartMeshYCoords(i),cartMeshZCoords(i),meshXCoords(i),meshYCoords(i),maxlat) 
      end do
   else
      meshXCoords(:) = cartMeshXCoords(:)
      meshYCoords(:) = cartMeshYCoords(:)
      ! meshZCoords(:) = cartMeshZCoords(:)  !  meshZCoords is never used and assignment causes runtime error
   endif

end function odu_get_xy_coordinates


end module odugrid