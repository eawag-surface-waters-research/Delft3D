!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2017.                                
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
   
module network1D
!use xxx
implicit none
!user defined data types
   contains
!
! in = branch id, branch offset of the input point
! out = x,y coordinates of the input point
!
function ggeo_get1D_cartesian_coordinates(branchid, branchoffset, geopointsX, geopointsY, branchidxs, branchlengths, sourcetargets, nodesX, nodesY, meshXCoord, meshYCoord) result(ierr)
   
   integer, intent(in)               :: branchid, branchidxs(:),sourcetargets(:)
   double precision, intent(in)      :: branchoffset, geopointsX(:), geopointsY(:), branchlengths(:), nodesX(:), nodesY(:)
   double precision, intent(out)     :: meshXCoord, meshYCoord 
   
   integer                           :: ngeopointsbranch, segstart, segend, step, angle, brind, i, ierr, ind 
   double precision, allocatable     :: geopointsXbranch(:), geopointsYbranch(:), branchSegmentLengths(:)
   double precision                  :: sourceNodeX, sourceNodeY, deltaX, deltaY, totallength, afac, fractionbranchlength, cos, sin 
     
   sourceNodeX = nodesX(sourcetargets(branchid*2))
   sourceNodeY = nodesY(sourcetargets(branchid*2))
   
   ! Determine the geopoints belonging to the input branchid
   ngeopointsbranch = 0;
   do i = 1, size(branchidxs,1)
      if (branchidxs(i)==branchid) then
         ngeopointsbranch = ngeopointsbranch + 1
      endif
   end do
   
   ! Allocate variables for this branch id
   allocate(geopointsXbranch(ngeopointsbranch))
   allocate(geopointsYbranch(ngeopointsbranch))
   allocate(branchSegmentLengths(ngeopointsbranch))
   
   ind = 0 
   do i = 1, size(geopointsX,1)
      if (branchidxs(i)==branchid) then
         ind = ind + 1
         geopointsXbranch(ind) = geopointsX(i)
         geopointsYbranch(ind) = geopointsY(i)
      endif
   end do
   
   !Determine where the branch starts and end
   if ( (abs(geopointsXbranch(1) - sourceNodeX) < epsilon(0.D0)).and.(abs(geopointsYbranch(1)==sourceNodeY) < epsilon(0.D0))) then
         segstart = 1
         segend   = ngeopointsbranch - 1
         step     = 1
   else
         segstart = ngeopointsbranch
         segend   = 2
         step     =-1
   endif
   
   !calculate the segments lengths
   totallength = 0.0D0
   do i = segstart, step, segend - step
      deltaX = geopointsXbranch(i+step)-geopointsXbranch(i)
      deltaY = geopointsYbranch(i+step)-geopointsYbranch(i)
      branchSegmentLengths(i)= sqrt(deltaX**2+deltaY**2)
      totallength = totallength + branchSegmentLengths(i)
   enddo

   ! Increase the lengths of the segments to match the total branch length 
   afac = branchlengths(branchid)/totallength
   branchSegmentLengths = branchSegmentLengths * afac
   
   ! Find segment id where the offset is contained
   totallength = 0
   do i = segstart, step, segend - step
      totallength = totallength + branchSegmentLengths(i) 
      if (totallength >= branchoffset) then
            brind = i      
            fractionbranchlength = totallength - branchoffset ;
         exit
      endif
   enddo
   
   ! Now we can calculate the x and y coordinates of the input point 
   deltaX = geopointsXbranch(i+step)-geopointsXbranch(i)
   deltaY = geopointsYbranch(i+step)-geopointsYbranch(i)
   if (deltaX > epsilon(0.D0)) then
      angle =  atan(deltaY/deltaX)
   else
      angle =  2.D0*atan(1.D0)
   endif
   
   meshXCoord = geopointsXbranch(brind) + fractionbranchlength *cos(angle) 
   meshYCoord = geopointsYbranch(brind) + fractionbranchlength *sin(angle) 
   
end function ggeo_get1D_cartesian_coordinates


end module network1D