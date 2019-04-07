!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2019.!
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

!> This module contains general functions for snapping locations to either flowlink numbers or flownode numbers
module m_inquire_flowgeom
   implicit none
   
   private

   public findlink !< find flowlink number
   public findnode !< find flownode number
   
   interface findlink
      module procedure findlink_by_pli          !< find the flow link number, using a polyline
      module procedure findlink_by_branchindex  !< find the flow link number, using (branch index, chainage)
      module procedure findlink_by_branchid     !< find the flow link number, using (branch id, chainage)
      module procedure findlink_by_structureid  !< find the flow link number, using a structure id
   end interface

   interface findnode
      module procedure findnode_by_pol          !< find the flow node number, using a polygon
      module procedure findnode_by_id           !< find the flow node number, using node Id 
      module procedure findnode_by_branchindex  !< find the flow node number, using (branch index, chainage)
      module procedure findnode_by_branchid     !< find the flow node number, using (branch id, chainage)
   end interface
   
   integer, public, parameter :: FL_1D = 1
   integer, public, parameter :: FL_2D = 2

   contains
   
   !> Find flow link number(s) intersected by a given polyline.
   function findlink_by_pli(npl, xpl, ypl, Larr , numlinks, lftopol, sortlinks, linktype) result(ierr)
      use m_flowgeom, only : xz, yz, ln, lnx, lnx1D
      use sorting_algorithms
      
      integer                           :: ierr
      integer,           intent(in   )  :: npl          !< Number of polyline points.
      double precision,  intent(in   )  :: xpl(:)       !< x-coordinates of the polyline.
      double precision,  intent(in   )  :: ypl(:)       !< y-coordinates of the polyline.
      integer,           intent(  out)  :: Larr(:)      !< array with flow links, intersected by the polyline. Length is the resonsibility of the call site.
      integer,           intent(  out)  :: numlinks     !< Number of found flow links.
      integer, optional, intent(in   )  :: sortlinks    !< Indicates whether the flow links have to be sorted.
      integer, optional, intent(in   )  :: linktype     !< Limit search to specific link types: only 1D flow links (linktype==FL_1D), 2D (linktype==FL_2D), or both (linktype==FL_1D+FL_2D).
      integer, optional, intent(inout)  :: lftopol(:)   !< Mapping array from flow link to intersecting polyline segment.
      
      double precision :: xa, ya
      double precision :: xb, yb
      double precision :: xm, ym
      double precision :: crpm
      double precision :: dist
      double precision, allocatable :: distsStartPoly(:)
      double precision, allocatable :: sortedDistsStartPoly(:)
      integer, allocatable          :: sortedIndexses(:)
      integer, allocatable          :: tempLinkArray(:)
      integer :: found
      integer :: size_arr
      integer :: L
      integer :: Lstart, Lend
      integer :: isec
      integer :: k1, k2
      
      size_arr = size(Larr)
      numlinks = 0
      if (present(sortLinks)) then
         allocate(distsStartPoly(lnx))
      endif
      
      ! select search range for flow links
      if (present(linktype)) then
         select case(linktype)
         case (FL_1D)
            Lstart = 1
            Lend   = lnx1D
         case (FL_2D)
            Lstart = lnx1D + 1
            Lend   = lnx
         case (FL_1D+FL_2D)
            Lstart = 1
            Lend   = lnx
         end select
      else
         Lstart = 1
         Lend   = lnx
      endif

      do L  = Lstart,Lend
         k1 = ln(1,L)
         k2 = ln(2,L) 
         xa = xz(k1)
         ya = yz(k1)
         xb = xz(k2)
         yb = yz(k2)
           
         call crosspoly(xa,ya,xb,yb,xpl,ypl,npl,XM,YM,CRPM,found,isec,dist)
      
         if (found == 1) then   
            numlinks = numlinks + 1
            if (numlinks > size_arr) then
               ! internal error insufficient space in links array
               ierr = -1
               return
            endif
            if(present(lftopol)) then
               lftopol(numlinks) = isec
            endif
            
            if (crpm > 0) then 
               Larr(numlinks) = -L
            else 
               Larr(numlinks) =  L
            end if
            if (present(sortLinks)) then
               distsStartPoly(numlinks) = dist
            endif
         end if
      enddo
    
      ! if required, sort the links by distance along the polyline
      if (present(sortLinks) .and. numlinks > 0) then
         if (sortLinks==1) then
            allocate(sortedDistsStartPoly(numlinks))
            allocate(sortedIndexses(numlinks))
            allocate(tempLinkArray(numlinks))
  
            call sort(numlinks, distsStartPoly(1:numlinks), sortedDistsStartPoly, sortedIndexses)
            do L=1,numlinks
               tempLinkArray(L) = Larr(sortedIndexses(L))
            end do
            Larr(1:numlinks) = tempLinkArray

            deallocate(sortedDistsStartPoly)
            deallocate(sortedIndexses)
            deallocate(tempLinkArray)
         endif
      endif
      if (allocated(distsStartPoly)) deallocate(distsStartPoly)
  
      ierr = 0
   end function findlink_by_pli


   !> Find the nearest flow link number for a given location, using (branch index, chainage).
   function findlink_by_branchindex(branchindex, chainage, L) result(ierr)
      use unstruc_channel_flow
      
      integer                          :: ierr
      integer,          intent(in   )  :: branchindex    !< Branch index in network brs set.
      double precision, intent(in   )  :: chainage       !< Chainage of item on the branch with index branchindex
      integer,          intent(  out)  :: L              !< Found flow link number, -1 when not found.

      L = -1

      if (branchIndex >= 1 .and. branchIndex <= network%brs%Count) then
         L = getLinkIndex(network%brs%branch(branchIndex), chainage)
      end if

      ierr = 0
   end function findlink_by_branchindex


   !> Find the nearest flow link number for a given location, using (branch id, chainage).
   function findlink_by_branchid(branchid, chainage, L) result(ierr)
      use unstruc_channel_flow
      use m_hash_search
      
      integer                             :: ierr
      character(len=Idlen), intent(in   ) :: branchid       !< Branch Id to be searched in network brs set.
      double precision,     intent(in   ) :: chainage       !< Chainage of item on the branch with index branchindex.
      integer,              intent(  out) :: L              !< Found flow link number, -1 when not found.
      
      integer :: branchindex

      L = -1

      ierr = 0

      branchindex = hashsearch(network%brs%hashlist, branchid)
      if (branchindex > 0) then
         ierr =  findlink_by_branchindex(branchindex, chainage, L)
      end if

   end function findlink_by_branchid


   !> Given a structure id, find the corresponding flow link L
   !! If not find, then L returns 0
   subroutine findlink_by_structureid(strucid, L)
   use unstruc_channel_flow
   implicit none
   character(len=*), intent(in) :: strucid !< Structure id
   integer         , intent(out):: L       !< Found flowlink number
   
   integer :: i
   character strucid_tmp*40
   
   L = 0
   
   do i = 1, network%sts%Count
      strucid_tmp = network%sts%struct(i)%id
      if (trim(strucid_tmp) == trim(strucid)) then
         L = network%sts%struct(i)%link_number
         exit
      end if
   end do
   
   return
   end subroutine findlink_by_structureid
   
   !> find flow node number(s), enclosed in a polygon
   function findnode_by_pol(npol, xpol, ypol, points, numpoints, nodetype) result(ierr)
      use m_flowgeom, only : xz, yz, ndx2D, ndxi
      use messagehandling
      use m_polygon, only : xpl, ypl, npl

      integer                                        :: ierr
      integer,                        intent(in   )  :: npol         !< number of points in the polygon
      double precision, dimension(:), intent(in   )  :: xpol         !< x-coordinates of the points in the polygon
      double precision, dimension(:), intent(in   )  :: ypol         !< y-coordinates of the points in the polygon
      integer,                        intent(  out)  :: points(:)    !< array with points, inside the polygon. Length is the resonsibility of the call-side
      integer,                        intent(  out)  :: numpoints    !< number of found links        
      integer, optional,              intent(in   )  :: nodetype     !< select for search range only 1D nodes (nodetype==FL_1D), 2d (nodetype==FL_2D), or both (nodetype==FL_1D+FL_2D)
      
      integer :: n
      integer :: nstart
      integer :: nend
      integer :: in
      integer :: size_points
      
       ! 1:ndx2D, ndx2D+1:ndxi, ndxi+1:ndx1Db, ndx1Db:ndx
      if (present(nodetype)) then
         select case(nodetype)
         case (FL_1D)
            nstart = ndx2D+1
            nend   = ndxi
         case (FL_2D)
            nstart = 1
            nend   = ndx2D
         case (FL_1D + FL_2D)
            nstart = 1
            nend   = ndxi
         end select
      else
         nstart = 1
         nend   = ndxi
      endif
      
      ! initialize polygon module. 
      npl = npol
      xpl(1:npol) = xpol(1:npol)
      ypl(1:npol) = ypol(1:npol)
      
      ! make sure inwhichpolygon initializes some variables first.
      in = -1
      size_points = size(points)
      numpoints = 0
      do n = nstart, nend
         call inwhichpolygon(xz(n),yz(n),in)
         if (in > 0) then
            numpoints = numpoints + 1
            if (numpoints > size_points) then
               ierr = -1
               return
            endif
            points(numpoints) = n
         endif
      enddo
      
      ierr = 0
   end function findnode_by_pol         


   !> Find the flow node number, using node Id.
   function findnode_by_id(nodeId, nodenr)  result(ierr)
      use messagehandling
      use m_hash_search
      use unstruc_channel_flow
      
      integer                             :: ierr
      character(len=Idlen), intent(in   ) :: nodeId          !< Id of the connection node
      integer,              intent(  out) :: nodenr          !< Found flow node number, -1 when not found.
      
      integer :: nodeindex
      
      nodenr = -1
      nodeindex = hashsearch(network%nds%hashlist, nodeId)
      if (nodeindex > 0) then
         nodenr = network%nds%node(nodeindex)%gridNumber
      end if

      ierr = 0
   end function findnode_by_id        


   !> find the flow node number, using (branch id, chainage).
   function findnode_by_branchid(branchId, chainage, nodenr) result(ierr)
      use m_hash_search
      use unstruc_channel_flow

      integer                             :: ierr
      character(len=Idlen), intent(in   ) :: branchid       !< branch Id
      double precision, intent(in)        :: chainage       !< chainage of item on the branch with id branchid
      integer,          intent(  out)     :: nodenr         !< Found flow node number, -1 when not found.
      
      integer :: branchindex

      nodenr = -1
      ierr = 0

      branchindex = hashsearch(network%brs%hashlist, branchid)
      if (branchindex > 0) then
         ierr =  findnode_by_branchindex(branchindex, chainage, nodenr)
      end if

   end function findnode_by_branchid     

   
   !> Find the flow node number, using (branch index, chainage).
   function findnode_by_branchindex(branchIndex, chainage, nodenr)  result(ierr)
      use m_hash_search
      use unstruc_channel_flow

      integer                             :: ierr
      integer         , intent(in   )     :: branchindex    !< branch index
      double precision, intent(in)        :: chainage       !< chainage of item on the branch with index branchindex
      integer,          intent(  out)     :: nodenr         !< Found flow node number, -1 when not found.

      nodenr = -1

      if (branchIndex >= 1 .and. branchIndex <= network%brs%Count) then
         nodenr = getGridPointNumber(network%brs%branch(branchindex), chainage)
      end if

      ierr = 0
   end function findnode_by_branchindex     
 
end module m_inquire_flowgeom