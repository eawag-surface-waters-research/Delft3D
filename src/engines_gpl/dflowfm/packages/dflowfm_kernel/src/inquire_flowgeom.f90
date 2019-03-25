
!> This module contains general functions for snapping locations to either flowlink numbers or flownode numbers
module m_inquire_flowgeom
   implicit none
   
   private

   public findlink !< find flowlink number
   public findpoint !< find grid index
   
   interface findlink
      module procedure findlink_by_pli          !< find flow link number, using  a polygon
      module procedure findlink_by_branchindex  !< find the flow link number, using (branch index, chainage)
      module procedure findlink_by_branchid     !< find the flow link number, using (branch id, chainage)
   end interface

   interface findpoint
      module procedure findpoint_by_pli          !< find flow grid number, using  a polygon
      module procedure findpoint_by_id           !< find the grid number on node Id 
      module procedure findpoint_by_branchindex  !< find the flow link number, using (branch index, chainage)
      module procedure findpoint_by_branchid     !< find the flow link number, using (branch id, chainage)
   end interface
   
   integer, public, parameter :: FL_1D = 1
   integer, public, parameter :: FL_2D = 2

   contains
   
   !> find flow link number, using  a polygon
   subroutine findlink_by_pli(xpol, ypol, npol, Larr , numlinks, lftopol, sortlinks, mask)
      use m_flowgeom, only : xz, yz, ln, lnx, lnx1D
      use sorting_algorithms
      
      integer, intent(in)              :: npol
      double precision,  intent(in   )  :: xpol(npol)   !< x-coordinates of the polyline
      double precision,  intent(in   )  :: ypol(npol)   !< y-coordinates of the polyline
      integer,           intent(  out)  :: Larr(:)      !< array with links, connected to the polygon. Length is the resonsibility of the call-side
      integer,           intent(  out)  :: numlinks     !< number of found links        
      integer, optional, intent(in   )  :: sortlinks    !< indicates whether the links have to be sorted.
      integer, optional, intent(in   )  :: mask         !< select for search range only 1D links (mask==FL_1D), 2d (mask==FL_2D), or both (mask==FL_1D+FL_2D)
      integer, optional, intent(inout)  :: lftopol(:)   !< flow link to intersecting polygon segment
      
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
      
      ! select 
      if (present(mask)) then
         select case(mask)
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
           
         call crosspoly(xa,ya,xb,yb,xpol,ypol,npol,XM,YM,CRPM,found,isec,dist)
      
         if (found == 1) then   
            numlinks = numlinks + 1
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
    
      !if required, sort the links by distance in the polyline
      if (present(sortLinks) .and. numlinks.gt.0) then
         if (sortLinks==1) then
            allocate(sortedDistsStartPoly(numlinks))
            allocate(sortedIndexses(numlinks))
            allocate(tempLinkArray(numlinks))
  
            call sort(numlinks,distsStartPoly(1:numlinks),sortedDistsStartPoly,sortedIndexses)
            tempLinkArray = Larr(sortedIndexses(1:numlinks))
            Larr(1:numlinks) = tempLinkArray
            
            deallocate(distsStartPoly)
            deallocate(sortedDistsStartPoly)
            deallocate(sortedIndexses)
            deallocate(tempLinkArray)
         endif
      endif
  
   end subroutine findlink_by_pli
   
   !> find the flow link number, using (branch index, chainage)
   subroutine findlink_by_branchindex(branchindex, chainage, L)
      use unstruc_channel_flow
      
      integer,          intent(in)     :: branchindex    !< branch number
      double precision, intent(in)     :: chainage       !< chainage of item on the branch with index branchindex
      integer,          intent(  out)  :: L              !< link number 
      
      L = getLinkIndex(network%brs%branch(branchIndex), chainage)
      
   end subroutine findlink_by_branchindex

   subroutine findlink_by_branchid(branchid, chainage, L)
      use unstruc_channel_flow
      use m_hash_search
      
      character(len=Idlen), intent(in   ) :: branchid       !< branch Id
      double precision, intent(in)        :: chainage       !< chainage of item on the branch with index branchindex
      integer,          intent(  out)     :: L              !< link number 
      
      integer :: branchindex

      branchindex = hashsearch(network%brs%hashlist, branchid)
      L = getLinkIndex(network%brs%branch(branchIndex), chainage)
      
   end subroutine findlink_by_branchid

   !> find flow grid number(s), using  a polygon
   subroutine findpoint_by_pli(points, numpoints, mask)         
      use m_flowgeom, only : xz, yz, ndx2D, ndxi
      
      integer,           intent(  out)  :: points(:)    !< array with links, connected to the polygon. Length is the resonsibility of the call-side
      integer,           intent(  out)  :: numpoints    !< number of found links        
      integer, optional, intent(in   )  :: mask         !< select for search range only 1D links (mask==FL_1D), 2d (mask==FL_2D), or both (mask==FL_1D+FL_2D)
      ! ! 1:ndx2D, ndx2D+1:ndxi, ndxi+1:ndx1Db, ndx1Db:ndx
      !if (present(mask)) then
      !   select case(mask)
      !   case (FL_1D)
      !      nstart = ndx2D+1
      !      nend   = ndxi
      !   case (FL_2D)
      !      nstart = 1
      !      nend   = ndx2D
      !   case (FL_1D + FL_2D)
      !      nstart = 1
      !      nend   = ndxi
      !   end select
      !else
      !   nstart = 1
      !   nend   = ndxi
      !endif
      !
      !size_points = size(points)
      !numpoints = 0
      !do n = nstart, nend
      !   call inwhichpolygon(xp,yp,in)
      !enddo
      
      !TODO JN: Implement this function
   end subroutine findpoint_by_pli         
   
   !> find the grid number on node Id 
   subroutine findpoint_by_id(nodeId, gridindex)         
      use messagehandling
      use m_hash_search
      use unstruc_channel_flow
      
      character(len=Idlen), intent(in   ) :: nodeId          !< Id of the connection node
      integer,              intent(  out) :: gridindex       !< grid index
      
      integer :: nodeindex
      
      nodeindex = hashsearch(network%nds%hashlist, nodeId)
      gridindex = network%nds%node(nodeindex)%gridNumber
      
   end subroutine findpoint_by_id        
   
   !> find the flow link number, using (branch id, chainage)
   subroutine findpoint_by_branchid(branchId, chainage, gridindex)     
      use m_hash_search
      use unstruc_channel_flow

      character(len=Idlen), intent(in   ) :: branchid       !< branch Id
      double precision, intent(in)        :: chainage       !< chainage of item on the branch with index branchindex
      integer,          intent(  out)     :: gridindex      !< gridnumber 
      
      integer :: branchindex

      branchindex = hashsearch(network%brs%hashlist, branchid)
      
      gridindex = getGridPointNumber(network%brs%branch(branchindex), chainage)
   end subroutine findpoint_by_branchid     
 
   !> find the flow link number, using (branch id, chainage)
   subroutine findpoint_by_branchindex(branchIndex, chainage, gridindex)     
      use m_hash_search
      use unstruc_channel_flow

      integer         , intent(in   )     :: branchindex    !< branch index
      double precision, intent(in)        :: chainage       !< chainage of item on the branch with index branchindex
      integer,          intent(  out)     :: gridindex      !< gridnumber 

      gridindex = getGridPointNumber(network%brs%branch(branchindex), chainage)
   end subroutine findpoint_by_branchindex     
 
end module m_inquire_flowgeom