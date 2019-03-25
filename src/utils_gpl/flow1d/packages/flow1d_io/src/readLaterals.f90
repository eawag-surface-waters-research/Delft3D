module m_readLaterals
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2019.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.             
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
!  $Id$
!  $HeadURL$
!-------------------------------------------------------------------------------

   use MessageHandling
   use m_network
   use m_Laterals

   use properties
   use m_hash_search

   implicit none

   private

   public readLateralLocations

   contains

   subroutine readLateralLocations(network, lateralLocationFile)

      implicit none
      
      type(t_network), intent(inout) :: network
      character*(*), intent(in)      :: lateralLocationFile

      logical                                       :: success
      type(tree_data), pointer                      :: md_ptr 
      integer                                       :: istat
      integer                                       :: numstr
      integer                                       :: i

      character(len=IdLen)                          :: lateralID
      character(len=IdLen)                          :: branchID
      
      double precision                              :: Chainage
      double precision                              :: length
      integer                                       :: branchIdx
      type(t_lateral), pointer                      :: pLat
      type(t_branch), pointer                       :: pBranch

      call tree_create(trim(lateralLocationFile), md_ptr, maxlenpar)
      call prop_file('ini',trim(lateralLocationFile),md_ptr, istat)
      
      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if

      do i = 1, numstr
         
         if (tree_get_name(md_ptr%child_nodes(i)%node_ptr) .eq. 'lateraldischarge') then
            
            ! Read Data
            
            call prop_get_string(md_ptr%child_nodes(i)%node_ptr, 'lateraldischarge', 'id', lateralID, success)
            if (success) call prop_get_string(md_ptr%child_nodes(i)%node_ptr, 'lateraldischarge', 'branchid', branchID, success)
            if (success) call prop_get_double(md_ptr%child_nodes(i)%node_ptr, 'lateraldischarge', 'chainage', Chainage, success)
            if (.not. success) then
               call SetMessage(LEVEL_FATAL, 'Error Reading Lateral Location '''//trim(lateralID)//'''')
            endif
            
            call prop_get_double(md_ptr%child_nodes(i)%node_ptr, 'lateraldischarge', 'length', length, success)
            if (.not. success) length = 0.0d0
            
            branchIdx = hashsearch(network%brs%hashlist, branchID)
            if (branchIdx > 0) Then
               
               pBranch => network%brs%branch(branchIdx)
               
               ! Check Chainages
               if (Chainage < 0.0d0) then
                  call SetMessage(LEVEL_ERROR, 'Negative Chainage for Lateral Location '''//trim(lateralID)//'''')
               elseif (Chainage > pBranch%length) then
                  call SetMessage(LEVEL_ERROR, 'Chainage > Branch Length for Lateral Location '''//trim(lateralID)//'''')
               elseif (length < 0.0d0) then
                  call SetMessage(LEVEL_ERROR, 'Negative Length for Lateral Location '''//trim(lateralID)//'''')
               elseif ((Chainage + length) > pBranch%length) then
                  call SetMessage(LEVEL_ERROR, 'Chainage + Length > Branch Length for Lateral Location '''//trim(lateralID)//'''')
               endif
            else
               call SetMessage(LEVEL_ERROR, 'Error Reading Lateral Location '''//trim(lateralID)//''': Branch: '''//trim(branchID)//''' not Found')
            endif
      
            network%lts%Count = network%lts%Count + 1
            if (network%lts%Count > network%lts%Size) then
               call realloc(network%lts)
            endif
      
            pLat => network%lts%lat(network%lts%Count)
            
            pLat%id = lateralID
            pLat%pointsCount = 1
            allocate(pLat%branch(1))
            allocate(pLat%beginchainage(1))
            allocate(pLat%endchainage(1))
            
            if (length > 0.0d0)  then
               pLat%pointLateral = .false.
            else
               pLat%pointLateral = .true.
            endif
      
            pLat%branch(1)       = branchIdx
            pLat%beginchainage(1)  = Chainage
            pLat%endchainage(1)    = Chainage + length
            pLat%concentration   = 0.0d0;          
            pLat%freshWater      = .true.
      
         endif
      
      end do
      
      call fill_hashtable(network%lts)
      
      call tree_destroy(md_ptr)

   end subroutine readLateralLocations

end module m_readLaterals
