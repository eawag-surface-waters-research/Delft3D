module m_readRetentions
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
   use m_Storage
   use m_tables

   use properties
   use m_hash_search

   implicit none

   private

   public readRetentions

   contains

   subroutine readRetentions(network, retentionFile)

      implicit none
      
      type(t_network), intent(inout) :: network
      character*(*), intent(in)      :: retentionFile

      logical                                       :: success
      type(tree_data), pointer                      :: md_ptr 
      integer                                       :: istat
      integer                                       :: numstr
      integer                                       :: i

      character(len=IdLen)                          :: retentionID
      character(len=IdLen)                          :: branchID
      character(len=IdLen)                          :: nodeID
      character(len=IdLen)                          :: storageType
      logical                                       :: useTable
      
      double precision                              :: Chainage
      integer                                       :: branchIdx
      integer                                       :: nodeIdx
      integer                                       :: local_grid_index
      integer                                       :: gridPoint
      type(t_storage), pointer                      :: pSto
      
      integer                                       :: nLevels
      integer                                       :: interPolate
      logical                                       :: useStreetStorage
      double precision, allocatable, dimension(:)   :: storLevels
      double precision, allocatable, dimension(:)   :: storAreas

      call tree_create(trim(retentionFile), md_ptr, maxlenpar)
      call prop_file('ini',trim(retentionFile),md_ptr, istat)
      
      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if

      success = .true.
      call prop_get_logical(md_ptr, 'general', 'useStreetStorage', useStreetStorage, success)
      if (.not. success) then
         useStreetStorage = .true.
      endif
      
      do i = 1, numstr
         
         if (tree_get_name(md_ptr%child_nodes(i)%node_ptr) .eq. 'retention') then
            
            ! Read Data
            success = .true.
            call prop_get_string(md_ptr%child_nodes(i)%node_ptr, 'retention', 'id', retentionID, success)
            if (success) call prop_get_string(md_ptr%child_nodes(i)%node_ptr, 'retention', 'branchid', branchID, success)
            if (success) call prop_get_double(md_ptr%child_nodes(i)%node_ptr, 'retention', 'chainage', Chainage, success)
            if (success) then
               branchIdx = hashsearch(network%brs%hashlist, branchID)
               if (branchIdx <= 0) Then
                  call SetMessage(LEVEL_ERROR, 'Error Reading Retention '''//trim(retentionID)//''': Branch: '''//trim(branchID)//''' not Found')
                  exit
               endif
               gridPoint = getCalcPoint(network%brs, branchIdx, Chainage)
               if (gridPoint == network%brs%branch(branchIdx)%points(1) ) then
                  gridPoint = -network%brs%branch(branchIdx)%fromNode%index
                  local_grid_index = -1
                  branchIdx        = -1
               elseif (gridPoint == network%brs%branch(branchIdx)%points(2)) then
                  gridPoint = -network%brs%branch(branchIdx)%toNode%index
                  branchIdx        = -1
                  local_grid_index = -1
               else
                  local_grid_index = gridPoint - network%brs%branch(branchIdx)%points(1) + 1 
               endif
               
            else
               success = .true.
               call prop_get_string(md_ptr%child_nodes(i)%node_ptr, 'retention', 'nodeId', nodeid, success)
               if (.not. success) then
                  call SetMessage(LEVEL_FATAL, 'Error Reading Retention '''//trim(retentionID)//'''')
                  exit
               endif
               nodeIdx = hashsearch(network%nds%hashlist, nodeId)
               if (nodeIdx <= 0) Then
                  call SetMessage(LEVEL_ERROR, 'Error Reading Retention '''//trim(retentionID)//''': node: '''//trim(nodeID)//''' not Found')
                  exit
               endif
               gridPoint        = network%nds%node(nodeIdx)%index
               branchIdx        = -1
               local_grid_index = -1
            endif

            call prop_get_string(md_ptr%child_nodes(i)%node_ptr, 'retention', 'storagetype', storageType, success)
            if (.not. success) storageType = 'Reservoir'
            

            network%storS%Count = network%storS%Count + 1
            if (network%storS%Count > network%storS%Size) then
               call realloc(network%storS)
            endif
      
            pSto => network%storS%stor(network%storS%Count)
            nullify(pSto%storageArea)
            nullify(pSto%streetArea)

            ! Bcause of the complicated data structure of SOBEK storage in 'connection nodes'
            ! must be separated from the ordinary gridpoints
            pSto%id        = retentionID
            pSto%gridPoint = gridPoint
            network%storS%mapping(gridPoint) = network%storS%Count
            psto%branch_index     = branchIdx
            psto%local_grid_index = local_grid_index
            psto%node_index = gridpoint
            if (storageType == 'Closed') then
               pSto%storageType = nt_Closed
            elseif (storageType == 'Loss') then
               pSto%storageType = nt_Loss
            else
               pSto%storageType = nt_Reservoir
            endif

            call prop_get_integer(md_ptr%child_nodes(i)%node_ptr, 'retention', 'numlevels', nLevels, success)
            if (success) then
               useTable = .true.
            else
               nLevels  = 1
               useTable = .false.
            endif
            
            ! Allocate Arrays
            call realloc(storLevels, nLevels, stat=istat)
            if (istat == 0) call realloc(storAreas, nLevels, stat=istat)
            if (istat .ne. 0) then
               call SetMessage(LEVEL_FATAL, 'Reading Retentions: Error Allocating Arrays')
            endif
            
            if (useTable) then

               call prop_get_doubles(md_ptr%child_nodes(i)%node_ptr, 'retention', 'levels', storLevels, nLevels, success)
               if (success)call prop_get_doubles(md_ptr%child_nodes(i)%node_ptr, 'retention', 'storageArea', storAreas, nLevels, success)
               if (.not. success) then
                  call SetMessage(LEVEL_FATAL, 'Reading Retentions: Error in Level/Storage Data')
               endif
      
               call prop_get_integer(md_ptr%child_nodes(i)%node_ptr, 'retention', 'interpolate', interPolate, success)
               if (.not. success) interPolate = 0
               
            else
               
               call prop_get_double(md_ptr%child_nodes(i)%node_ptr, 'retention', 'bedlevel', storLevels(1), success)
               if (success) call prop_get_double(md_ptr%child_nodes(i)%node_ptr, 'retention', 'area', storAreas(1), success)
               if (.not. success) then
                  call SetMessage(LEVEL_ERROR, 'Reading Retentions: Error in Level/Storage Data')
                  cycle
               endif
               
               interPolate = 1
               
            endif

            if (storAreas(1) <= 0d0) then
               call setMessage(LEVEL_ERROR, 'Area at Bed Level for Retention ' // trim(retentionID) // ' <= 0.0. Please enter a positive value')
            endif
            
            call setTable(pSto%storageArea, interPolate, storLevels, storAreas, nLevels)

            call prop_get_double(md_ptr%child_nodes(i)%node_ptr, 'retention', 'streetlevel', storLevels(1), success)
            call prop_get_double(md_ptr%child_nodes(i)%node_ptr, 'retention', 'streetstoragearea', storAreas(1), success)
            if (success .and. storAreas(1) > 0d0 .and. useStreetStorage) then
               psto%useStreetStorage = .true.
               call setTable(pSto%streetArea, interPolate, storLevels, storAreas, nLevels)
            else 
               psto%useStreetStorage = .false.
            endif

      
         endif
      
      end do
      
      ! Clear Arrays
      istat = 0
      if (allocated(storLevels)) deallocate(storLevels, stat=istat)
      if (istat == 0 .and. allocated(storAreas)) deallocate(storAreas, stat=istat)
      if (istat .ne. 0) then
         call SetMessage(LEVEL_ERROR, 'Reading Retentions: Error Deallocating Arrays')
      endif

      call fill_hashtable(network%storS)
      
      call tree_destroy(md_ptr)

   end subroutine readRetentions

end module m_readRetentions