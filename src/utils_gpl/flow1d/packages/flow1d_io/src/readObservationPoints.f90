module m_readObservationPoints
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
   use m_ObservationPoints
   use m_GlobalParameters

   use properties
   use m_hash_search
   use m_hash_list
   use string_module

   implicit none

   private

   public readObservationPoints
   public read_obs_point_cache
   public write_obs_point_cache

   contains

   subroutine readObservationPoints(network, observationPointsFile)

      implicit none
      
      type(t_network), intent(inout)        :: network
      character*(*), intent(in)             :: observationPointsFile

      logical                               :: success
      type(tree_data), pointer              :: md_ptr 
      integer                               :: istat
      integer                               :: numstr
      integer                               :: i

      character(len=IdLen)                  :: obsPointID
      character(len=IdLen)                  :: obsPointName
      character(len=IdLen)                  :: branchID
      
      double precision                      :: Chainage
      integer                               :: branchIdx
      integer                               :: p1
      integer                               :: p2
      integer                               :: l1
      type(t_ObservationPoint), pointer     :: pOPnt
      type(t_branch), pointer               :: pbr
      
      character(CharLn)                     :: line
      character(CharLn)                     :: pnt1
      character(CharLn)                     :: pnt2
      character(CharLn)                     :: val1
      character(CharLn)                     :: val2

      integer                               :: pos
      integer                               :: ibin = 0
      character(len=Charln)                 :: binfile
      logical                               :: file_exist
 
      pos = index(observationPointsFile, '.', back = .true.)
      binfile = observationPointsFile(1:pos)//'cache'
      inquire(file=binfile, exist=file_exist)
      if (doReadCache .and. file_exist) then
         open(newunit=ibin, file=binfile, status='old', form='unformatted', access='stream', action='read', iostat=istat)
         if (istat /= 0) then
            call setmessage(LEVEL_FATAL, 'Error opening Observation Point Cache file')
            ibin = 0
         endif
         call read_obs_point_cache(ibin, network)
         close(ibin)
         ibin = 0
         return
      endif

      call tree_create(trim(observationPointsFile), md_ptr, maxlenpar)
      call prop_file('ini',trim(observationPointsFile),md_ptr, istat)
      
      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if

      do i = 1, numstr
         
         if (tree_get_name(md_ptr%child_nodes(i)%node_ptr) .eq. 'observationpoint') then
            
            ! Read Data
            
            call prop_get_string(md_ptr%child_nodes(i)%node_ptr, 'observationpoint', 'id', obsPointID, success)
            if (success) call prop_get_string(md_ptr%child_nodes(i)%node_ptr, 'observationpoint', 'branchid', branchID, success)
            if (success) call prop_get_double(md_ptr%child_nodes(i)%node_ptr, 'observationpoint', 'chainage', Chainage, success)
            if (.not. success) then
               call SetMessage(LEVEL_ERROR, 'Error Reading Observation Point '''//trim(obsPointID)//'''')
               cycle
            endif
            
            call prop_get_string(md_ptr%child_nodes(i)%node_ptr, 'observationpoint', 'name', obsPointName, success)
            if (.not. success) then
               obsPointName = obsPointID
            endif
            
            branchIdx = hashsearch(network%brs%hashlist, branchID)
            if (branchIdx <= 0) Then
               call SetMessage(LEVEL_ERROR, 'Error Reading Observation Point '''//trim(obsPointID)//''': Branch: '''//trim(branchID)//''' not Found')
               cycle
            endif
      
            network%obs%Count = network%obs%Count+1
            if (network%obs%Count > network%obs%size) then
               call realloc(network%obs)
            endif
            
            
            pOPnt => network%obs%OPnt(network%obs%Count)
            
            pOPnt%id        = obsPointID
            pOPnt%name      = obsPointName
            pOPnt%branch    => network%brs%branch(branchIdx)
            pOPnt%branchIdx = branchIdx
            pOPnt%offset    = Chainage
            pbr             => network%brs%branch(branchIdx)
      
            call get2CalcPoints(network%brs, branchIdx, Chainage, pOPnt%p1, pOPnt%p2, &
                                pOPnt%pointWeight, pOPnt%l1, pOPnt%l2, pOPnt%linkWeight)
      
            if (network%obs%interpolationType == OBS_NEAREST) then
               if (pOPnt%pointWeight > 0.5d0) then
                  pOPnt%pointWeight = 1.0d0
               else
                  pOPnt%pointWeight = 0.0d0
               endif
               p1 = pOPnt%p1 - pbr%Points(1) + 1
               l1 = pOPnt%l1 - pbr%uPoints(1) + 1
               if (l1 >= p1) then
                  pOPnt%linkWeight = 1.0d0
               else
                  pOPnt%linkWeight = 0.0d0
               endif
            endif

            ! Debug Info
            
            if (thresholdLvl_file == LEVEL_DEBUG) then
            write(val1, '(f10.0)') Chainage
            call remove_all_spaces(val1)
            line = 'Observation point '//trim(obsPointID)//' added on branch '//trim(pbr%id)//' at offset '//val1
               call setMessage(LEVEL_DEBUG, line)
            p1 = pOPnt%p1 - pbr%Points(1) + 1
            p2 = pOPnt%p2 - pbr%Points(1) + 1
            
            write(pnt1, '(f10.0)') pbr%gridPointsOffsets(p1)
            call remove_all_spaces(pnt1)
            write(pnt2, '(f10.0)') pbr%gridPointsOffsets(p2)
            call remove_all_spaces(pnt2)
            write(val1,'(f6.3)') pOPnt%pointWeight
            write(val2,'(f6.3)') 1.0-pOPnt%pointWeight
            line = '    Values at grid points evaluated by Val(P_'//trim(pnt1)//') * '//trim(val1)//' + Val(P_'//trim(pnt2)//') * '//trim(val2)
            call setMessage(LEVEL_DEBUG, line)
            p1 = pOPnt%l1 - pbr%uPoints(1) + 1
            p2 = pOPnt%l2 - pbr%uPoints(1) + 1
            
            write(pnt1, '(f10.0)') pbr%uPointsOffsets(p1)
            call remove_all_spaces(pnt1)
            write(pnt2, '(f10.0)') pbr%uPointsOffsets(p2)
            call remove_all_spaces(pnt2)
            write(val1,'(f6.3)') pOPnt%linkWeight
            write(val2,'(f6.3)') 1.0-pOPnt%linkWeight
            line = '    Values at u-points evaluated by Val(P_'//trim(pnt1)//') * '//trim(val1)//' + Val(P_'//trim(pnt2)//') * '//trim(val2)
            call setMessage(LEVEL_DEBUG, line)
            endif

         endif

      end do
    
      call fill_hashtable(network%obs)
      
      call tree_destroy(md_ptr)

   end subroutine readObservationPoints

   subroutine read_obs_point_cache(ibin, network)
   
      type(t_network), intent(inout)         :: network
      integer, intent(in)                    :: ibin
      
      integer                           :: i
      type(t_ObservationPoint), pointer :: pobs

      read(ibin) network%obs%count
      network%obs%growsby = network%obs%count + 2
      call realloc(network%obs)

      do i = 1, network%obs%count
      
         pobs => network%obs%OPnt(i)
         
         read(ibin) pobs%id 
         read(ibin) pobs%name 
         read(ibin) pobs%p1
         read(ibin) pobs%p2
         read(ibin) pobs%pointWeight          
         read(ibin) pobs%l1
         read(ibin) pobs%l2
         read(ibin) pobs%linkWeight
         read(ibin) pobs%branchIdx
         pobs%branch => network%brs%branch(pobs%branchIdx)
         read(ibin) pobs%offset

      enddo
      
      call read_hash_list_cache(ibin, network%obs%hashlist)
         
   end subroutine read_obs_point_cache
   
   subroutine write_obs_point_cache(ibin, obs)
   
      type(t_ObservationPointSet), intent(in)  :: obs
      integer, intent(in)                      :: ibin
      
      integer                           :: i
      type(t_ObservationPoint), pointer :: pobs
      
      write(ibin) obs%Count

      do i = 1, obs%Count
      
         pobs => obs%OPnt(i)

         write(ibin) pobs%id 
         write(ibin) pobs%name 
         write(ibin) pobs%p1
         write(ibin) pobs%p2
         write(ibin) pobs%pointWeight          
         write(ibin) pobs%l1
         write(ibin) pobs%l2
         write(ibin) pobs%linkWeight
         write(ibin) pobs%branchIdx
         write(ibin) pobs%offset
        
      enddo
      
      call write_hash_list_cache(ibin, obs%hashlist)
      
   end subroutine write_obs_point_cache
   
   
end module m_readObservationPoints
