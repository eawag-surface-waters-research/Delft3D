!> Define the lateral discharges
module m_Laterals
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

   use m_node
   use m_branch
   use m_1d_structures
   use m_tables
   use m_hash_search
   use MessageHandling
   
   implicit none
   
   private
   
   public getQlat
   public getLoad
   public getGridPoint
   public setQlat
   public setQlatQh
   public realloc
   public dealloc
   public lookupGridPoint
   
   public setLateralIncrement

   public fill_hashtable
   
   interface fill_hashtable
      module procedure fill_hashtable_lts
   end interface 

   !> Get the closest gridpoint to the Lateral discharge
   interface getGridPoint
      module procedure getLatGridPoint
   end interface

   !> Realloc a derived type for a lateral discharge
   interface realloc
      module procedure reallocLateral
   end interface

   !> Dealloc a derived type for a lateral discharge
   interface dealloc
      module procedure deallocLateral
   end interface dealloc

   !> Derived type to contain the attributes for a lateral discharge
   type, public :: t_lateral
      character(IdLen)                             :: id           !< Identification of the lateral discharge
      integer, allocatable, dimension(:)           :: branch       !< array of reaches
      double precision, allocatable, dimension(:)  :: beginChainage  !< begin locatie of lateral discharge along reach 
      double precision, allocatable, dimension(:)  :: endChainage    !< End location of lateral discharge along reach
      
      double precision                             :: discharge    !< Discharge value
      double precision, dimension(2)               :: concentration !< Concentration value
      double precision, allocatable, dimension(:)  :: distributedDischarge 
                                                                   !> Indicates whether lateral is a fresh water source: \n
                                                                   !! - at inflow salinity concentration will be set to 0 \n
                                                                   !! - at outflow load will be set to 0.
      logical                                      :: freshWater
      logical                                      :: pointLateral
     
      ! calculation info
      integer                                      :: pointsCount
      integer, allocatable, dimension(:)           :: brnum
      integer, allocatable, dimension(:)           :: gridPoints
      double precision, allocatable, dimension(:)  :: weights
      type(t_table), pointer                       :: qh => null()
      logical                                      :: use_internal_discharge
      logical                                      :: use_internal_salinity
      
      ! Results Calculated when writing HIS-File, can be used when writing NetCDF-File
      double precision                             :: actualDischarge
      double precision                             :: definedDischarge
      double precision                             :: differenceDischarge
   end type  
   
   !> Set of lateral discharges
   type, public :: t_lateralSet
      integer                                               :: Size = 0       !< 
      integer                                               :: growsBy = 2000
      integer                                               :: Count= 0       !< Current number of lateral discharges
      type(t_lateral), pointer, dimension(:)                :: lat
      type(t_hashlist)                                      :: hashlist
   end type t_lateralSet

   double precision, parameter                              :: minDeltaX = 0.01d0
   
contains
   
   subroutine admin_laterals(lts)
      type(t_lateralSet), intent(inout) :: lts
      
      
      
   end subroutine admin_laterals
   

   !> Set the increment in lateral discharges
   subroutine setLateralIncrement(lts, increment)
      type(t_lateralSet)  :: lts        !< Set of lateral discharges
      integer, intent(in) :: increment  !< New growsby factor for the lateral discahrge set
      
      lts%growsBy = increment
   end subroutine setLateralIncrement
    
   !> Get the location of thelateral discharge
   integer function getLatGridPoint(pLat, iseq)
      integer, intent(in)           :: iseq !< 
      type(t_lateral), intent(in)   :: pLat !< Current lateral discharge
      
      getLatGridPoint  = plat%gridPoints(iseq)
   end function getLatGridPoint

   !> Get the lateral discharge for a specific lateral source and at a specified grid location (relative in lateral)
   double precision function getQlat(pLat, iseq, level)
      type(t_lateral), intent(in)   :: pLat     !< Current lateral discharge
      integer, intent(in)           :: iseq     !< Sequence number for grid location in lateral source
      double precision              :: level    !< Water level at specified location
      
      
      if (associated(pLat%qh) ) then
         getQlat = pLat%weights(iseq)*interpolate(pLat%qh, level)
      else
         getQlat = pLat%weights(iseq)*pLat%discharge
      endif
      
   end function getQlat

   !> Get the salinity load for a specific lateral source and at a specified grid location (relative in lateral).\n
   !! When QLAT < 0 and freshwater is false then the ambient salinity is used. 
   !! In this case getLoad will be set to QLAT.
   double precision function getLoad(pLat, iseq, icon, qlat)
      type(t_lateral), intent(in)   :: pLat     !< Current lateral discharge
      integer, intent(in)           :: iseq     !< Sequence number for grid location in lateral source
      integer, intent(in)           :: icon     !< sequence number of constituent
      double precision              :: qlat     !< Discharge of lateral source at specified location
      
      if (.not. pLat%freshWater) then
         if (qlat > 0) then
            getLoad = qlat*pLat%concentration(icon)
         else
            getLoad = qlat
         endif
      else
         getLoad = 0
      endif
   end function getLoad
   
   
   
   !> Set the discharge value
   subroutine setQlat(pLat, qlat)
      type(t_lateral)   :: pLat !< Current lateral discharge
      double precision  :: qlat
      
      pLat%discharge = qlat
   end subroutine setQlat
   
   !> Set levels and discharges, QH-relation
   subroutine setQlatQh(pLat, levels, discharge, length)
      type(t_lateral)   :: pLat   !< Current lateral discharge
      integer           :: length !< Array length of levels and discharges
      double precision, dimension(length)      :: levels
      double precision, dimension(length)      :: discharge
      
      call setTable(pLat%qh, 0, levels, discharge, length)
   end subroutine setQlatQh
   
   !> Look for the gridpoints before and after the lateral discharge location and give the Chainage for both on the reach
   subroutine lookupGridPoint(brs, ChainageLateral, branch, goforward, ipoint1, ipoint2, Chainage1, Chainage2)
      ! Modules
      
      implicit none
      ! Input/output parameters
      type(t_branchSet)                   :: brs
      double precision, intent(in)        :: ChainageLateral
      integer, intent(in)                 :: branch
      logical, intent(in)                 :: goforward
      integer, intent(out)                :: ipoint1
      integer, intent(out)                :: ipoint2
      double precision, intent(out)       :: Chainage1
      double precision, intent(out)       :: Chainage2
      
      ! Local variables
      type(t_branch)                      :: pBran
      
      ! Program code
      
      pBran = brs%branch(branch)

      if (goforward) then
         
         ipoint2 = 1
         do while ( (ipoint2 < pBran%gridPointsCount) .and.             &
                    (chainageLateral > pBran%gridPointschainages(ipoint2)) )
            ipoint2 = ipoint2 + 1
         enddo
         if (chainagelateral >= pBran%gridPointschainages(ipoint2) ) then
            ipoint1 = max(1, ipoint2)
         else
            ipoint1 = max(1, ipoint2 - 1)
         endif
         
      else
         
         ipoint2 = pBran%gridPointsCount
         do while ( (ipoint2 > 1) .and.             &
                    (chainageLateral < pBran%gridPointschainages(ipoint2)) )
            ipoint2 = ipoint2 - 1
         enddo
         if (chainagelateral <= pBran%gridPointschainages(ipoint2) ) then
            ipoint1 = min(pBran%gridPointsCount, ipoint2)
         else
            ipoint1 = min(pBran%gridPointsCount, ipoint2 + 1)
         endif
         
      endif
      
      ! check whether start point is boundary node
      if (ipoint1 == 1) then
         if (brs%branch(branch)%fromnode%nodetype >= nt_LevelBoun) then
            ipoint1 = 2
            ipoint2 = max(2, ipoint2)
         endif
      endif
      ! check whether end point is boundary node
      if (ipoint2 == pBran%gridPointsCount) then
         if (brs%branch(branch)%tonode%nodetype >= nt_LevelBoun) then
            ipoint2 = ipoint2-1
            ipoint1 = min(ipoint2, ipoint1)
         endif
      endif
   
      chainage1 = pBran%gridPointschainages(ipoint1)
      chainage2 = pBran%gridPointschainages(ipoint2)
      
      ! change sequence number into SOBEK gridpoint number
      ipoint1 = pBran%Points(1) + ipoint1 - 1
      ipoint2 = pBran%Points(1) + ipoint2 - 1

   end subroutine lookupGridPoint
   
   subroutine deallocLateral(lts)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_lateralSet), intent(inout) :: lts
      
      ! Local variables
      integer                       :: i
   
      ! Program code
      if (associated(lts%lat)) then
         
         do i = 1, lts%count
            if (allocated(lts%lat(i)%branch))                deallocate(lts%lat(i)%branch)
            if (allocated(lts%lat(i)%beginChainage))           deallocate(lts%lat(i)%beginChainage)
            if (allocated(lts%lat(i)%endChainage))             deallocate(lts%lat(i)%endChainage)
            if (allocated(lts%lat(i)%distributedDischarge))  deallocate(lts%lat(i)%distributedDischarge)
            if (allocated(lts%lat(i)%brnum))                 deallocate(lts%lat(i)%brnum)
            if (allocated(lts%lat(i)%gridPoints))            deallocate(lts%lat(i)%gridPoints)
            if (allocated(lts%lat(i)%weights))               deallocate(lts%lat(i)%weights)
            
            if (associated(lts%lat(i)%qh)) then
               call dealloc(lts%lat(i)%qh)
               lts%lat(i)%qh => null()
            endif
         enddo
         deallocate(lts%lat)
      endif
      
      lts%lat   => null()
      lts%count = 0
      lts%size  = 0
         
      call dealloc(lts%hashlist)
      
   end subroutine
!
!
subroutine reallocLateral(lts)
   ! Modules
   
   implicit none
   ! Input/output parameters
   type(t_lateralSet), intent(inout)      :: lts
   
   ! Local variables
   type(t_lateral), pointer, dimension(:) :: oldLats
   
   ! Program code
   
   if (lts%size > 0) then
      oldLats=>lts%lat
   endif
   
   if (lts%growsBy <=0) then
      lts%growsBy = 200
   endif
   allocate(lts%lat(lts%size+lts%growsBy))
   
   if (lts%size > 0) then
      lts%lat(1:lts%size) = oldLats(1:lts%size)
      deallocate(oldLats)
   endif
   lts%size = lts%size+lts%growsBy
end subroutine

subroutine fill_hashtable_lts(lts)
   
   type (t_lateralSet), intent(inout), target   :: lts
      
   integer                                      :: ilt
   character(len=idlen), dimension(:), pointer  :: ido
      

   if (lts%Count <= 0) return    ! Nothing to hash

   allocate(lts%hashlist%id_list(lts%Count))
   lts%hashlist%id_count = lts%Count
   ido => lts%hashlist%id_list
      
   do ilt = 1, lts%count
      ido(ilt) = lts%lat(ilt)%id
   enddo
      
   call hashfill(lts%hashlist)
      
end subroutine fill_hashtable_lts


end module m_Laterals
