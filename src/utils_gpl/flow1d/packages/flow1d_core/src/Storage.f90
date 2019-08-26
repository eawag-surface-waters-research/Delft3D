module m_Storage
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
   use m_alloc
   use m_tables
   use m_hash_search

   implicit none
   
   private 
     
   ! storage types
   integer, public, parameter :: nt_None        = 0
   integer, public, parameter :: nt_Reservoir   = 2
   integer, public, parameter :: nt_Closed      = 3
   integer, public, parameter :: nt_Loss        = 4
   
   public realloc
   public dealloc
   public create

   public addStorage
   public getVolume
   public getSurface
   public getbedLevel
   public getStreetLevel
   public getStorageType
   public noStreetStorage
   public printData
   public fill_hashtable

   interface printData
      module procedure printStorageSet
      module procedure printStorage
   end interface
   
   interface getbedLevel
      module procedure getbedLevelStorage
   end interface
   
   interface GetVolume
      module procedure GetVolumeByGridPoint
      module procedure GetVolumeByStorNode
   end interface GetVolume
   
   interface GetSurface
      module procedure GetSurfaceByGridPoint
      module procedure GetSurfaceByStorNode
   end interface GetSurface

   interface fill_hashtable
      module procedure fill_hashtable_sto
   end interface 

   interface create
      module procedure createStorage
   end interface

   interface realloc
      module procedure reallocStorage
   end interface

   interface dealloc
      module procedure deallocStorage
   end interface dealloc

    !---------------------------------------------------------
  
   type, public :: t_storage
      character(len=idlen)    :: id                      !< unique id of storage area
      character(len=idlen)    :: nodeId                  !< Node Id
      character(len=idlen)    :: name                    !< Long name in the user interface
      integer                 :: gridPoint               !< gridpoint index
      integer                 :: storageType             !< type of storage on street
                                                         !! 0: no storage
                                                         !! 2: reservoir storage
                                                         !! 3: closed manhole
      type(t_table), pointer  :: storageArea             !< table containing storage area and levels
      type(t_table), pointer  :: streetArea              !< table containing storage area and levels on street level
      logical                 :: useStreetStorage        !< flag indicating whether streetstorage is to be used
      double precision        :: x                       !< (optional) x-coordinate
      double precision        :: y                       !< (optional) y-cooridnate
      logical                 :: useTable                !< flag indicating whether table is to be used
   end type
   
   type, public :: t_storageSet
      integer                                               :: Size = 0
      integer                                               :: growsBy = 2000
      integer                                               :: Count= 0
      integer                                               :: Count_xy = 0 ! Number of storage nodes that are defined by x-, y-coordinates
      type(t_storage), pointer, dimension(:)                :: stor
      integer, dimension(:), allocatable                    :: mapping
      type(t_hashlist)                                      :: hashlist
   end type t_storageSet
   
contains

   subroutine createStorage(storS, nnode, ngrid)
      integer ngrid
      integer nnode
      type(t_storageSet)               :: storS
      
      allocate(storS%mapping(-nnode:ngrid))
      storS%mapping = 0
   end subroutine createStorage
    
   integer function addStorage(storS, id, gridPoint, storageType, levels, storageArea, interpolTypeStorage, lengthStorageArea,       &
                               levelsOnStreet, storageAreaOnStreet, interpolTypeStreet, lengthStreetArea)
      use m_node
      use m_branch
      
      ! Modules               
                              
      implicit none
      
      ! Input/output parameters
      type(t_storageSet)               :: storS
      character(len=idlen), intent(in) :: id
      integer, intent(in)              :: gridPoint
      integer, intent(in)              :: storageType
      integer, intent(in)              :: interpolTypeStorage
      integer, intent(in)              :: interpolTypeStreet
      integer, intent(in)              :: lengthStorageArea
      integer, intent(in)              :: lengthStreetArea
      
      double precision, dimension(lengthStorageArea)  :: levels
      double precision, dimension(lengthStorageArea)  :: storageArea
      double precision, dimension(:)                  :: levelsOnStreet
      double precision, dimension(:)                  :: storageAreaOnStreet
      
      ! Local variables
      integer istor
      character(len=CharLn) :: line

      ! Program code
      
      storS%Count = storS%Count+1
      istor = storS%Count
      if (storS%Count > storS%Size) then
         call realloc(storS)
      endif
      nullify(storS%stor(istor)%storageArea)
      nullify(storS%stor(istor)%streetArea)
      
      ! look for nodenumber, using gridpoint index
      if (gridPoint >= lbound(storS%mapping,1) .and. gridPoint <= ubound(storS%mapping,1)) then
         storS%mapping(gridpoint) = storS%Count
      else
         write(line, '(''Internal error: gridPoint in setStorage has value '', i5, '' values must be between 1 and '', i5)') gridPoint, size(storS%mapping) 
         call Setmessage(level_fatal, line)
      endif
      
      storS%stor(istor)%storageType = storageType
      storS%stor(istor)%id = id 
      ! Look for gridpoint index in network
      if (storageType /= nt_None) then
         storS%stor(istor)%gridPoint = gridPoint
         call setTable(storS%stor(istor)%storageArea, interpolTypeStorage, levels, storageArea, lengthStorageArea)
         if (lengthStreetArea/=0) then
            call setTable(storS%stor(istor)%streetArea,  interpolTypeStreet,  levelsOnStreet, storageAreaOnStreet, lengthStreetArea)
         endif
      endif
      addstorage = stors%count
   end function addStorage
   
   subroutine deallocStorage(storS)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_storageSet)            :: storS
      
      ! Local variables
      integer                       :: i
   
      ! Program code
      if (storS%count > 0) then
         if (associated(storS%stor)) then 
            do i = 1, storS%Count
               if (associated(storS%stor(i)%storageArea)) then
                  call dealloc(storS%stor(i)%storageArea)
                  storS%stor(i)%storageArea => null()
               endif
               if (associated(storS%stor(i)%streetArea)) then
                  call dealloc(storS%stor(i)%streetArea)
                  storS%stor(i)%streetArea => null()
               endif
            enddo
            deallocate(storS%stor)
         endif
      endif
      storS%stor => null()
      storS%Size  = 0
      storS%Count = 0
      if (allocated(storS%mapping)) then
         deallocate(storS%mapping)
      endif
      
      call dealloc(storS%hashlist)

   end subroutine
!
!
   subroutine reallocStorage(storS)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_storageSet), intent(inout)          :: storS
      
      ! Local variables
      type(t_storage), pointer, dimension(:)     :: oldStorS
      
      ! Program code
      
      if (storS%Size > 0) then
         oldStorS=>storS%stor
      endif
      
      if (storS%growsBy <=0) then
         storS%growsBy = 200
      endif
      allocate(storS%stor(storS%Size+storS%growsBy))
      
      if (storS%Size > 0) then
         storS%stor(1:storS%Size) = oldstorS(1:storS%Size)
         deallocate(oldstorS)
      endif
      storS%Size = storS%Size+storS%growsBy
   end subroutine
   
   
   double precision function getSurfaceByGridpoint(storS, gridpoint, level)
      ! Modules
   
      implicit none
      ! Input/output parameters
      type(t_storageSet), intent(in)         :: stors
      integer, intent(in)                    :: gridpoint
      double precision, intent(in)           :: level

      integer     istor
      type(t_storage), pointer         :: storage
      ! Program code
      !         Check if storage on street is to be calculated:
   
      istor = storS%mapping(gridpoint)
      if (istor/=0) then
         storage=>stors%stor(istor)
         getSurfaceByGridpoint = getSurfaceByStorNode(storage, level)
      else
         getSurfaceByGridpoint = 0d0
      endif   
   end function getSurfaceByGridpoint
   
   double precision function getSurfaceByStorNode(storage, level)
      ! Modules
   
      implicit none
      ! Input/output parameters
      type(t_storage), intent(in)            :: storage
      double precision, intent(in)           :: level

      if (storage%useStreetStorage ) then
         ! check if water level is above street level
         if (level >= storage%streetArea%x(1) ) then
            getSurfaceByStorNode = interpolate(storage%streetArea, level)
            ! finished
            return
         endif
      endif
      ! else : calculate well storage:
      if (storage%storageType /= nt_None .and. level >= storage%storageArea%x(1) ) then
         getSurfaceByStorNode = interpolate(storage%storageArea, level)
      else
         getSurfaceByStorNode = 0d0
      endif
      
   end function getSurfaceByStorNode

   
   double precision function getVolumeByGridPoint(storS, gridPoint, level)
      ! Modules

      implicit none
      ! Input/output parameters
      type(t_storageSet), intent(in)         :: stors
      integer, intent(in)                    :: gridpoint
      double precision, intent(in)           :: level

      ! Local variables
      integer     istor
   
      ! Program code

      istor = storS%mapping(gridpoint)
      if (istor/=0) then
         getVolumeByGridPoint = getVolumeByStorNode(storS%stor(istor), level)
      else
         getVolumeByGridPoint = 0d0
      endif

   end function getVolumeByGridPoint

   double precision function getVolumeByStorNode(storage, level)
      ! Modules

      implicit none
      ! Input/output parameters
      type(t_storage), intent(in)            :: storage
      double precision, intent(in)           :: level

      ! Local variables
      double precision  :: level2         !< level2 is the level to which the normal storageArea must be calculated, which is the street level or the actual water level
   
      ! Program code
      !         Check if storage on street is to be calculated:
   
      if (storage%storagetype/=nt_none) then
         level2 = level
         getVolumeByStorNode = 0d0
         if (storage%useStreetStorage ) then
            ! check if water level is above street level
            if (level > storage%streetArea%x(1)) then
               getVolumeByStorNode= integrate(storage%streetArea, level)
               level2 = storage%streetArea%x(1)
               ! finished
            endif
         endif
         
         ! else : calculate well storage:
         getVolumeByStorNode = getVolumeByStorNode+ integrate(storage%storageArea, level2)
      else 
         getVolumeByStorNode = 0d0
      endif
   end function getVolumeByStorNode

   subroutine noStreetStorage(storS, gridPoint)
      type(t_storageSet), intent(inout)  :: storS
      integer, intent(in)                :: gridpoint

      ! Local variables
      integer                          :: istor
      type(t_storage), pointer         :: storage
      double precision                 :: street_level

   
      istor = storS%mapping(gridpoint)
      if (istor/=0) then
         storage=>stors%stor(istor)
         if (associated(storage%streetArea)) then

            ! Set Area to 0.0 but keep street level
            street_level = storage%streetArea%x(1)

            deallocate(storage%streetArea%x)
            deallocate(storage%streetArea%y)

            allocate(storage%streetArea%x(1))
            allocate(storage%streetArea%y(1))

            storage%streetArea%x(1) = street_level
            storage%streetArea%y(1) = 0d0
            storage%streetArea%length = 1
            storage%streetArea%stCount = 0

         endif
      endif
   end subroutine noStreetStorage


   double precision function getbedLevelStorage(storS, gridPoint, default)
      type(t_storageSet), intent(in)  :: storS
      integer, intent(in)                    :: gridpoint

      ! Local variables
      integer     istor
      type(t_storage), pointer         :: storage
      double precision                 :: default     
   
      istor = storS%mapping(gridpoint)
      if (istor/=0) then
         storage=>stors%stor(istor)
         getbedLevelStorage = storage%storageArea%x(1)
      else
         getbedLevelStorage = default
      endif
   end function getbedLevelStorage


   double precision function getStreetLevel(storS, gridpoint)
      type(t_storageSet), intent(inout)  :: storS
      integer, intent(in)                :: gridpoint

      ! Local variables
      integer     istor
      type(t_storage), pointer         :: storage
   
      getStreetLevel = 99999
      istor = storS%mapping(gridpoint)
      if (istor/=0) then
         storage=>stors%stor(istor)
         if (associated(storage%streetArea)) then
            getStreetLevel = storage%streetArea%x(1)
         endif
      endif
   end function getStreetLevel

   integer function getStorageType(storS, gridpoint)
      type(t_storageSet)     :: storS
      integer                 :: gridpoint
   
      if (storS%mapping(gridpoint) /=0) then
         getStorageType = storS%stor(storS%mapping(gridpoint))%storageType
      else
         getStorageType = nt_none
      endif

   end function getStorageType

   subroutine fill_hashtable_sto(storS)
   
      type (t_storageSet), intent(inout), target   :: storS
      
      integer                                      :: ist
      character(len=idlen), dimension(:), pointer  :: ids
      
      allocate(storS%hashlist%id_list(storS%Count))
      storS%hashlist%id_count = storS%Count
      ids => storS%hashlist%id_list
      
      do ist = 1, storS%count
         ids(ist) = storS%stor(ist)%id
      enddo
      
      call hashfill(storS%hashlist)
      
   end subroutine fill_hashtable_sto
   
   subroutine printStorageSet(storS, unit)
      type(t_storageSet)         :: storS
      integer                    :: unit
   
      integer                    :: i
   
      write(unit, '(a)') ''
      write(unit, '(a)') 'Storage in nodes'
      write(unit, '(a)') '================'
   
      write(unit, '(''Number of storage nodes in network = '', i7)') stors%Count
      do i = 1, stors%Count
         write(unit, '(a)') ''
         write(unit, '(''Storage node: '', i7)') i
         call printData(storS%stor(i), unit)
      enddo
   end subroutine printStorageSet

   subroutine printStorage(storage, unit)
      type(t_storage)         :: storage
      integer                    :: unit
   
      write(unit, '(''Gridpoint location: '', i7, '' storageType = '', i3)') storage%gridPoint, storage%storageType
      if (associated(storage%storageArea)) then
         write(unit, '(''Storage area data:'')')
         call printData(storage%storageArea, unit)
      endif
      if (associated(storage%streetArea)) then
         write(unit, '(''Street storage data:'')')
         call printData(storage%streetArea, unit)
      endif
   end subroutine printStorage

   
end module m_Storage
