module m_1d_structures
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
   use m_GlobalParameters
   use m_alloc
   use m_branch
   use m_tables
   use m_CrossSections
   use m_Weir
   use m_Culvert
   use m_pump
   use m_Orifice
   use m_General_Structure
   use m_Universal_Weir
   use m_Bridge
   use m_ExtraResistance
   use m_hash_search
   use m_Dambreak
   use iso_c_utils

   implicit none

   private

   public realloc
   public dealloc

   public addStructure
   public getStructureCount
   public setRestartDataForStructures
   public setValue
   public getValue
   public reIndexCrossSections
   public getStrucType_from_string

   public getTableValue
   public getCrossSection
   public getStructureById
   public GetStrucType_from_int
   public get_crest_level
   public get_crest_level_c_loc
   public get_width
   public get_watershed_threshold
   public get_gle
   public get_opening_height
   public get_valve_opening
   public fill_hashtable
   public set_crest_level
   public set_crest_width
   public set_gle
   public set_opening_height
   public set_valve_opening
   public set_capacity
   public incStructureCount
   public GetPumpCapacity
   public GetPumpStage
   public GetPumpReductionFactor
   
   public initialize_structure

   public printData

   interface fill_hashtable
      module procedure fill_hashtable_sts
   end interface 
   
   interface AddStructure
      module procedure AddStructure_short
      module procedure AddStructureByCalcPoints
   end interface

   interface getTableValue
      module procedure getTableValueStruc
   end interface

   interface SetValue
      module procedure setValueStruc
   end interface

   interface GetValue
      module procedure getValueStruc
   end interface

   interface realloc
      module procedure reallocstructure
      module procedure reallocCompound
      module procedure reallocForcingList
   end interface

   interface dealloc
      module procedure deallocstructure
      module procedure deallocCompound
      module procedure deallocForcingList
   end interface dealloc

   ! TODO: the next declarations are duplicates of OMI_CF_DATA.
   integer, public, parameter :: CFiCrestLevel         = 18
   integer, public, parameter :: CFiCrestWidth         = 19
   integer, public, parameter :: CFiGateLowerEdgeLevel = 20
   integer, public, parameter :: CFiGateOpeningHeight  = 21
   integer, public, parameter :: CFiValveOpening       = 22
   integer, public, parameter :: CFiSetpoint           = 29
   integer, public, parameter :: CFiHighestParameter   = 31

    !---------------------------------------------------------
   type, public :: t_structure
      character(IdLen)                 :: id             !< Id of the structure
      character(IdLen)                 :: name           !< (long) name of the structure
      integer                          :: type           !< integer structure type
      integer                          :: ibran          !< branch index
      double precision                 :: chainage       !< Chainage
      integer                          :: numCoordinates !< number of coordinates in the location polygon
      double precision, pointer, dimension(:)   :: xCoordinates   !< x-coordinates of the location polygon
      double precision, pointer, dimension(:)   :: yCoordinates   !< y-coordinates of the location polygon
      
      integer                          :: numlinks       !< number of links in structure
      integer, pointer, dimension(:)   :: linknumbers    !< link numbers of structure (length = numlinks)
      integer                          :: state = -1     !< State of the Structure for General Structure, Weir, Orifice and Culvert/Siphon
                                                         !< 0 = No Flow
                                                         !< 1 = Free Weir Flow
                                                         !< 2 = Drowned Weir Flow
                                                         !< 3 = Free Gate Flow
                                                         !< 4 = Drowned Gate Flow
                                                         !< 5 = Free Flow for Culvert and Siphons
                                                         !< 6 = Drowned Flow for Culvert and Siphons
      integer                          :: compound
      character(IdLen)                 :: compoundName = ' '
      type(t_weir), pointer            :: weir => null()
      type(t_orifice), pointer         :: orifice => null()
      type(t_pump), pointer            :: pump => null()
      type(t_culvert),pointer          :: culvert => null()
      type(t_uni_weir),pointer         :: uniweir => null()
      type(t_bridge),pointer           :: bridge => null()
      type(t_GeneralStructure),pointer :: generalst => null()
      type(t_ExtraResistance),pointer  :: extrares => null()
      type(t_dambreak),pointer         :: dambreak => null()
   end type

   type, public :: t_structureSet
      integer                                               :: Size               = 0
      integer                                               :: growsBy            = 2000
      integer                                               :: Count              = 0 !< Current number of structures in this set.
      integer, dimension(ST_MAX_TYPE)                       :: countByType        = 0
      integer                                               :: compoundCount      = 0
      logical                                               :: hasExtraResistance = .false.
      type(t_structure), pointer, dimension(:)              :: struct
      !> Contains information on
      real, dimension(:,:), allocatable                     :: restartData
      type(t_hashlist)                                      :: hashlist_weir
      type(t_hashlist)                                      :: hashlist_culvert
      type(t_hashlist)                                      :: hashlist_bridge
      type(t_hashlist)                                      :: hashlist_pump
      integer                                               :: numWeirs
      integer                                               :: numCulverts
      integer                                               :: numPumps
      integer                                               :: numBridges
      integer                                               :: numOrifices
      integer                                               :: numGates
      integer                                               :: numGeneralStructures
      integer, pointer, dimension(:)                        :: weirIndices
      integer, pointer, dimension(:)                        :: culvertIndices 
      integer, pointer, dimension(:)                        :: pumpIndices
      integer, pointer, dimension(:)                        :: bridgeIndices
      integer, pointer, dimension(:)                        :: orificeIndices
      integer, pointer, dimension(:)                        :: gateIndices
      integer, pointer, dimension(:)                        :: generalStructureIndices
   end type t_structureSet

   type, public :: t_compound
      character(IdLen)               :: id
      character(IdLen)               :: compoundName
      integer nrstruc
      !    integer, dimension(:), pointer :: struct ! vooralsnog niet nodig
      integer gridpoint1
      integer gridpoint2
      integer istru               !< help variable to remove compounds with one structure
      logical cleared             ! To Check if compound is still cleared
      double precision totalDischarge
      double precision elementDischarge      !< contains the discharge of the first element of the compound structure (which is located on the branch link of the network)
      double precision totalVelocity
      double precision elementVelocity       !< contains the velocity of the first element of the compound structure (which is located on the branch link of the network)
      double precision water_level_up
      double precision water_level_down
      double precision head
      double precision difu
      double precision area
   end type t_compound

   type, public :: t_compoundSet
      integer                                               :: Size     = 0
      integer                                               :: growsBy = 2000
      integer                                               :: Count    = 0
     type(t_compound), pointer, dimension(:)                :: compound
   end type t_compoundSet

   !> Data type to store user input for structure forcings, to be processed later by a kernel.
   !! For example, a pump's capacity may be prescribed by a time series in a .bc file.
   !! The flow1d structure reader only reads all user-supplied input, and later it is up to
   !! the calling kernel to initialize that forcing provider.
   type, public :: t_forcing
      character(IdLen)                 :: st_id      !< The structure's character Id.
      integer                          :: st_type    !< Structure type (e.g., ST_PUMP).
      character(IdLen)                 :: param_name !< Name of the structure's parameter that this forcing data is for.
      double precision, pointer        :: targetptr  !< Pointer to scalar variable in which the provided
                                                     !< parameter value(s) can later be stored.
                                                     !< For example => pump%capacity.
      character(Charln)                :: filename   !< Name of file that contains the forcing data (e.g., a time series file).
   end type

   !> An ordered list of structure forcing items.
   type, public :: t_forcingList
      integer                                :: Size     = 0  !< Current maximum size of the forcing list.
      integer                                :: growsBy  = 20 !< increment upon each realloc call.
      integer                                :: Count    = 0  !< Current actual number of items in the forcing list.
      type(t_forcing), pointer, dimension(:) :: forcing       !< actual forcing list.
   end type

   contains
   
   integer function AddStructure_short(sts, leftcalc, rightcalc, linknumber, icompound, compoundName, id, structureType)
      ! Modules
   
      implicit none
   
      ! Input/output parameters
      type(t_StructureSet) :: sts
      integer              :: leftcalc
      integer              :: rightcalc
      integer              :: linknumber
      integer              :: icompound
      character(*)         :: compoundName
      character(*)         :: id
      ! In 3Di branches have both xy and branchid, chainage
   
      integer              :: structureType
   
      ! Local variables
      integer :: ibranch
      double precision :: x
      double precision :: y
      double precision :: chainage
      
   
      ! Program code
      chainage = 0d0
      x = 0d0
      y = 0d0
      ibranch = 0
      AddStructure_short = AddStructureByCalcPoints(sts, linknumber, chainage, icompound, compoundName, id, structureType, x, y, ibranch)
   end function AddStructure_short

   integer function AddStructureByCalcPoints(sts, linknumber, chainage, icompound, compoundName, id, structureType, x, y, ibranch)
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_StructureSet) :: sts
      integer              :: linknumber
      double precision     :: chainage
      integer              :: icompound
      character(*)         :: compoundName
      character(*)         :: id
      double precision, optional :: x
      double precision, optional :: y
      integer, optional :: ibranch
      ! In 3Di branches have both xy and branchid, chainage

      integer              :: structureType

      ! Local variables
      integer              :: i, j

      type(t_structure), pointer       :: pstru

      ! Program code
      sts%Count = sts%Count+1
      i = sts%Count
      if (sts%Count > sts%Size) then
         call realloc(sts)
      endif
      call incStructureCount(sts, structureType)

      allocate(sts%struct(i)%linknumbers(1), sts%struct(i)%xCoordinates(1), sts%struct(i)%yCoordinates(1))
      
      sts%struct(i)%id                 = id
      sts%struct(i)%linknumbers(1)     = linknumber
      sts%struct(i)%chainage           = chainage
      sts%struct(i)%compound           = icompound
      sts%struct(i)%compoundName       = compoundName
      sts%struct(i)%type            = structureType
      if (present(x) .and. present(y)) then
         sts%struct(i)%xCoordinates(1) = x
         sts%struct(i)%yCoordinates(1) = y
      else
         sts%struct(i)%xCoordinates(1) = 0d0
         sts%struct(i)%yCoordinates(1) = 0d0
      endif
      if (present(ibranch)) then
         sts%struct(i)%ibran = ibranch
      else
         sts%struct(i)%ibran = 0
      end if

      AddStructureByCalcPoints = sts%count
   end function AddStructureByCalcPoints

   !> Increments the counter for a specific type in the overall structure set.
   subroutine incStructureCount(sts, type)
      implicit none
      type(t_StructureSet), intent(inout) :: sts
      integer,              intent(in)    :: type !< Type id of the new structure.

      if (type > ST_MAX_TYPE) then
         call mess(LEVEL_ERROR, 'incStructureCount: invalid structure type: ', type)
         return
      end if

      sts%countByType(type) = sts%countByType(type) + 1
   end subroutine incStructureCount


   !> Gets the number of structures of a specific type.
   integer function getStructureCount(sts, type)
      implicit none
      type(t_StructureSet), intent(in) :: sts
      integer,              intent(in) :: type

      getStructureCount = 0
      if (type > ST_MAX_TYPE) then
         call mess(LEVEL_ERROR, 'incStructureCount: invalid structure type: ', type)
         return
      end if

      getStructureCount = sts%countByType(type)
   end function getStructureCount

subroutine deallocstructure(sts)
   ! Modules

   implicit none

   ! Input/output parameters
   type(t_structureSet), intent(inout)          :: sts

   ! Local variables
   integer                 :: i
   integer                 :: length

   ! Program code
   if (associated(sts%struct)) then
      length = sts%size
      do i = 1, length
         if (associated(sts%struct(i)%weir))       deallocate(sts%struct(i)%weir)
         if (associated(sts%struct(i)%orifice))    deallocate(sts%struct(i)%orifice)
         if (associated(sts%struct(i)%pump))       call dealloc(sts%struct(i)%pump)
         if (associated(sts%struct(i)%culvert))    call dealloc(sts%struct(i)%culvert)
         if (associated(sts%struct(i)%uniweir))    call dealloc(sts%struct(i)%uniweir)
         if (associated(sts%struct(i)%bridge))     deallocate(sts%struct(i)%bridge)
         if (associated(sts%struct(i)%generalst))  call dealloc(sts%struct(i)%generalst)
         if (associated(sts%struct(i)%extrares))   call dealloc(sts%struct(i)%extrares)
         if (associated(sts%struct(i)%xCoordinates)) deallocate(sts%struct(i)%xCoordinates)
         if (associated(sts%struct(i)%yCoordinates)) deallocate(sts%struct(i)%yCoordinates)
         if (associated(sts%struct(i)%linknumbers )) deallocate(sts%struct(i)%linknumbers )
         
         sts%struct(i)%weir      => null()
         sts%struct(i)%orifice   => null()
         sts%struct(i)%pump      => null()
         sts%struct(i)%culvert   => null()  
         sts%struct(i)%uniweir   => null() 
         sts%struct(i)%bridge    => null() 
         sts%struct(i)%generalst => null()
         sts%struct(i)%extrares  => null()
         sts%struct(i)%xCoordinates => null()
         sts%struct(i)%yCoordinates => null()
         sts%struct(i)%linknumbers  => null()
      enddo
      deallocate(sts%struct)
   endif

   if (allocated(sts%restartData)) then
      deallocate(sts%restartData)
   endif
   
   call dealloc(sts%hashlist_weir)
   call dealloc(sts%hashlist_pump)
   call dealloc(sts%hashlist_bridge)
   call dealloc(sts%hashlist_culvert)

   sts%struct       => null()
   sts%count         = 0
   sts%size          = 0
   sts%countByType   = 0
   sts%compoundcount = 0
   sts%hasExtraResistance = .false.

end subroutine deallocstructure
!
subroutine deallocCompound(cmps)
   ! Modules

   implicit none

   ! Input/output parameters
   type(t_compoundSet), intent(inout)          :: cmps

   ! Local variables

   ! Program code
   if (associated(cmps%compound)) then
      deallocate(cmps%compound)
   endif
   
   cmps%compound => null()
   cmps%size  = 0
   cmps%count = 0

end subroutine
!
!
   subroutine reallocstructure(sts)
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_structureSet), intent(inout)          :: sts

      ! Local variables
      type(t_structure), pointer, dimension(:)     :: oldsts

      ! Program code

      if (sts%Size > 0) then
         oldsts=>sts%struct
      endif

      if (sts%growsBy <=0) then
         sts%growsBy = 200
      endif
      allocate(sts%struct(sts%Size+sts%growsBy))

      if (sts%Size > 0) then
         sts%struct(1:sts%Size) = oldsts(1:sts%Size)
         deallocate(oldsts)
      endif
      sts%Size = sts%Size+sts%growsBy
   end subroutine

   subroutine reallocCompound(cmps)
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_compoundSet), intent(inout)          :: cmps

      ! Local variables
      type(t_compound), pointer, dimension(:)      :: oldcmps

      ! Program code

      if (cmps%Size > 0) then
         oldcmps=>cmps%compound
      endif

      if (cmps%growsBy <=0) then
         cmps%growsBy = 200
      endif
      allocate(cmps%compound(cmps%Size+cmps%growsBy))

      if (cmps%Size > 0) then
         cmps%compound(1:cmps%Size) = oldcmps(1:cmps%Size)
         deallocate(oldcmps)
      endif
      cmps%Size = cmps%Size+cmps%growsBy
   end subroutine


!> Deallocates a forcing list and sets all counters to zero.
subroutine deallocForcingList(fs)
   ! Modules

   implicit none

   ! Input/output parameters
   type(t_forcingList), intent(inout) :: fs !< The forcing list.

   ! Local variables

   ! Program code
   if (associated(fs%forcing)) then
      deallocate(fs%forcing)
   endif
   
   fs%forcing => null()
   fs%size  = 0
   fs%count = 0

end subroutine
!
!

subroutine reallocForcingList(fs)
   ! Modules

   implicit none

   ! Input/output parameters
   type(t_forcingList), intent(inout)          :: fs

   ! Local variables
   type(t_forcing), pointer, dimension(:)      :: oldforcing

   ! Program code

   if (fs%Size > 0) then
      oldforcing=>fs%forcing
   endif

   if (fs%growsBy <=0) then
      fs%growsBy = 200
   endif
   allocate(fs%forcing(fs%Size+fs%growsBy))

   if (fs%Size > 0) then
      fs%forcing(1:fs%Size) = oldforcing(1:fs%Size)
      deallocate(oldforcing)
   endif
   fs%Size = fs%Size+fs%growsBy
end subroutine

   double precision function getTableValueStruc(pstru, x) result (res)
      double precision :: x
      type(t_structure) :: pstru

      select case (pstru%type)
      case (ST_CULVERT, ST_SIPHON, ST_INV_SIPHON)
          res = interpolate(pstru%culvert%lossCoeff, x)
      case default
          res = 0.0d0
      end select
   end function


   subroutine getCrossSection(sts, crs, istru, pcross)
      type(t_StructureSet)                :: sts
      type(t_CrossSectionSet)             :: crs
      integer                             :: istru
      type(t_crossSection), pointer       :: pcross

      integer           :: icross

      select case(sts%struct(istru)%type)
         case (ST_CULVERT, ST_SIPHON, ST_INV_SIPHON)
            icross = sts%struct(istru)%culvert%crosssectionnr
         case (ST_BRIDGE)
            icross = sts%struct(istru)%bridge%crosssectionnr
         case default
            icross = 0
      end select
      if (icross==0) then
         pcross => null()
      else
         pcross => crs%cross(icross)
      endif
   end subroutine getCrossSection

   !> Set structure parameter
   logical function setValueStruc(sts, istru, iparam, value)
      use m_GlobalParameters
      
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_structureSet)             :: sts         !< set containing structure data
      integer                          :: istru       !< structure sequence number
      integer                          :: iparam      !< parameter to be changed
      double precision                 :: value       !< new value

!
!
! Local variables
!
      character(CharLn)                   :: line
!
!
!! executable statements -------------------------------------------------------
!
       SetValueStruc = .true.
       if (iparam==CFiCrestWidth .and. value < 0.0) then
          line = 'The crest width for structure with id: '//trim(sts%struct(istru)%id) //' is less than zero.'
          call setMessage(LEVEL_ERROR, line)
          return
       endif

       select case (sts%struct(istru)%type)
       case (ST_WEIR)
          select case (iparam)
          case (CFiCrestLevel)
             sts%struct(istru)%weir%crestlevel=value
          case (CFiCrestWidth)
             sts%struct(istru)%weir%crestwidth=value
          case default
             SetValueStruc = .false.
          end select
       case (ST_ORIFICE)
          select case (iparam)
          case (CFiCrestLevel)
             sts%struct(istru)%orifice%crestlevel=value
          case (CFiCrestWidth)
             sts%struct(istru)%orifice%crestwidth=value
          case (CFiGateOpeningHeight)
             sts%struct(istru)%orifice%openlevel=value - sts%struct(istru)%orifice%crestlevel
          case (CFiGateLowerEdgeLevel)
             sts%struct(istru)%orifice%openlevel =value
          case default
             SetValueStruc = .false.
          end select
       case (ST_CULVERT, ST_SIPHON, ST_INV_SIPHON)
          if (iparam==CFiValveOpening) then
             sts%struct(istru)%culvert%valveOpening=value
          else
            SetValueStruc = .false.
          endif
       case (ST_PUMP)
          if (iparam==CFiPumpCapacity) then
             if (sts%struct(istru)%pump%capacity(1)*value < -1e-6) then
                ! The pump direction may not be changed.
                line = 'The pumping direction of pump '//trim(sts%struct(istru)%id) //' is changed. This is not allowed.'
                call setMessage(LEVEL_ERROR, line)
                return
             endif
             sts%struct(istru)%pump%isControlled = .true.
             sts%struct(istru)%pump%capacitySetpoint = value
           else
             SetValueStruc = .false.
           endif
       case (ST_GENERAL_ST)
          select case (iparam)
          case (CFiCrestLevel)
             sts%struct(istru)%generalst%zs=value
          case (CFiCrestWidth)
             sts%struct(istru)%generalst%ws=value
          case (CFiGateLowerEdgeLevel)
             sts%struct(istru)%generalst%gateLowerEdgeLevel =value
          case (CFiGateOpeningHeight)
             sts%struct(istru)%generalst%gateLowerEdgeLevel =value + sts%struct(istru)%generalst%zs
          case default
             SetValueStruc = .false.
          end select
       case default
         !nothing
       end select

       if (.not. allocated(sts%restartData).and. (sts%count > 0)) then
          allocate(sts%restartData(sts%count, CFiHighestParameter))
          sts%restartData = missingValue
       endif

       if (iparam <= CFiHighestParameter) then
          sts%restartData(istru, iparam) = value
       else
          SetValueStruc = .false.
       endif

   end function setValueStruc

   !> Get structure parameter
   double precision function getValueStruc(sts, istru, iparam)
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_structureSet)             :: sts         !< set containing structure data
      integer                          :: istru       !< structure sequence number
      integer                          :: iparam      !< parameter of interest
!
!
! Local variables
!
      character(CharLn)                   :: line
!
!
!! executable statements -------------------------------------------------------
!
       select case (sts%struct(istru)%type)
       case (ST_WEIR)
           if (iparam == CFiCrestLevel) getValueStruc = sts%struct(istru)%weir%crestlevel
           if (iparam == CFiCrestWidth) getValueStruc = sts%struct(istru)%weir%crestwidth
       case (ST_ORIFICE)
           if (iparam == CFiCrestLevel)         getValueStruc = sts%struct(istru)%orifice%crestlevel
           if (iparam == CFiCrestWidth)         getValueStruc = sts%struct(istru)%orifice%crestwidth
           if (iparam == CFiGateLowerEdgeLevel) getValueStruc = sts%struct(istru)%orifice%openlevel
           if (iparam == CFiGateOpeningHeight)  getValueStruc = sts%struct(istru)%orifice%openlevel - sts%struct(istru)%orifice%crestlevel
       case (ST_CULVERT)
           if (iparam == CFiCrestLevel)         getValueStruc = sts%struct(istru)%orifice%crestlevel
           if (iparam == CFiGateLowerEdgeLevel) getValueStruc = sts%struct(istru)%orifice%openlevel
           if (iparam == CFiGateOpeningHeight)  getValueStruc = sts%struct(istru)%orifice%openlevel - sts%struct(istru)%orifice%crestlevel
       case (ST_SIPHON, ST_INV_SIPHON)
           if (iparam == CFiGateOpeningHeight)  getValueStruc = sts%struct(istru)%orifice%openlevel - sts%struct(istru)%orifice%crestlevel
       case (ST_PUMP)
           getValueStruc = sts%struct(istru)%pump%capacitySetpoint
           if (sts%struct(istru)%pump%capacity(1)*getValueStruc < -1e-6) then
             ! The pump direction may not be changed.
             line = 'The pumping direction of pump '//trim(sts%struct(istru)%id) //' is changed. This is not allowed.'
             call setMessage(LEVEL_ERROR, line)
             return
           elseif (sts%struct(istru)%pump%capacity(1) < getValueStruc) then
             write(line, '(''The controlled capacity (='', g12.4, '') is higher than the maximum capacity (= '', g12.4, ''). The capacity is reduced to the maximum capacity'')' ) getValueStruc, sts%struct(istru)%pump%capacity(1)
             call setMessage(LEVEL_ERROR, line)
             getValueStruc = sts%struct(istru)%pump%capacity(1)
           endif
       case (ST_GENERAL_ST)
           if (iparam == CFiCrestLevel)         getValueStruc = sts%struct(istru)%generalst%zs
           if (iparam == CFiCrestWidth)         getValueStruc = sts%struct(istru)%generalst%ws
           if (iparam == CFiGateLowerEdgeLevel) getValueStruc = sts%struct(istru)%generalst%gateLowerEdgeLevel
           if (iparam == CFiGateOpeningHeight)  getValueStruc = sts%struct(istru)%generalst%gateLowerEdgeLevel - sts%struct(istru)%generalst%zs
       case default
         !nothing
       end select

       if (iparam == CFiCrestWidth .and. getValueStruc < 0.0) then
          line = 'The crest width for structure with id: '//trim(sts%struct(istru)%id) //' is less than zero.'
          call setMessage(LEVEL_ERROR, line)
          return
       endif

   end function getValueStruc

   subroutine reIndexCrossSections(sts, crs)
      ! modules

      implicit none
      ! variables
      type(t_structureSet)             :: sts       !< Current structure set
      type(t_CrossSectionSet)          :: crs       !< Current cross-section set
      ! local variables
      integer i
      !program code

      ! Check for structures with cross sections.
      ! since the cross section list is now sorted the locations are changed
      do i = 1, sts%count
         select case(sts%struct(i)%type)
            case(ST_CULVERT, ST_SIPHON, ST_INV_SIPHON)
               if (sts%struct(i)%culvert%crosssectionnr > 0) then
                  sts%struct(i)%culvert%crosssectionnr = crs%crossSectionIndex(sts%struct(i)%culvert%crosssectionnr)
                  sts%struct(i)%culvert%pcross => crs%cross(sts%struct(i)%culvert%crosssectionnr)
               endif
            case(ST_BRIDGE)
               if (sts%struct(i)%bridge%crosssectionnr > 0) then
                  sts%struct(i)%bridge%crosssectionnr = crs%crossSectionIndex(sts%struct(i)%bridge%crosssectionnr)
                  sts%struct(i)%bridge%pcross => crs%cross(sts%struct(i)%bridge%crosssectionnr)
               endif
         end select
      enddo

   end subroutine reIndexCrossSections

   subroutine SetRestartDataForStructures(sts)

      ! modules
      use m_globalParameters
      implicit none

      ! variables
      type(t_structureSet)             :: sts       !< Current structure set

      ! local variables
      integer iparam, istru
      logical success

      !program code
      success = .true.
      do istru = 1, sts%count
         do iparam = 1, CFiHighestParameter
            if (abs(sts%restartData(istru, iparam) - missingValue) > 1d0) then
               success = success .and. setValueStruc(sts, istru, iparam, dble(sts%restartData(istru, iparam)))
            endif
         enddo
      enddo
      if (.not. success) then
         call setMessage(LEVEL_FATAL,"INTERNAL ERROR: inconsistent restart data for RTC-controlled structure data")
      endif

   end subroutine SetRestartDataForStructures

   integer function GetStrucType_from_string(string)
      use properties
      
      character(len=*) :: string

      call lowercase(string, 999)
      select case(trim(string))
      case ('pump')
         GetStrucType_from_string = ST_PUMP
      case ('generalstructure')
         GetStrucType_from_string = ST_GENERAL_ST
      case ('weir')
         GetStrucType_from_string = ST_WEIR
      case ('orifice')
         GetStrucType_from_string = ST_ORIFICE
      case ('gate')
         GetStrucType_from_string = ST_GATE
      case ('culvert')
         GetStrucType_from_string = ST_CULVERT
      case ('siphon')
         GetStrucType_from_string = ST_SIPHON
      case ('inverted_siphon')
         GetStrucType_from_string = ST_INV_SIPHON
      case ('universal_weir')
         GetStrucType_from_string = ST_UNI_WEIR
      case ('dambreak')
         GetStrucType_from_string = ST_DAMBREAK
      case ('bridge')
         GetStrucType_from_string = ST_BRIDGE
      case default
         GetStrucType_from_string = -1
      end select
   end function GetStrucType_from_string


   subroutine GetStrucType_from_int(istrtype, strng)
      integer, intent(in) :: istrtype

      character (len=*), intent(out) :: strng
      
      select case(istrtype)
         case (ST_PUMP)
            strng = 'pump'
         case (ST_GENERAL_ST)
            strng = 'general_structure'
         case (ST_WEIR)
            strng = 'weir'
         case (ST_ORIFICE)
            strng = 'orifice'
         case (ST_GATE)
            strng = 'gate'
         case (ST_CULVERT)
            strng = 'culvert'
         case (ST_SIPHON)
            strng = 'siphon'
         case (ST_INV_SIPHON)
            strng = 'inverted_siphon'
         case (ST_UNI_WEIR)
            strng = 'universal_weir'
         case (ST_DAMBREAK)
            strng = 'dambreak'
         case (ST_BRIDGE)
            strng = 'bridge'
         case default
            strng = 'unknown'
      end select
   end subroutine GetStrucType_from_int

   function getStructureById(sts, id) result(pstru)
      type(t_structureSet), intent(in)    :: sts       !< Current structure set
      character(len=*), intent(in)        :: id
      type(t_structure), pointer          :: pstru

      integer :: istruc

      pstru => null()
      do istruc = 1, sts%count
         if (trim(sts%struct(istruc)%id) == trim(id)) then
            pstru => sts%struct(istruc)
         endif
      enddo
      ! not found: return -1

   end function getStructureById

   double precision function get_crest_level(struc)
      type(t_structure), intent(in) :: struc
      
       select case (struc%type)
          case (ST_WEIR)
             get_crest_level = struc%weir%crestlevel
          case (ST_UNI_WEIR)
             get_crest_level = struc%uniweir%crestlevel
          case (ST_ORIFICE)
             get_crest_level = struc%orifice%crestlevel
          case (ST_CULVERT, ST_SIPHON, ST_INV_SIPHON)
             get_crest_level = max(struc%culvert%leftlevel, struc%culvert%rightlevel)
          case (ST_PUMP)
             get_crest_level = huge(1d0)
          case (ST_GENERAL_ST)
             get_crest_level = struc%generalst%zs
          case default
             get_crest_level = huge(1d0)
       end select

   end function get_crest_level

   type(c_ptr) function get_crest_level_c_loc(struc)
      type(t_structure), intent(in) :: struc
      
       select case (struc%type)
          case (ST_WEIR)
             get_crest_level_c_loc = c_loc(struc%weir%crestlevel)
          case (ST_UNI_WEIR)
             get_crest_level_c_loc = c_loc(struc%uniweir%crestlevel)
          case (ST_ORIFICE)
             get_crest_level_c_loc = c_loc(struc%orifice%crestlevel)
          case (ST_GENERAL_ST)
             get_crest_level_c_loc = c_loc(struc%generalst%zs)
          case default
             get_crest_level_c_loc = C_NULL_PTR
       end select

   end function get_crest_level_c_loc   
   
   double precision function get_width(struc)
      type(t_structure), intent(in) :: struc
      
       select case (struc%type)
          case (ST_WEIR)
             get_width = struc%weir%crestwidth
          case (ST_GENERAL_ST)
             get_width = struc%generalst%ws
          case (ST_ORIFICE)
             get_width = struc%orifice%crestwidth
          case default
             get_width = huge(1d0)
       end select

   end function get_width


   !> Returns the threshold level for a structure that determines how it blocks incoming water levels.
   !! This can typically be used to initialize 1D water levels along branches, in between structures.
   !! Most structures have their watershed threshold identical to their crest level, but some (orifice)
   !! always keep the left and right levels separated.
   double precision function get_watershed_threshold(struc)
      type(t_structure), intent(in) :: struc
      
       select case (struc%type)
       case (ST_ORIFICE)
          get_watershed_threshold = huge(1d0)
       case default
          get_watershed_threshold = get_crest_level(struc)
       end select

   end function get_watershed_threshold
   
   double precision function get_gle(struc)
      
      type (t_structure), intent(inout) :: struc
      
      select case(struc%type)
      case (ST_ORIFICE)
         get_gle = struc%orifice%openlevel 
      case (ST_GENERAL_ST)
         get_gle = struc%generalst%gateLowerEdgeLevel
      case default
         get_gle = huge(1d0)
      end select
   end function get_gle
   
   double precision function get_opening_height(struc)
      
      type (t_structure), intent(inout) :: struc
      
      select case(struc%type)
      case (ST_ORIFICE)
         get_opening_height = struc%orifice%openlevel - struc%orifice%crestlevel
      case (ST_GENERAL_ST)
         get_opening_height = struc%generalst%gateLowerEdgeLevel - struc%generalst%zs
      end select
   end function get_opening_height
   
   double precision function get_valve_opening(struc)
      
      type (t_structure), intent(inout) :: struc
      
      select case(struc%type)
      case (ST_CULVERT, ST_SIPHON, ST_INV_SIPHON)
         get_valve_opening = struc%culvert%valveOpening
      end select
   end function get_valve_opening

   subroutine fill_hashtable_sts(sts)
   
      type (t_structureSet), intent(inout), target :: sts
      
      integer                                      :: ist
      character(len=idlen), dimension(:), pointer  :: ids_weir
      character(len=idlen), dimension(:), pointer  :: ids_culvert
      character(len=idlen), dimension(:), pointer  :: ids_bridge
      character(len=idlen), dimension(:), pointer  :: ids_pump
      
      if (sts%Count <= 0) return    ! Nothing to hash
      
      allocate(sts%hashlist_weir%id_list(sts%Count))
      allocate(sts%hashlist_culvert%id_list(sts%Count))
      allocate(sts%hashlist_bridge%id_list(sts%Count))
      allocate(sts%hashlist_pump%id_list(sts%Count))
      
      sts%hashlist_weir%id_count = sts%Count
      sts%hashlist_culvert%id_count = sts%Count
      sts%hashlist_bridge%id_count = sts%Count
      sts%hashlist_pump%id_count = sts%Count
      
      ids_weir => sts%hashlist_weir%id_list
      ids_culvert => sts%hashlist_culvert%id_list
      ids_bridge => sts%hashlist_bridge%id_list
      ids_pump => sts%hashlist_pump%id_list
      
      ids_weir    = ' '
      ids_culvert = ' '
      ids_bridge  = ' '
      ids_pump    = ' '
      
      
      do ist = 1, sts%count
         select case(sts%struct(ist)%type)
         case (ST_WEIR, ST_ORIFICE,ST_GENERAL_ST, ST_UNI_WEIR)
            ids_weir(ist) = sts%struct(ist)%id
         case (ST_CULVERT, ST_SIPHON, ST_INV_SIPHON)
            ids_culvert(ist) = sts%struct(ist)%id
         case (ST_BRIDGE)
            ids_bridge(ist) = sts%struct(ist)%id
         case (ST_PUMP)
            ids_pump(ist) = sts%struct(ist)%id
         end select            
      enddo
      
      call hashfill(sts%hashlist_weir   )
      call hashfill(sts%hashlist_culvert)
      call hashfill(sts%hashlist_bridge )
      call hashfill(sts%hashlist_pump   )
      
   end subroutine fill_hashtable_sts
   
   subroutine set_crest_level(struc, value)
   
      type (t_structure), intent(inout) :: struc
      double precision, intent(in)      :: value
   
      select case(struc%type)
      case (ST_WEIR)
         struc%weir%crestlevel=value
      case (ST_ORIFICE)
         struc%orifice%crestlevel=value
      case (ST_UNI_WEIR)
         struc%uniweir%crestlevel=value
      case (ST_GENERAL_ST)
         struc%generalst%zs=value
      case default
         !nothing
      end select
   end subroutine set_crest_level

   subroutine set_crest_width(struc, value)
      
      type (t_structure), intent(inout) :: struc
      double precision, intent(in)      :: value
      
      select case(struc%type)
      case (ST_WEIR)
         struc%weir%crestwidth=value
      case (ST_ORIFICE)
         struc%orifice%crestwidth=value
      case (ST_GENERAL_ST)
         struc%generalst%ws=value
      end select
   end subroutine set_crest_width

   subroutine set_gle(struc, value)
      
      type (t_structure), intent(inout) :: struc
      double precision, intent(in)      :: value
      
      select case(struc%type)
      case (ST_ORIFICE)
         struc%orifice%openlevel =value
      case (ST_GENERAL_ST)
         struc%generalst%gateLowerEdgeLevel =value
      end select
   end subroutine set_gle
   
   subroutine set_opening_height(struc, value)
      
      type (t_structure), intent(inout) :: struc
      double precision, intent(in)      :: value
      
      select case(struc%type)
      case (ST_ORIFICE)
         struc%orifice%openlevel=value + struc%orifice%crestlevel
      case (ST_GENERAL_ST)
         struc%generalst%gateLowerEdgeLevel =value + struc%generalst%zs
      end select
   end subroutine set_opening_height
   
   subroutine set_valve_opening(struc, value)
      
      type (t_structure), intent(inout) :: struc
      double precision, intent(in)      :: value
      
      select case(struc%type)
      case (ST_CULVERT, ST_SIPHON, ST_INV_SIPHON)
         struc%culvert%valveOpening=value
      end select
   end subroutine set_valve_opening
   
   subroutine set_capacity(struc, value)
      
      type (t_structure), intent(inout) :: struc
      double precision, intent(in)      :: value
      
      
      if (struc%pump%capacity(1)*value < -1e-6) then
         ! The pump direction may not be changed.
         msgbuf = 'The pumping direction of pump '//trim(struc%id) //' is changed. This is not allowed.'
         call setMessage(LEVEL_ERROR, msgbuf)
         return
      endif
      struc%pump%isControlled = .true.
      struc%pump%capacitySetpoint = value
   end subroutine set_capacity
   
   !> Gets pump capacity.
   double precision function GetPumpCapacity(stru)
      implicit none
      type(t_structure), intent(in)   :: stru !< Structure
         
      if (stru%type /= ST_PUMP) then
         return
      end if

      if (stru%pump%is_active) then
         GetPumpCapacity = stru%pump%direction * stru%pump%stage_capacity
      else
         GetPumpCapacity = 0d0
      end if

   end function GetPumpCapacity
   
   !> Gets pump actual stage.
   double precision function GetPumpStage(stru)
      implicit none   
      type(t_structure), intent(in)   :: stru !< Structure 

      if (stru%type /= ST_PUMP) then
         return
      end if
         
      GetPumpStage = dble(stru%pump%actual_stage)
      
   end function GetPumpStage
   
   !> Gets pump reduction factor.
   double precision function GetPumpReductionFactor(stru)
      implicit none   
      type(t_structure), intent(in)   :: stru !< Structure
         
      if (stru%type /= ST_PUMP) then
         return
      end if

      GetPumpReductionFactor = stru%pump%reduction_factor
   end function GetPumpReductionFactor
   
   subroutine initialize_structure(struct, numlinks, links, wu)

      type(t_structure),               intent(inout) :: struct
      integer,                         intent(in   ) :: numlinks
      integer, dimension(:),           intent(in   ) :: links
      double precision, dimension(:),  intent(in   ) :: wu
      
      allocate(struct%linknumbers(numlinks))
      struct%numlinks = numlinks
      struct%linknumbers = links(1:numlinks)
      
      select case(struct%type)
      case (ST_GENERAL_ST)
         allocate(struct%generalst%widthcenteronlink(numlinks), struct%generalst%gateclosedfractiononlink(numlinks))
         allocate(struct%generalst%fu(3,numlinks), struct%generalst%ru(3,numlinks), struct%generalst%au(3,numlinks))
         struct%generalst%fu = 0d0
         struct%generalst%ru = 0d0
         struct%generalst%au = 0d0
         call update_widths(struct%generalst, numlinks, links, wu)
      case (ST_CULVERT)
         if (numlinks > 1) then
            call setmessage(LEVEL_ERROR, 'Multiple links for culvert structures is not supported, check structure'//trim(struct%id))
         endif
      case default
         ! A reminder not to forget other structures that are added:
         call setMessage(LEVEL_ERROR, 'Internal error, this structure type is not (yet) implemented in initialize_structure')
      end select

   end subroutine initialize_structure
   
end module m_1d_structures
