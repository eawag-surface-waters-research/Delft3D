module m_structure
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
   use m_Advanced_Weir
   use m_Bridge
   use m_River_Weir
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
   public getStrucType

   public getTableValue
   public getCrossSection
   public getStructureById
   public get_crest_level
   public get_crest_level_c_loc
   public get_width
   public get_watershed_threshold
   public get_gle
   public get_opening_height
   public get_valve_opening
   public get_capacity
   public fill_hashtable
   public set_crest_level
   public set_crest_width
   public set_gle
   public set_opening_height
   public set_valve_opening
   public set_capacity

   public printData

   interface fill_hashtable
      module procedure fill_hashtable_sts
   end interface 
   
   interface getStrucType
      module procedure getStrucType_from_string
      module procedure getStrucType_from_int
   end interface
   interface AddStructure
      module procedure AddStructure_short
      module procedure AddStructureByCalcPoints
      module procedure AddStructureByBranchLocation
   end interface

   interface printData
      module procedure printStrucureSet
      module procedure printStructure
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
   end interface

   interface dealloc
      module procedure deallocstructure
      module procedure deallocCompound
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
      character(IdLen)                 :: id
      character(IdLen)                 :: st_name
      integer                          :: st_type
      integer                          :: ibran
      integer                          :: left_calc_point
      integer                          :: right_calc_point
      integer                          :: link_number
      double precision                 :: x, y
      double precision                 :: distance
      double precision                 :: charHeight
      double precision                 :: charWidth
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
      type(t_riverweir),pointer        :: riverweir => null()
      type(t_advweir),pointer          :: advweir => null()
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
      double precision :: distcalc
      
   
      ! Program code
      distcalc = 0d0
      x = 0d0
      y = 0d0
      ibranch = 0
      AddStructure_short = AddStructureByCalcPoints(sts, leftcalc, rightcalc, linknumber, distcalc, icompound, compoundName, id, structureType, x, y, ibranch)
   end function AddStructure_short

   integer function AddStructureByCalcPoints(sts, leftcalc, rightcalc, linknumber, distcalc, icompound, compoundName, id, structureType, x, y, ibranch)
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_StructureSet) :: sts
      integer              :: leftcalc
      integer              :: rightcalc
      integer              :: linknumber
      double precision     :: distcalc
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

      sts%struct(i)%id                 = id
      sts%struct(i)%left_calc_point    = leftcalc
      sts%struct(i)%right_calc_point   = rightcalc
      sts%struct(i)%link_number        = linknumber
      sts%struct(i)%distance           = distcalc
      sts%struct(i)%compound           = icompound
      sts%struct(i)%compoundName       = compoundName
      sts%struct(i)%st_type            = structureType
      if (present(x) .and. present(y)) then
         sts%struct(i)%x = x
         sts%struct(i)%y = y
      else
         sts%struct(i)%x = 0d0
         sts%struct(i)%y = 0d0
      endif
      if (present(ibranch)) then
         sts%struct(i)%ibran = ibranch
      else
         sts%struct(i)%ibran = 0
      end if


      if (icompound == 0) then
         ! look up compound structure
         do j = 1, i-1
            pstru => sts%struct(j)
            if ( (pstru%left_calc_point == leftcalc) .and. (pstru%right_calc_point == rightcalc) ) then
!               if (pstru%compound==0) then
!                  sts%compoundCount = sts%compoundCount+1
!                  pstru%compound = sts%compoundCount
!               endif
               sts%struct(i)%compound = pstru%compound
               exit
            endif
         enddo
      endif
      AddStructureByCalcPoints = sts%count
   end function AddStructureByCalcPoints

   integer function AddStructureByBranchLocation(sts, brs, ibranch, dist, icompound, compoundName, id, structureType)
      ! Modules

      implicit none

      ! Input/output parameters
      integer              :: ibranch
      double precision                 :: dist
      character(*)         :: id

      integer              :: icompound
      character(*)         :: compoundName
      
      type(t_StructureSet) :: sts
      type(t_BranchSet)    :: brs
      integer              :: structureType
      ! Local variables
      integer              :: leftcalc
      integer              :: rightcalc
      integer              :: ilink

      double precision                 :: distcalc

      ! Program code
      call getCalcPoints(brs, ibranch, dist, leftcalc, rightcalc, ilink, distcalc)

      AddStructureByBranchLocation = AddStructureByCalcPoints(sts, leftcalc, rightcalc, ilink, distcalc, &
                                                              icompound, compoundName, id, structureType, ibranch = ibranch)
      
   end function AddStructureByBranchLocation


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
         if (associated(sts%struct(i)%riverweir))  call dealloc(sts%struct(i)%riverweir)
         if (associated(sts%struct(i)%advweir))    deallocate(sts%struct(i)%advweir)
         if (associated(sts%struct(i)%generalst))  deallocate(sts%struct(i)%generalst)
         if (associated(sts%struct(i)%extrares))   call dealloc(sts%struct(i)%extrares)
         
         sts%struct(i)%weir      => null()
         sts%struct(i)%orifice   => null()
         sts%struct(i)%pump      => null()
         sts%struct(i)%culvert   => null()  
         sts%struct(i)%uniweir   => null() 
         sts%struct(i)%bridge    => null() 
         sts%struct(i)%riverweir => null()
         sts%struct(i)%advweir   => null()
         sts%struct(i)%generalst => null()
         sts%struct(i)%extrares  => null()
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

   double precision function getTableValueStruc(pstru, x) result (res)
      double precision :: x
      type(t_structure) :: pstru

      select case (pstru%st_type)
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

      select case(sts%struct(istru)%st_type)
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

       select case (sts%struct(istru)%st_type)
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
             sts%struct(istru)%culvert%inivalveopen=value
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
       case (ST_RIVER_WEIR)
          select case (iparam)
          case (CFiCrestLevel)
             sts%struct(istru)%riverweir%crestlevel=value
          case (CFiCrestWidth)
             sts%struct(istru)%riverweir%crestwidth=value
          case default
             SetValueStruc = .false.
          end select
       case (ST_ADV_WEIR)
          select case (iparam)
          case (CFiCrestLevel)
             sts%struct(istru)%advweir%crestlevel=value
          case (CFiCrestWidth)
             sts%struct(istru)%advweir%totwidth=value
          case default
             SetValueStruc = .false.
          end select
       case (ST_GENERAL_ST)
          select case (iparam)
          case (CFiCrestLevel)
             sts%struct(istru)%generalst%levelcenter=value
          case (CFiCrestWidth)
             sts%struct(istru)%generalst%widthcenter=value
          case (CFiGateLowerEdgeLevel)
             sts%struct(istru)%generalst%gateheight =value
          case (CFiGateOpeningHeight)
             sts%struct(istru)%generalst%gateheight =value + sts%struct(istru)%generalst%levelcenter
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
       select case (sts%struct(istru)%st_type)
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
       case (ST_RIVER_WEIR)
           if (iparam == CFiCrestLevel) getValueStruc = sts%struct(istru)%riverweir%crestlevel
           if (iparam == CFiCrestWidth) getValueStruc = sts%struct(istru)%riverweir%crestwidth
       case (ST_ADV_WEIR)
           if (iparam == CFiCrestLevel) getValueStruc = sts%struct(istru)%advweir%crestlevel
           if (iparam == CFiCrestWidth) getValueStruc = sts%struct(istru)%advweir%totwidth
       case (ST_GENERAL_ST)
           if (iparam == CFiCrestLevel)         getValueStruc = sts%struct(istru)%generalst%levelcenter
           if (iparam == CFiCrestWidth)         getValueStruc = sts%struct(istru)%generalst%widthcenter
           if (iparam == CFiGateLowerEdgeLevel) getValueStruc = sts%struct(istru)%generalst%gateheight
           if (iparam == CFiGateOpeningHeight)  getValueStruc = sts%struct(istru)%generalst%gateheight - sts%struct(istru)%generalst%levelcenter
       case default
         !nothing
       end select

       if (iparam == CFiCrestWidth .and. getValueStruc < 0.0) then
          line = 'The crest width for structure with id: '//trim(sts%struct(istru)%id) //' is less than zero.'
          call setMessage(LEVEL_ERROR, line)
          return
       endif

   end function getValueStruc

   subroutine printStrucureSet(sts, unit)
      type(t_structureSet) sts
      integer unit

      integer i
      write(99, '(a)') ''
      write(99, '(a)') 'Structures'
      write(99, '(a)') '=========='
      do i = 1, sts%count
         write (unit, '(a)') ''
         write(unit, '(a, i8)') 'structure number', i
         call printData(sts%struct(i), unit)
      enddo
   end subroutine printStrucureSet

   subroutine printStructure(struc, unit)
      type(t_structure) struc
      integer unit

      write(unit, '(a, i5)') 'id = '//trim(struc%id)
      write(unit, '(''branch = '' , i7, '' left calc point = '', i7, ''right calc point = '', i7)')  &
                  struc%ibran, struc%left_calc_point, struc%right_calc_point
      write(unit, '(''distance = '', f10.3)') struc%distance
   end subroutine printStructure

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
         select case(sts%struct(i)%st_type)
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
      character(len=*) :: string

      select case(trim(string))
      case ('river_weir')
         GetStrucType_from_string = ST_RIVER_WEIR
      case ('advanced_weir')
         GetStrucType_from_string = ST_ADV_WEIR
      case ('pump')
         GetStrucType_from_string = ST_PUMP
      case ('general_structure')
         GetStrucType_from_string = ST_GENERAL_ST
      case ('weir')
         GetStrucType_from_string = ST_WEIR
      case ('orifice')
         GetStrucType_from_string = ST_ORIFICE
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


   character function GetStrucType_from_int(istrtype)
      integer :: istrtype

      select case(istrtype)
         case (ST_RIVER_WEIR)
            GetStrucType_from_int = 'river_weir'
         case (ST_ADV_WEIR)
            GetStrucType_from_int = 'advanced_weir'
         case (ST_PUMP)
            GetStrucType_from_int = 'pump'
         case (ST_GENERAL_ST)
            GetStrucType_from_int = 'general_structure'
         case (ST_WEIR)
            GetStrucType_from_int = 'weir'
         case (ST_ORIFICE)
            GetStrucType_from_int = 'orifice'
         case (ST_CULVERT)
            GetStrucType_from_int = 'culvert'
         case (ST_SIPHON)
            GetStrucType_from_int = 'siphon'
         case (ST_INV_SIPHON)
            GetStrucType_from_int = 'inverted_siphon'
         case (ST_UNI_WEIR)
            GetStrucType_from_int = 'universal_weir'
         case (ST_DAMBREAK)
            GetStrucType_from_int = 'dambreak'
         case (ST_BRIDGE)
            GetStrucType_from_int = 'bridge'
         case default
            GetStrucType_from_int = 'unknown'
      end select
   end function GetStrucType_from_int

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
      
       select case (struc%st_type)
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
          case (ST_RIVER_WEIR)
             get_crest_level = struc%riverweir%crestlevel
          case (ST_ADV_WEIR)
             get_crest_level = struc%advweir%crestlevel
          case (ST_GENERAL_ST)
             get_crest_level = struc%generalst%levelcenter
          case default
             get_crest_level = huge(1d0)
       end select

   end function get_crest_level

   type(c_ptr) function get_crest_level_c_loc(struc)
      type(t_structure), intent(in) :: struc
      
       select case (struc%st_type)
          case (ST_WEIR)
             get_crest_level_c_loc = c_loc(struc%weir%crestlevel)
          case (ST_UNI_WEIR)
             get_crest_level_c_loc = c_loc(struc%uniweir%crestlevel)
          case (ST_ORIFICE)
             get_crest_level_c_loc = c_loc(struc%orifice%crestlevel)
          case (ST_RIVER_WEIR)
             get_crest_level_c_loc = c_loc(struc%riverweir%crestlevel)
          case (ST_ADV_WEIR)
             get_crest_level_c_loc = c_loc(struc%advweir%crestlevel)
          case (ST_GENERAL_ST)
             get_crest_level_c_loc = c_loc(struc%generalst%levelcenter)
          case default
             get_crest_level_c_loc = C_NULL_PTR
       end select

   end function get_crest_level_c_loc   
   
   double precision function get_width(struc)
      type(t_structure), intent(in) :: struc
      
       select case (struc%st_type)
          case (ST_WEIR)
             get_width = struc%weir%crestwidth
          case (ST_RIVER_WEIR)
             get_width = struc%riverweir%crestwidth
          case (ST_ADV_WEIR)
             get_width = struc%advweir%totwidth
          case (ST_GENERAL_ST)
             get_width = struc%generalst%widthcenter
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
      
       select case (struc%st_type)
       case (ST_ORIFICE)
          get_watershed_threshold = huge(1d0)
       case default
          get_watershed_threshold = get_crest_level(struc)
       end select

   end function get_watershed_threshold
   
   double precision function get_gle(struc)
      
      type (t_structure), intent(inout) :: struc
      
      select case(struc%st_type)
      case (ST_ORIFICE)
         get_gle = struc%orifice%openlevel 
      case (ST_GENERAL_ST)
         get_gle = struc%generalst%gateheight 
      end select
   end function get_gle
   
   double precision function get_opening_height(struc)
      
      type (t_structure), intent(inout) :: struc
      
      select case(struc%st_type)
      case (ST_ORIFICE)
         get_opening_height = struc%orifice%openlevel - struc%orifice%crestlevel
      case (ST_GENERAL_ST)
         get_opening_height = struc%generalst%gateheight - struc%generalst%levelcenter
      end select
   end function get_opening_height
   
   double precision function get_valve_opening(struc)
      
      type (t_structure), intent(inout) :: struc
      
      select case(struc%st_type)
      case (ST_CULVERT, ST_SIPHON, ST_INV_SIPHON)
         get_valve_opening = struc%culvert%inivalveopen
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
         select case(sts%struct(ist)%st_type)
         case (ST_WEIR, ST_ORIFICE, ST_RIVER_WEIR, ST_ADV_WEIR, ST_GENERAL_ST, ST_UNI_WEIR)
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
   
      select case(struc%st_type)
      case (ST_WEIR)
         struc%weir%crestlevel=value
      case (ST_ORIFICE)
         struc%orifice%crestlevel=value
      case (ST_RIVER_WEIR)
         struc%riverweir%crestlevel=value
      case (ST_ADV_WEIR)
         struc%advweir%crestlevel=value
      case (ST_UNI_WEIR)
         struc%uniweir%crestlevel=value
      case (ST_GENERAL_ST)
         struc%generalst%levelcenter=value
      case default
         !nothing
      end select
   end subroutine set_crest_level

   subroutine set_crest_width(struc, value)
      
      type (t_structure), intent(inout) :: struc
      double precision, intent(in)      :: value
      
      select case(struc%st_type)
      case (ST_WEIR)
         struc%weir%crestwidth=value
      case (ST_ORIFICE)
         struc%orifice%crestwidth=value
      case (ST_RIVER_WEIR)
         struc%riverweir%crestwidth=value
      case (ST_ADV_WEIR)
         struc%advweir%totwidth=value
      case (ST_GENERAL_ST)
         struc%generalst%widthcenter=value
      end select
   end subroutine set_crest_width

   subroutine set_gle(struc, value)
      
      type (t_structure), intent(inout) :: struc
      double precision, intent(in)      :: value
      
      select case(struc%st_type)
      case (ST_ORIFICE)
         struc%orifice%openlevel =value
      case (ST_GENERAL_ST)
         struc%generalst%gateheight =value
      end select
   end subroutine set_gle
   
   subroutine set_opening_height(struc, value)
      
      type (t_structure), intent(inout) :: struc
      double precision, intent(in)      :: value
      
      select case(struc%st_type)
      case (ST_ORIFICE)
         struc%orifice%openlevel=value + struc%orifice%crestlevel
      case (ST_GENERAL_ST)
         struc%generalst%gateheight =value + struc%generalst%levelcenter
      end select
   end subroutine set_opening_height
   
   subroutine set_valve_opening(struc, value)
      
      type (t_structure), intent(inout) :: struc
      double precision, intent(in)      :: value
      
      select case(struc%st_type)
      case (ST_CULVERT, ST_SIPHON, ST_INV_SIPHON)
         struc%culvert%inivalveopen=value
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
   
   double precision function get_capacity(struc)
      
      type (t_structure), intent(inout) :: struc
      
      if (struc%pump%isControlled) then
         get_capacity = struc%pump%capacitySetpoint
      else
         get_capacity = struc%pump%capacity(1)
      endif
      
   end function get_capacity
   
end module m_structure
