!> Define boundary conditions (water level and discharge with or without salinity, and wind speed and direction)
module m_boundaryConditions
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

   integer, parameter, public       :: H_BOUN  = 1             !< Parameter for boundary condition of type water level boundary
   integer, parameter, public       :: Q_BOUN  = 2             !< Parameter for boundary condition of type discharge boundary
   integer, parameter, public       :: S_BOUN  = 3             !< Parameter for boundary condition of type salt boundary
   integer, parameter, public       :: T_BOUN  = 6             !< Parameter for boundary condition of type temperature boundary
   integer, parameter, public       :: B_WINDVEL = 4           !< Parameter for wind velocity
   integer, parameter, public       :: B_WINDDIR = 5           !< Parameter for wind direction
   integer, parameter, public       :: NUM_BOUN_TYPE = 6       !< Number of possible boundary types
   integer, parameter, private      :: eps  = 1d-6             !< accuracy parameter

   private

   public AddBoundary
   public ShiftValues
   public realloc
   public dealloc
   public printData
   public GetSalinityIndexFromFlowBoundary

   public fill_hashtable
   
   interface fill_hashtable
      module procedure fill_hashtable_bd
   end interface 

   !> Shift and add values in a array
   interface ShiftValues
      module procedure ShiftValuesBnd
   end interface

   !> Print the boundary condition(s) or type
   interface printData
      module procedure printBoundaryConditions
      module procedure printBoundaryConditionType
      module procedure printBoundaryCondition
   end interface

   !> Reallocate the boundary derived type: t_boundary
   interface realloc
      module procedure reallocBoundary
   end interface

   !> Deallocate the boundary derived type: t_boundary
   interface dealloc
      module procedure deallocBoundary
   end interface dealloc

   !> Derived type for defining one boundary condition
   type, public :: t_boundary
      integer                :: length  = 0  !< number of values in arrays values1 and values2
      character(IdLen)       :: nodeID       !< node ID on which the boundary condition is placed
      integer                :: node    = 0  !< node number on which the boundary condition is placed
                                             !> type of boundary condition, possible values
                                             !! - 1 : time series (VALUES1 contains the time in seconds, VALUES2 contains the boundary values)
                                             !! - 2 : HQ boundary (VALUES1 contains the discharge, VALUES2 contains the corresponding water levels)
                                             !! - 3 : series of fourier
                                             !! - 4 : series of tidal components \n
                                             !! For salinity only iopt ==2 is a valid value. In that case a Thatcher Harleman condition is applied
      integer                :: iopt    = 0
                                             !> function type for interpolation\n
                                             !! - div(access, 10) == 1: periodical function
                                             !! - mod(access, 10) == 0: linear function
                                             !! - mod(access, 10) == 1: block function
      integer                :: access  = 0
      double precision       :: returnTime = 0d0  !< Thatcher Harleman return time for salinity boundary conditions

      type(t_table), pointer :: table => null()   !< table containing time series of boundary condition
      integer                :: salinityIndex     !< index to corresponding salinity boundary
      
      ! Help variables for SOBEK administration
      integer           :: igrid             !< sobek grid number
      integer           :: intern            !< internal Pluvius grid number
      integer           :: branch            !< Pluvius branch number
      integer           :: linknumber        !< Pluvius link number
      integer           :: direction         !< direction > 0 means boundary at begin of link, direction < 0 boundary at end of link
   end type

    !> Array containing boundary data grouped according to the types
    !! - pos. 1: water level boundary conditions
    !! - pos. 2: discharge boundary conditions
    !! - pos. 3: H(Q) boundary conditions
    !! - pos. 4: Salinity boundary conditions
   type, public :: t_tp !< groups of boundary conditions
      character(LEN=40)                                     :: TypeLabel     !< Boundary type label
      integer                                               :: Size = 0      !< Size of the derived type array bd
      integer                                               :: growsBy = 2000 !< Increase the bd-array with the value growsBy
      integer                                               :: Count= 0      !< Current number of boundaries
      type(t_boundary), pointer, dimension(:)               :: bd => null()  !< Array containing boundary conditions
      type(t_hashlist)                                      :: hashlist
   end type t_tp

   !> Derived type to store the boundaries
   type, public :: t_boundarySet
      integer                                 :: Count= NUM_BOUN_TYPE !< Maximum number of boundary set allowed
      type(t_tp), dimension(NUM_BOUN_TYPE)    :: tp
   end type t_boundarySet

contains

   !> Adds a boundary and condition to the set of boundaries.
   integer function AddBoundary(boundaries, nds, typ, node, iopt, access, values1, values2, length, returnTime)
      ! Modules
      use m_node

      implicit none

      ! Input/output parameters
      type(t_boundarySet)              :: boundaries        !< set containing boundary conditions
      type(t_nodeSet)                  :: nds               !< set containing node data
                                                            !> type of boundary condition, possible values
                                                            !! - 1 : water level
                                                            !! - 2 : discharge
                                                            !! - 3 : salinity
      integer                          :: typ 
      integer                          :: node  
                                                            !> iopt = 1 : H- or Q-boundary
      integer                          :: iopt              !! iopt = 2 : QH-Boundary
      integer                          :: access            !< Allowed value: 0, 1, 10 and 11
      double precision, dimension(:)   :: values1           !<
      double precision, dimension(:)   :: values2           !<
      integer                          :: length            !< Length of arrays values1 and values2
      double precision                 :: returnTime        !< Thatcher Harleman return time

      ! local parameters
      integer           :: i, j
      integer           :: count
      character(Charln)                             :: line
      double precision, dimension(:), allocatable   :: help

      ! Program code

      ! Check this only in case of DLL version. In the EXE version Parsen has already checked this
!            line = 'On node '//trim(nds%node(node)%id) // ' more than one boundary condition is defined.'

      if ( (nds%count > 0) .and. (typ <= nt_DischBoun) ) then
         if ( (typ == 1 .or. typ ==2)  .and. (nds%node(node)%nodetype == nt_LevelBoun .or. nds%node(node)%nodetype == nt_DischBoun) ) then

            ! two boundaries on one node
            line = 'On node '//trim(nds%node(node)%id) //' more than one boundary condition is defined.'
            call setMessage(LEVEL_FATAL, line)
            return
         elseif ( (typ == 3)  .and. (nds%node(node)%nodetype /= nt_LevelBoun .and. nds%node(node)%nodetype /= nt_DischBoun) ) then

            ! a salinity concentration must have a corresponding flow boundary

            line = 'On node '//trim(nds%node(node)%id) //' a salinity boundary is defined, without a flow boundary condition.'
            call setMessage(LEVEL_FATAL, line)
            return
         elseif (typ == 2 .and. nds%node(node)%nodetype /= nt_EndNode) then
            line = 'Discharge boundary at node '//trim(nds%node(node)%id) //' is not an endnode.'
            call setMessage(LEVEL_FATAL, line)
            return
         endif
      endif

      if ( (iopt==2) .and. (typ==H_BOUN .or. typ==Q_BOUN) ) then
         ! QH boundary
         ! Check if all values are in ascending order Q as well as H
         do i = 2, length
            if (abs(values1(i)) <= abs(values1(i-1)) ) then
               line = 'On node '//trim(nds%node(node)%id) //' the discharges of Q(h) boundary condition are not in ascending order.'
               call SetMessage(LEVEL_WARN, line)
            endif
            if (values2(i) <= values2(i-1)) then
               line = 'On node '//trim(nds%node(node)%id) //' the levels of Q(h) boundary condition are not in ascending order.'
               call SetMessage(LEVEL_WARN, line)
            endif
            if ( (values1(1) < -eps) .and. (values1(i) > eps) .or. (values1(1) > eps) .and. (values1(i) < -eps)) then
               line = 'On node '//trim(nds%node(node)%id) //' the discharge of Q(h) boundary condition has positive and negative values.'
               call SetMessage(LEVEL_FATAL, line)
            endif
         enddo

         ! Switch Table Columns
         typ = Q_BOUN
         allocate(help(length))
         help    = values1
         values1 = values2
         values2 = help
         deallocate(help)
         
      endif

      ! check if node is on boundary of network

      boundaries%tp(typ)%Count = boundaries%tp(typ)%Count+1
      count = boundaries%tp(typ)%Count
      if (count > boundaries%tp(typ)%Size) then
         call realloc(boundaries%tp(typ), typ)
      endif

      if (nds%count > 0) then
         if (typ==1) then
            ! water level boundary
            nds%node(node)%nodetype = nt_LevelBoun
         elseif (typ==2) then
            ! discharge boundary
            nds%node(node)%nodetype = nt_DischBoun
         endif

      endif

      boundaries%tp(typ)%bd(count)%node        = node
      boundaries%tp(typ)%bd(count)%length      = length
      boundaries%tp(typ)%bd(count)%iopt        = iopt
      boundaries%tp(typ)%bd(count)%access      = access
      boundaries%tp(typ)%bd(count)%returnTime  = returnTime
      boundaries%tp(typ)%bd(count)%salinityIndex = -1
      if (typ==3 .and. returntime>0.0) then
         boundaries%tp(typ)%bd(count)%iopt     = 2
      endif
      if (typ == 3 ) then
         loop : do i = 1, 2
            do j = 1, boundaries%tp(i)%count
               if (boundaries%tp(i)%bd(j)%node == node) then
                  boundaries%tp(i)%bd(j)%salinityIndex = count
                  exit loop
               endif
            enddo
         end do loop
      endif
      call setTable( boundaries%tp(typ)%bd(count)%table, access, values1, values2, length)
      Addboundary = count
   end function AddBoundary

   !> Shift boundary values in table
   subroutine  ShiftValuesBnd(boundaries, typ, index, time, value)
      type(t_boundarySet)              :: boundaries !< set of boundaries
      integer                          :: typ        !< Type of boundary
      integer                          :: index      !< Current boundary
      double precision                 :: time       !< Time for which the value need to be set
      double precision                 :: value      !< Boundary condition value

      call ShiftValues(boundaries%tp(typ)%bd(index)%table, time, value)
    end subroutine  ShiftValuesBnd

   !> Deallocate a boundary set
   subroutine deallocBoundary(boundaries)
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_boundarySet), intent(inout)          :: boundaries

      ! Local variables
      integer                       :: i, j
      integer                       :: count

      ! Program code
      do i = 1, NUM_BOUN_TYPE
         count = boundaries%tp(i)%count
         if (associated(boundaries%tp(i)%bd)) then
            do j = 1, count
               deallocate(boundaries%tp(i)%bd(j)%table)
               boundaries%tp(i)%bd(j)%table => null()
            enddo
            if (count > 0) deallocate(boundaries%tp(i)%bd)
         endif
         boundaries%tp(i)%bd => null()
         boundaries%tp(i)%Size  = 0
         boundaries%tp(i)%Count = 0
         call dealloc(boundaries%tp(i)%hashlist)
      enddo
   end subroutine
    !
    !> Reallocate a boundary
   subroutine reallocBoundary(tp, typ)
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_tp), intent(inout)          :: tp  !< Boundary
      integer, intent(in)                :: typ !< Type of boundary

      ! Local variables
      type(t_boundary), pointer, dimension(:)     :: oldbds

      ! Program code

      if (tp%Size == 0) then

         select case (typ)

         case (1)
            tp%TypeLabel = 'Level Boundary'

         case (2)
            tp%TypeLabel = 'Discharge Boundary'

         case (3)
            tp%TypeLabel = 'Salinity Boundary'

         case (4)
            tp%TypeLabel = 'Wind Velocity'

         case (5)
            tp%TypeLabel = 'Wind Direction'

         case default
            tp%TypeLabel = 'Unknown'

         end select

      else
         oldbds=>tp%bd
      endif
      if (tp%growsBy <=0) then
         tp%growsBy = 200
      endif
      
      allocate(tp%bd(tp%Size+tp%growsBy))

      if (tp%Size > 0) then
         tp%bd(1:tp%Size) = oldbds(1:tp%Size)
         deallocate(oldbds)
      endif
      tp%Size = tp%Size+tp%growsBy
   end subroutine

   !> Prints a whole boundary set
   subroutine printBoundaryConditions(boundaries, unit)
      type(t_boundarySet) boundaries
      integer          unit

      integer  i

      do i = 1, NUM_BOUN_TYPE
         call printData(boundaries%tp(i), unit)
      enddo
   end subroutine printBoundaryConditions

   !> Print boundary type and data
   subroutine printBoundaryConditionType(tp, unit)
      type(t_tp) tp
      integer          unit

      integer  i

      do i = 1, tp%count
         write(unit, '(a)') ''
         write(unit, '(a)') 'Type: '//tp%TypeLabel
         call printData(tp%bd(i), unit)
      enddo
   end subroutine printBoundaryConditionType

   !> Print the boundary condition
   subroutine printBoundaryCondition(bd, unit)
      type(t_boundary) bd
      integer          unit

      write(unit, '(''node = '', i6, '' iopt = '', i2, '' returntime = '', g16.3)') bd%node,     &
                  bd%iopt, bd%returntime
      ! only print qh-tables because, time series are different. in DelftFlow and DeltaShell
      if (bd%iopt==2) then
         call printData(bd%table, unit)
      endif
   end subroutine printBoundaryCondition

   integer function GetSalinityIndexFromFlowBoundary(boundaries, typ, index)
      type(t_boundarySet)  :: boundaries
      integer              :: typ
      integer              :: index
      
      GetSalinityIndexFromFlowBoundary = boundaries%tp(typ)%bd(index)%salinityIndex
   end function GetSalinityIndexFromFlowBoundary
   
   subroutine fill_hashtable_bd(btyp)
   
      type (t_tp), intent(inout), target           :: btyp
      
      integer                                      :: ibd
      character(len=idlen), dimension(:), pointer  :: ido

      allocate(btyp%hashlist%id_list(btyp%Count))
      btyp%hashlist%id_count = btyp%Count
      ido => btyp%hashlist%id_list
      
      do ibd = 1, btyp%count
         ido(ibd) = btyp%bd(ibd)%nodeID
      enddo
      
      call hashfill(btyp%hashlist)
      
   end subroutine fill_hashtable_bd
   
end module M_boundaryConditions
