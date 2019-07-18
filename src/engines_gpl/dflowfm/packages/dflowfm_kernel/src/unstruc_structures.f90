module m_structures

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

use properties
use m_GlobalParameters
implicit none

type(tree_data), pointer, public :: strs_ptr !< A property list with all input structure specifications of the current model. Not the actual structure set.
integer :: jaoldstr !< tmp backwards comp: we cannot mix structures from EXT and from structure-input files. Use one or the other.

 ! Structure Parameters
 double precision, dimension(:,:), allocatable :: valpump     !< Array for pump;      (1,:) flow link width, used for averaging.
                                                              !<                      (2,:) pump discharge
                                                              !<                      (3,:) pump structure water level up
                                                              !<                      (4,:) pump structure water level down
                                                              !<                      (5,:) pump structure head
                                                              !<                      (6,:) pump capacity
                                                              !<                      (7,:) actual pump stage
                                                              !<                      (8,:) pump head
                                                              !<                      (9,:) pump reduction factor
                                                              !<                      (10,:) pump water level at delivery side
                                                              !<                      (11,:) pump water level at suction side                                                             !<                      (2,:) capacity
 double precision, dimension(:,:), allocatable :: valgate     !< Array for gate;      (1,:) discharge through gate
 double precision, dimension(:,:), allocatable :: valcdam     !< Array for cdam;      (1,:) discharge through controlable dam
                                                              !<                      (2,:) Upstream average water levels
                                                              !<                      (3,:) downstream average water level
                                                              !<                      (4,0) width of dam
 double precision, dimension(:,:), allocatable :: valgategen  !< Array for gate(new), (1,:) discharge through gate
                                                              !<                      (2,:) Upstream average water level
                                                              !<                      (3,:) gate width
 double precision, dimension(:,:), allocatable :: valweirgen  !< Array for weir;      (1,:) discharge through weir
                                                              !<                      (2,:) discharge through weir
 double precision, dimension(:,:), allocatable :: valcgen     !< Array for general structure (old ext), (1,:) discharge
 double precision, dimension(:,:), allocatable :: valgenstru  !< Array for general structure (new ext), (1,:) discharge
 double precision, dimension(:,:), allocatable, target :: valdambreak !< Array for dambreak, (1,:) instantanuous, (2,:) cumulative

 integer                           :: NUMVALS_PUMP = 11        !< Number of variables for pump
 integer                           :: NUMVALS_GATE = 5        !< Number of variables for gate
 integer                           :: NUMVALS_CDAM = 4        !< Number of variables for controble dam
 integer                           :: NUMVALS_CGEN = 4        !< Number of variables for general structure (old ext file)
 integer                           :: NUMVALS_GATEGEN = 9     !< Number of variables for gate (new)
 integer                           :: NUMVALS_WEIRGEN = 7     !< Number of variables for weir
 integer                           :: NUMVALS_GENSTRU = 8     !< Number of variables for general structure( new exe file)
 integer                           :: NUMVALS_DAMBREAK = 2    !< Number of variables for dambreak

 integer                           :: jahiscgen               !< Write structure parameters to his file, 0: n0, 1: yes
 integer                           :: jahispump               !< Write pump      parameters to his file, 0: n0, 1: yes
 integer                           :: jahisgate               !< Write gate      parameters to his file, 0: n0, 1: yes
 integer                           :: jahiscdam               !< Write dam       parameters to his file, 0: n0, 1: yes
 integer                           :: jahisweir               !< Write weir      parameters to his file, 0: n0, 1: yes
 integer                           :: jahisdambreak           !< Write dambreak  parameters to his file, 0: n0, 1: yes

 integer, parameter :: IOPENDIR_FROMLEFT  = -1 !< Gate door opens/closes from left side.
 integer, parameter :: IOPENDIR_FROMRIGHT =  1 !< Gate door opens/closes from right side.
 integer, parameter :: IOPENDIR_SYMMETRIC =  0 !< Gate door opens/closes symmetrically (from center).

 type tgate                                          !< Gate structure type, before it gets evaluated as a general structure.
    !double precision :: sill_level       !< Not used: stored in zcgen(1,igen)
    !double precision :: lower_edge_level !< Not used: stored in zcgen(2,igen)
    !double precision :: opening_width    !< Not used: stored in zcgen(3,igen)
    double precision :: door_height       !< Height of the door, used for 'double-barrier' overflow. Time-INDEPENDENT.
    double precision :: sill_width        !< Width of the sill, may be larger than the opening width, such that in open part we have weir flow and in closed part we have gate flow. Time-INDEPENDENT.
    integer          :: opening_direction !< Direction from which the gate opens/closes, IOPENDIR_FROMLEFT|FROMRIGHT|SYMMETRIC.
 end type tgate

 ! TIDAL TURBINES: Insert allocatable of type structure_turbines here

 type(tgate), allocatable :: gates(:)
   contains


   subroutine init_structure_hisvalues()
      use m_flowexternalforcings , only: npumpsg, ncgensg, ngatesg, ncdamsg, ngategen, ngenstru, nweirgen, ndambreaksg
      !use m_structures, only: NUMVALS_PUMP, NUMVALS_GATE, NUMVALS_CDAM, NUMVALS_CGEN, &
      !                        NUMVALS_GATEGEN, NUMVALS_WEIRGEN, NUMVALS_GENSTRU
      use m_alloc

      implicit none

      jahiscgen = 1
      jahispump = 1
      jahisgate = 1
      jahiscdam = 1
      jahisweir = 1
      jahisdambreak = 1

      if( jahispump > 0 .and. npumpsg > 0) then
         if( allocated( valpump ) ) deallocate( valpump )
         allocate( valpump(NUMVALS_PUMP,npumpsg) ) ; valpump = 0d0
      endif
      if( jahiscgen > 0 ) then
         if( ncgensg > 0 ) then
            if( allocated( valcgen ) ) deallocate( valcgen )
            allocate( valcgen(NUMVALS_CGEN,ncgensg) ) ; valcgen = 0d0
         endif
         if( ngenstru > 0 ) then
            if( allocated( valgenstru ) ) deallocate( valgenstru )
            allocate( valgenstru(NUMVALS_GENSTRU,ngenstru) ) ; valgenstru  = 0d0
         endif
      endif
      if( jahisgate > 0 ) then
         if( ngatesg > 0 ) then
            if( allocated( valgate ) ) deallocate( valgate )
            allocate( valgate(NUMVALS_CGEN,ngatesg) ) ; valgate = 0d0
         endif
         if( ngategen > 0 ) then
            if( allocated( valgategen ) ) deallocate( valgategen )
            allocate( valgategen(NUMVALS_GATEGEN,ngategen) ) ; valgategen = 0d0
         endif
      endif
      if( jahiscdam > 0 .and. ncdamsg > 0) then
         if( allocated( valcdam) ) deallocate( valcdam )
         allocate( valcdam(NUMVALS_CDAM,ncdamsg) ) ; valcdam = 0d0
      endif
      if( jahisweir > 0 .and. nweirgen > 0) then
         if( allocated( valweirgen) ) deallocate( valweirgen )
         allocate( valweirgen(NUMVALS_WEIRGEN,nweirgen) ) ; valweirgen = 0d0
      endif
      if( jahisdambreak > 0 .and. ndambreaksg > 0) then
         if( allocated( valdambreak ) ) deallocate( valdambreak )
         allocate( valdambreak(NUMVALS_DAMBREAK,ndambreaksg) ) ; valdambreak = 0d0
      endif

! TIDAL TURBINES: Insert init_turbines here

 end subroutine init_structure_hisvalues



!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_structures() instead.
subroutine default_structures()

call tree_destroy(strs_ptr)

call reset_structures()

! TIDAL TURBINES: Insert calls to deallocate_turbines and init_turbines here

end subroutine default_structures


!> Resets only structures variables intended for a restart of an existing flow simulation (same MDU).
!! Upon loading of new model/MDU, call default_structures() instead.
subroutine reset_structures()
   if (allocated(gates)) deallocate(gates)
end subroutine reset_structures

!> Fills the valstruct array for one given structure on a given link L.
!! This is an array with output values that are useful for all types of structures.
subroutine fill_valstruct_perlink(valstruct, L, dir)
   use m_missing, only: dmiss
   use m_flow, only: q1, s1
   use m_flowgeom, only: wu, ln
   implicit none
   double precision, dimension(:), intent(inout) :: valstruct   !< Output values on structure (e.g. valpumps(:)):
                                                                !< (1) total width
                                                                !< (2) structure discharge
                                                                !< (3) structure water level up
                                                                !< (4) structure water level down
                                                                !< (5) structure head
   integer,                        intent(in   ) :: L           !< flow link index
   double precision,               intent(in   ) :: dir         !< direction of flow link w.r.t. structure orientation (1.0 for same direction, -1.0 for opposite).

   integer :: ku, kd
   
   if (dir > 0) then
      ku = ln(1,L)
      kd = ln(2,L)
   else
      ku = ln(2,L)
      kd = ln(1,L)
   end if

   valstruct(1) = valstruct(1) + wu(L)
   valstruct(2) = valstruct(2) + q1(L)*dir
   valstruct(3) = valstruct(3) + s1(ku)*wu(L)
   valstruct(4) = valstruct(4) + s1(kd)*wu(L)
   valstruct(5) = valstruct(5) + (s1(ku)- s1(kd))*wu(L)
   
end subroutine fill_valstruct_perlink


!> Averages the values on one structure across all links,
!! where needed taking care of partition models.
!! Note: fill_valstructs_perlink must have been called in
!! a loop prior to calling this averaging routine.
subroutine average_valstruct(valstruct)
   use m_missing, only: dmiss
   use m_partitioninfo, only: jampi
   implicit none
   double precision, dimension(:), intent(inout) :: valstruct   !< Output values on structure (e.g. valpump(:)):
                                                                !< (1) total width (unchanged)
                                                                !< (2) structure discharge (unchanged)
                                                                !< (3) structure water level up (averaged)
                                                                !< (4) structure water level down (averaged)
                                                                !< (5) structure head (averaged)

   if( jampi == 0 ) then
      if(valstruct(1) == 0d0 ) then
         valstruct(2) = dmiss  ! discharge
         valstruct(3) = dmiss  ! s1up
         valstruct(4) = dmiss  ! s1down
         valstruct(5) = dmiss  ! head
      else
         ! valstruct(2): keep discharge at the summed value
         ! Average the remaining values:
         valstruct(3) = valstruct(3) / valstruct(1)
         valstruct(4) = valstruct(4) / valstruct(1)
         valstruct(5) = valstruct(5) / valstruct(1)
      endif
   endif

end subroutine average_valstruct

end module m_structures