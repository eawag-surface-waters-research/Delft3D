      module waqlib
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2014.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
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
      use precision
      implicit none
      
      integer, parameter :: wp=sp ! WAQ precision
!
      type waqlibtype
          !
          ! reals
          !
          real(wp), dimension(3)                   :: disp           ! array for uniform dispersion coefficients  
          !
          ! integers
          !
          integer                                  :: int_method     ! integration option
          integer                                  :: nprocess       ! number of processes
          integer                                  :: nprocpar       ! number of process parameters
          integer                                  :: nsubs          ! number of substances
          integer                                  :: start_his      ! history output start time
          integer                                  :: stop_his       ! history output stop time
          integer                                  :: step_his       ! history output time step
          integer                                  :: start_map      ! map output start time
          integer                                  :: stop_map       ! map output stop time
          integer                                  :: step_map       ! map output time step
          !
          ! pointers
          !
          real(wp), dimension(:), pointer          :: subsval        ! substance initial scalar conditions in delwaq precision
          real(wp), dimension(:), pointer          :: vol            ! volumes in delwaq precision
          real(wp), dimension(:), pointer          :: area           ! areas in delwaq precision
          real(wp), dimension(:), pointer          :: qag            ! flows in delwaq precision
          character(len=20), dimension(:), pointer :: process        ! process names
          character(len=20), dimension(:), pointer :: procpar        ! process parameter names
          character(len=20), dimension(:), pointer :: subs           ! substance names
          !
          ! logicals
          !
          logical                                  :: disp_flow_zero ! dispersion active when flow is zero
          logical                                  :: disp_bound     ! dispersion active at open boundaries
          logical                                  :: first_order    ! lower order scheme at open boundaries
          logical                                  :: forester       ! forester filter active
          logical                                  :: anticreep      ! anti-creep active
          !
          ! characters
          !
      end type waqlibtype
      
      logical, external                                 :: DefineWQSchematisation
      logical, external                                 :: DefineWQDispersion
      logical, external                                 :: DefineWQProcesses
      logical, external                                 :: SetIntegrationOptions
      logical, external                                 :: SetInitialVolume
      logical, external                                 :: SetCurrentValueScalarInit
      logical, external                                 :: SetBoundaryConditions
      logical, external                                :: SetFlowData
      logical, external                                :: ModelInitialize
      logical, external                                :: ModelPerformTimestep
      logical, external                               :: SetSimulationTimes
      logical, external                               :: SetOutputTimers
      logical, external                               :: GetCurrentValue
      
      contains
      
      subroutine rd_waqlib_par(wlp, start_time, stop_time, time_step)
!!--description-----------------------------------------------------------------
! Read waq lib parameters
!!--declarations----------------------------------------------------------------
      implicit none
!
!           Global variables
!
      type(waqlibtype),          intent(out):: wlp
      integer,                   intent(in) :: start_time
      integer,                   intent(in) :: stop_time
      integer,                   intent(in) :: time_step
!
!           Local variables
!
      integer                               :: istat
!
!! executable statements -------------------------------------------------------
!
      wlp%disp(1) = 1.0
      wlp%disp(2) = 1.0
      wlp%disp(3) = 1.0e-6
      !
      wlp%int_method = -999
      wlp%disp_flow_zero = .true.
      wlp%disp_bound = .true.
      wlp%first_order = .true.
      wlp%forester = .false.
      wlp%anticreep = .false.
      !
      wlp%nprocess = 0
      wlp%nprocpar = 0
      wlp%nsubs = 0
      !
      istat = 0
      if (istat==0) allocate(wlp%process(wlp%nprocess), stat=istat)
      if (istat==0) allocate(wlp%procpar(wlp%nprocpar), stat=istat)
      if (istat==0) allocate(wlp%subs   (wlp%nsubs)   , stat=istat)
      if (istat==0) allocate(wlp%subsval(wlp%nsubs)   , stat=istat)
      if (istat/=0) then
         write(*,*) '*** ERROR: rd_waqlib_par: memory allocation error'
         return
      endif
      !
      wlp%process = ' '
      wlp%procpar = ' '
      wlp%subs = ' '
      wlp%subsval = 0.0
      !
      wlp%start_his = start_time
      wlp%stop_his = stop_time
      wlp%step_his = time_step
      !
      wlp%start_map = start_time
      wlp%stop_map = stop_time
      wlp%step_map = time_step

      end subroutine rd_waqlib_par
      
      
      subroutine init_waqlib(noq, noseg, start_time, stop_time, time_step, ifrmto, lenex, vol, wlp)
!!--description-----------------------------------------------------------------
! Initialize WAQ as a shared library
!!--declarations----------------------------------------------------------------
      implicit none
!
!           Global variables
!
      integer,                  intent(in) :: noq
      integer,                  intent(in) :: noseg
      integer,                  intent(in) :: start_time
      integer,                  intent(in) :: stop_time
      integer,                  intent(in) :: time_step
      integer, dimension(:,:),  intent(in) :: ifrmto
      real(hp), dimension(:,:), intent(in) :: lenex
      real(hp), dimension(:),   intent(in) :: vol
      type(waqlibtype),         intent(out):: wlp
!
!           Local variables
!
      logical :: success
      integer :: i
      integer :: istat
      integer, dimension(4)                 :: noexch
      real(wp), dimension(:,:), allocatable :: wp_lenex
!
!! executable statements -------------------------------------------------------
!
      istat = 0
      call rd_waqlib_par(wlp, start_time, stop_time, time_step)
      !
      ! define WQ schematisation
      !
      noexch(1) = noq
      noexch(2) = 0 ! no second dimension yet
      noexch(3) = 0 ! no vertical dimension yet
      noexch(4) = 0 ! no Delwaq-G coupling yet
      success = DefineWQSchematisation(noseg,ifrmto,noexch)
      !
      ! define monitoring areas
      !
      !success = DefineWQMonitoringAreas()
      !
      ! define dry waste loads
      !
      !success = DefineWQDryWasteLoads()
      !
      ! define dispersion coefficients and segment distances
      !
      if (istat==0) allocate ( wp_lenex(2, noq), stat=istat)
      if (istat/=0) then
         write(*,*) '*** ERROR: init_waqlib: memory allocation error LENEX'
         return
      endif
      wp_lenex = real(lenex,wp)
      success = DefineWQDispersion(wlp%disp,wp_lenex)
      deallocate(wp_lenex, stat=istat)
      !
      ! write first volumes
      !
      if (istat==0) allocate ( wlp%vol(noseg), stat=istat)
      if (istat==0) allocate ( wlp%area(noq) , stat=istat)
      if (istat==0) allocate ( wlp%qag(noq)  , stat=istat)
      if (istat/=0) then
         write(*,*) '*** ERROR: init_waqlib: memory allocation error VOL'
         return
      endif
      wlp%vol = real(vol,wp)
      success = SetInitialVolume(wlp%vol)
      !
      ! set computational options
      !
      success = SetIntegrationOptions(wlp%int_method, wlp%disp_flow_zero, &
                                    & wlp%disp_bound, wlp%first_order, &
                                    & wlp%forester, wlp%anticreep)
      success = SetSimulationTimes( start_time, stop_time, time_step)
      !
      ! define processes
      ! 3rd argument should be: wlp%nsubs_transportable
      success = DefineWQProcesses( wlp%subs, wlp%nsubs, wlp%nsubs, &
                                 & wlp%procpar, wlp%nprocpar, &
                                 & wlp%process, wlp%nprocess )
      do i = 1, wlp%nsubs
         success = SetCurrentValueScalarInit( wlp%subs(i), wlp%subsval(i) )
      enddo
      !
      ! finalize initialization
      !
      success = ModelInitialize()
      !
      ! set map output times
      !
      success = SetOutputTimers( 1, wlp%start_map, wlp%stop_map, wlp%step_map )
      !
      ! set history output times
      !
      success = SetOutputTimers( 2, wlp%start_his, wlp%stop_his, wlp%step_his )
      end subroutine init_waqlib
      
      
      subroutine step_waqlib(vol, area, qag, wlp)
!!--description-----------------------------------------------------------------
! Let WAQ as a shared library perform a time step 
!!--declarations----------------------------------------------------------------
      implicit none
!
!           Global variables
!
      real(hp), dimension(:),   intent(in) :: vol
      real(hp), dimension(:),   intent(in) :: area
      real(hp), dimension(:),   intent(in) :: qag
      type(waqlibtype),         intent(in) :: wlp
!
!           Local variables
!
      logical :: success
      integer :: i
!
!! executable statements -------------------------------------------------------
!
      !do i = 1,nobnd
      !   success = SetBoundaryConditions( ibound(i), cbound )
      !enddo
      !
      ! pass volumes, areas and flows
      !
      success = SetFlowData(wlp%vol, wlp%area, wlp%qag)
      !
      ! perform time step
      !
      success = ModelPerformTimeStep()
      !
      !success = GetCurrentValue( 'Salinity', value )
      end subroutine step_waqlib
      
      end module waqlib