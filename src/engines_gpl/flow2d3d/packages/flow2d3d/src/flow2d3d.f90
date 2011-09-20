!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
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
!
!-------------------------------------------------------------------------------
!
! The following subroutine was originally located in runme.f90
!
!-------------------------------------------------------------------------------
!
!> \file
!! The dynamic library 'Delft3D-FLOW'.
!! Flow-related routines are in trisim.f90 and below.
!<

! Content specifically for the Doxygen-index page:
!> \mainpage Delft3D-FLOW API docs
!! \li <b> Main program</b>: deltares_hydro.F90
!! \li <b> Dynamic library entrances</b>: flow2d3d_dll.f90
!! \li <b> DD and RemoteOLV preparations, start all processes</b>: hydra.cpp (Hydra::Execute)
!! \li <b> Global data</b>: globaldata.f90 (flow, geometry, times, parameters, ...)
!! \li <b> Subdomain calculation toplevel</b>: trisim.f90
!! \li <b> Preprocessor: Convert time related data</b>: tdatom.f90
!! \li <b> Timeloop</b>: tricom_step.F90
!! \li <b> Main routine within one time step</b>: 
!! trisol.f90 (sigma layers)
!! z_trisol.f90 (z layers, hydrostatic)
!! z_trisol_nhfull.f90 (z layers, non-hydrostatic)
!<

!> Perform a Delft3D-FLOW computation, given a set of configuration parameters.
!!
!! Phases:
!! - 1) Interpretation of the keys/values from the config file
!! - 2) Create runid, based on mdfFile/ddbFile
!! - 3) In dd_execute:
!!    - RemoteOLV initialization
!!    - DD initialization
!!    - Start all processes:
subroutine runme_core(max_keyval, keys   , values   , error_message)
    !!--pseudo code and references--------------------------------------------------
    ! NONE
    !!--declarations----------------------------------------------------------------
    use precision
    implicit none
    !
    include 'fsm.i'   ! for FSM_SILENT (to start single threaded call)
    !
    ! Subroutine arguments
    !
    integer                                , intent(in)  :: max_keyval    !< Size of key-value arrays.
    character(256), dimension(max_keyval)  , intent(in)  :: keys          !< Array with keys for all configuration settings.
    character(256), dimension(max_keyval)  , intent(in)  :: values        !< Array with values for all configuration keys.
    character(256)                         , intent(out) :: error_message !< Error message, if not empty: echo and stop run.
    !
    ! Local variables
    !
    integer        :: argsinfile   ! 1: Delft3D-FLOW args (-c ddb -M -d 9 etc) are in an ASCII inputfile
    integer        :: i
    integer        :: rolvWait
    logical        :: ex           ! logical to test if a file exists
    character(256) :: engineName
    character(256) :: mdfFile      ! Name of input file of single domain Delft3D-FLOW calculation
    character(256) :: ddbFile      ! Name of input file containing the DomainDecomposition boundaries
    character(256) :: urlFile      ! Name of file to be written, containing remoteOLV hook
    character(256) :: runid
    character(256) :: jarPath
    character(256) :: jrePath
    character(256) :: version_full ! by calling getfullversionstring_deltares_hydro, the version number is visible with the what command
    !
    !! executable statements -------------------------------------------------------
    !
    call getfullversionstring_flow2d3d(version_full)
    !
    ! The output argument error_message MUST have value ' ' to continue the calculation.
    !
    !
    error_message = ' '
    engineName    = 'flow2d3d'
    mdfFile       = ' '
    ddbFile       = ' '
    runid         = ' '
    jarPath       = ' '
    jrePath       = ' '
    urlFile       = ' '
    rolvWait      = 0
    !
    do i = 1, max_keyval
       call small(keys(i),999)
       select case (keys(i))
       case ('component,mdffile')
          mdfFile  = values(i)
       case ('component,filewait')
          !
          ! for debugging:
          ! get some time to attach debugger to process
          !
          do
          inquire(file=trim(values(i)), exist=ex)
             if (ex) exit
             call sleep(1)
          enddo
       case ('component,ddbfile')
          ddbFile  = values(i)
       case ('remoteolv,jarpath')
          jarPath  = values(i)
          if (index(jarPath,'/') /= 0) then
             jarPath = trim(jarPath) // '/'
          else
             ! Use char(92) instead of a backslash; Doxygen does not like it
             jarPath = trim(jarPath) // char(92)
          endif
          jarPath  = trim(jarPath) // 'DelftOnline.jar'
       case ('remoteolv,jrepath')
          jrePath  = values(i)
       case ('remoteolv,wait')
          if (values(i)(1:1)=='y' .or. values(i)(1:1)=='Y') then
             rolvWait = 1
          endif
       case default
          ! error_message = 'WARNING: flow2d3d ignores key "' // trim(keys(i)) // '".'
       end select
    enddo
    !
    ! Exactly one of mdfFile and ddbFile must be defined
    !
    if (mdfFile==' ' .and. ddbFile==' ') then
       write(error_message,'(a)') 'ERROR: flow2d3d cannot find key "MDFfile" or "DDBfile".'
       return
    endif
    if (mdfFile/=' ' .and. ddbFile/=' ') then
       write(error_message,'(a)') 'ERROR: "MDFfile" and "DDBfile" are both defined.'
       return
    endif
    !
    ! Set runid based on mdffile/ddbfile
    !
    if (ddbFile /= ' ') then
       runid = ddbFile
    else
       runid = mdfFile
    endif
    !
    ! remove extension
    !
    i = index(runid, '.', back=.true.)
    if (i /= 0) then
       runid = runid(:i-1)
    endif
    runid = adjustl(runid)
    !
    ! Set urlFile bases on runid, jarPath and jrePath
    !
    if (jarPath/=' ' .and. jrePath/=' ') then
       urlFile = trim(runid) // '.url'
    endif
    !
    ! Go to C/C++ code (via ddexec.cpp and hydra/hydra.cpp) to:
    ! - initialize for remoteOLV
    ! - initialize for DD
    ! - start all DD processes
    ! - call trisim to do the actual calculation
    !
    call dd_execute(runid, ddbFile, jarPath, jrePath, urlFile, rolvWait)
end subroutine runme_core
!
!-------------------------------------------------------------------------------
!
! The following functions were originally located in d3df_dll.f90
!
!-------------------------------------------------------------------------------
!
function Initialize(componentID, schemID) result(retVal)
    use gdp_entry     ! gdpAlloc
    use mod_trisim
    !
    implicit none
    !
    ! result
    integer :: retVal              ! retVal == 0 : success
    !
    ! arguments
    character(*), intent(in) :: componentID  ! RR, RTC, etc.
    character(*), intent(in) :: schemID      ! schem. file (*.fnm)
    !
    ! locals

    character(256)    :: runID

    integer           :: context_id
    integer           :: fsm_flags
    integer           :: numdom        ! Number of subdomains (0 means single domain)
                                       ! as detected by hydra
    integer           :: nummap        ! Number of mappers (one for each DD boundaries connected with this subdomain)
                                       ! as detected by hydra

    character(1)      :: fsm_tracefile

    integer           :: idum, idum2

    integer, external :: esm_init_f
    integer, external :: ESM_Create_f
    integer, external :: trisim_c_init
    
    !
    ! body
    
    print *,'SE_INITIALIZE: |',schemID,'|'
    
    call gdpAlloc(componentID, schemID)

    call getRunIDFromSchemID(schemID, runID)
    !
    ! Add runID to gdp
    !
    gdp%runid = runID

! The following has been translated from delft3dflow.cpp:
!-----------------------------------------------------
    retval = trisim_c_init()

    fsm_flags = 1        ! fsm_flags = ESM_SILENT (from esm.h)

    !idum = ESM_Init_f(fsm_flags)

    !When DelftIO uses shared memory (on Linux only), the environment
    !parameter "DIO_SHM_ESM" is set.
    !The DelftIO shared memory block may never be used by FLOW itself!
    !Therefore, always call ESM_Create.


 ! shared memory context ID; should return -1000
    idum = 0
    idum2 = 0
    !context_id = ESM_Create_f (idum,idum2)
    context_id = -1000   !! this is wrong!!!
        
    !
    numdom = 0
    nummap = 0
    
    fsm_tracefile = ''
    !
!------------------------------------------------------------    

    retval = trisim_init(numdom, nummap, context_id, fsm_flags, fsm_tracefile, runID, gdp)
    if(retVal < 0) return

    retval = trisim_initialise_single_step(gdp)

    ! allocate states is done  on the OpenDA side
    !call allocate_d3d_states(1)

end function Initialize
!
!
!==============================================================================
function GetTimeHorizon(componentID, schemID, startMJD, endMJD) result(retVal)
    use gdp_entry    ! gdp, getOdfData
    use mod_trisim
!    use omi_d3d_flow
    implicit none
    !
    ! result
    integer :: retVal   ! 0: OK
    !
    ! arguments
    character(*)    , intent(in) :: componentID ! RR, RTC, etc., actually it is "D3D_flow"
    character(*)    , intent(in) :: schemID     ! schem. file (*.fnm)
    
    double precision, intent(out):: startMJD      ! Model's start time (MJD)
    double precision, intent(out):: endMJD        ! Model's end time (MJD)    
    
    double precision :: startm      ! Model's start time (minutes)
    double precision :: endm        ! Model's end time (minutes)
    !
    ! body

    startm = dble(gdp%gdinttim%itstrt) * gdp%gdexttim%dt
    endm   = dble(gdp%gdinttim%itfinish) * gdp%gdexttim%dt
    
    ! TODO: remove the -0.5D0 hack by fixing julday in inttim.igs (julday must be double precision)
    startMJD = gdp%gdinttim%julday - 2400000.5D0 + startm / 1440.0 - 0.5D0
    endMJD = gdp%gdinttim%julday - 2400000.5D0 + endm / 1440.0 - 0.5D0
    
    retVal   = 0
end function GetTimeHorizon
!
!
!==============================================================================
function GetDeltaT(componentID, schemID, deltaT_MJD) result(retVal)
    use gdp_entry    ! gdp, getOdfData
!    use omi_d3d_flow
    implicit none
    !
    ! result
    integer :: retVal   ! 0: OK
    !
    ! arguments
    character(*)    , intent(in) :: componentID ! RR, RTC, etc., actually it is "D3D_flow"
    character(*)    , intent(in) :: schemID     ! schem. file (*.fnm)
    double precision, intent(out):: deltaT_MJD      ! Model's DeltaT (in Modified Julian )
    double precision                :: deltaT      ! Model's DeltaT (in minutes  )
    !
    ! body
    !Vortech: acquired from gdp
    !deltaT = gdp%gdinttim%timnow
    deltaT = gdp%gdexttim%dt
    deltaT_MJD = deltaT / 1440.0

    retVal   = 0
end function GetDeltaT
!
!
!==============================================================================
subroutine GetCurrentTime(componentID, schemID, retVal)
    use gdp_entry    ! gdp
    !
    implicit none
    !
    ! arguments
    double precision :: retVal   ! Current Model time (minutes?)
    character(Len=*), intent(in) :: componentID  ! RR, RTC, etc.
    character(Len=*), intent(in) :: schemID      ! schem. file (*.fnm)
    !
    ! locals
    double precision :: Modified_Julian_fromJulianStartDay
    !
    ! body
    !
    ! TODO: remove the -0.5D0 hack by fixing julday in inttim.igs (julday must be double precision)
    Modified_Julian_fromJulianStartDay = gdp%gdinttim%julday - 2400000.5D0 - 0.5D0
    !
    retVal = Modified_Julian_fromJulianStartDay + gdp%gdinttim%timmin/1440.0
    !
end subroutine GetCurrentTime
!
!
!==============================================================================
function PerformTimeStep(componentID, schemID, time_step) result(retVal)
    use gdp_entry    
    use mod_trisim
    use m_openda_exchange_items

    implicit none  

    ! arguments
    character(Len=*), intent(in) :: componentID  ! RR, RTC, etc.
    character(Len=*), intent(in) :: schemID      ! schem. file (*.fnm)
    integer                      :: time_step    ! Current time step

    ! result
    integer :: retVal
    
    ! body

    if ( componentID == '' .or. schemID == '' ) then
       ! todo: ERROR message
    endif
    if ( time_step /= 1 ) then
       ! todo: check if time step is ok
    endif  
   
    retVal = trisim_check_step(gdp)
    if (retVal /= 0) then
      return
    endif
    doLogging = .true.

    retVal = trisim_step(gdp)

    retVal = trisim_prepare_next_step(gdp) 
    doLogging = .false. 
     
end function PerformTimeStep
!
!
!==============================================================================
function Finalize(componentID, schemID) result(retVal)
    use gdp_entry  ! gdpDealloc
    use mod_trisim
    
    implicit none

    integer, external :: trisim_c_finish

    ! result
    integer     :: retVal           ! retVal == 0 : success

    ! arguments
    character(Len=*), intent(in) :: componentID  ! RR, RTC, etc.
    character(Len=*), intent(in) :: schemID      ! schem. file (*.fnm)


    ! body
    retval = trisim_finish(gdp)
    retval = trisim_c_finish()

    call gdpDealloc(componentID, schemID)

end function Finalize
!
!
!==============================================================================
function GETERROR_core(error, errorDescription) result(retVal)
    use gdp_entry  ! gdp
    implicit none
    !
    ! return value
    integer                       :: retVal         ! >=0 : Success; <0 : Error
    !
    ! arguments
    character(len=*), intent(out) :: errorDescription ! error description text
    integer         , intent(in)  :: error            ! error index
    !
    ! locals
    character(256) :: filnam
    character(256) :: message
    logical        :: ex
    !
    ! body
    if ( error /= 0 ) then
        ! todo: use error
    endif
    message = 'For more information:'
    write(filnam,'(2a)') 'tri-diag.',trim(gdp%runid)
    inquire(file = filnam, exist = ex)
    if (ex) then
       write(message,'(4a)') trim(message), ' check file ',trim(filnam), ', or'
    endif
    write(message,'(2a)') trim(message), ' use "OmiEd_cmd.exe -r <case>.opr".'
    errorDescription = trim(message)
    retVal = 0
end function GETERROR_core
!
!
!==============================================================================
subroutine getRunIDFromSchemID(schemID, runID)
    implicit none
    !
    ! arguments
    character(*), intent(in)  :: schemID
    character(*), intent(out) :: runID
    !
    ! locals
    integer :: dotLoc
    integer :: slashLoc
    !
    ! body
    slashLoc = max(  index(schemID, '/', back=.true.), &
                   & index(schemID, '\', back=.true.)   ) 
    dotLoc   = index(schemID(slashLoc+1:), '.mdf', back=.true.)
    if (dotLoc == 0) then
       runID = trim(schemID(slashLoc+1:))
    else
       runID = schemID(slashLoc+1:slashLoc+dotLoc-1)
    endif
end subroutine getRunIDFromSchemID
!
!-------------------------------------------------------------------------------
!
! The following functions were originally located in get_openda_exchange_items.f90
!
!-------------------------------------------------------------------------------
!
function get_exchange_item_count() result(count)
    use gdp_entry    

    ! return value
    integer :: count ! # exchange items

    integer :: noquantities_out, noquantities_in

    ! number of EI's equals number of monitoring stations times output quantities
    noquantities_out = 5 ! temporary

    count = gdp%d%nostat * noquantities_out
    ! temporary: only wind
    if (gdp%gdprocs%wind) count = count + 1

end function get_exchange_item_count

!-----------------------------------------------

function get_exchange_item_id_II(location_id,quantity_id) result(id)
    !
    ! return value
    integer :: id ! # exchange id
    !
    !arguments
    integer :: location_id, quantity_id
    
    id = location_id * 1000 + quantity_id * 1

end function get_exchange_item_id_II

!-------------------------------------------------

function get_exchange_item_id_CI(location_id_c,quantity_id) result(id)
    use gdp_entry 

    use globaldata
    use m_openda_quantities
        
    implicit none
 
    include 'fsm.i'
    include 'tri-dyn.igd'
        
    !arguments
    character(len=*)  :: location_id_c

    integer          :: quantity_id
    
    ! return value
    integer :: id ! # exchange id
    
    integer :: location_id

    
    integer(pntrsize) , pointer :: nambnd
    integer           , pointer :: nto
    integer           , pointer :: ntof
    integer           , pointer :: ntoq
    integer           , pointer :: nostat
    character(20)     , dimension(:)    , pointer :: namst

    id = -1

    nambnd              => gdp%gdr_i_ch%nambnd
    nto                 => gdp%d%nto
    ntof                => gdp%d%ntof
    ntoq                => gdp%d%ntoq
    nostat              => gdp%d%nostat

    namst      => gdp%gdstations%namst
            
    if ((quantity_id == bound_HQ)  .or. (quantity_id == bound_temp)   & 
      .or. (quantity_id == bound_astroH) .or. (quantity_id == bound_salt)) then
             
    ! we assume that the location_id string refers to a unique boundary name,
    ! to be found in the *.bnd file
    ! We only take time serie boundaries into account.
    ! This has to be translated to a location_id, with ntof < location_id < nto
       call find_boundary(location_id_c, ch(nambnd),nto,ntof,ntoq, location_id)
       if (quantity_id == bound_astroH .and. location_id .gt. ntof) then
         print *,' WARNING: boundary ',location_id_c,'is NOT an astroH boundary!' 
       endif
       if (quantity_id .ne. bound_astroH .and. location_id .le. ntof+ntoq) then
         print *,' WARNING: boundary ',location_id_c,'is NOT a timeseries boundary!' 
       endif
       
       
    elseif ((quantity_id == waterlevel)) then
       call find_monitorpoint(location_id_c, namst,nostat, location_id)    
    elseif ((quantity_id == windu) .or. (quantity_id == windv) .or.      &
            (quantity_id == windgu) .or. (quantity_id == windgv)) then
       location_id = 1    ! windu/windv have only one location; 
                          ! noise field for gu and gv will be determined later on.      
    else
      print *,'SE_get_exchange_item_id_CI: only supported for wind, boundaries and monitor points'
      return
    endif

    id = location_id * 1000 + quantity_id * 1

end function get_exchange_item_id_CI

!------------------------------------------------
subroutine find_boundary(location_id_c, nambnd,nto,ntof,ntoq,location_id)
  
   implicit none
   character(20), dimension(nto) , intent(in)  :: nambnd

   character(*)     :: location_id_c

   integer,          intent(in) :: nto, ntof, ntoq 
   integer,          intent(out) :: location_id
   
   integer :: i, lenid
   logical :: lfound

   lenid = len(location_id_c)
   lenid = min(20,lenid)
   
   lfound = .false.
   do i = 1, nto
 
      if (location_id_c(1:lenid)==nambnd(i)(1:lenid)) then
         lfound = .true.
         location_id = i
      endif
   enddo
   
   if (.not. lfound) then
      print *, 'WARNING: exchange item ',trim(location_id_c), ' has no corresponding boundary name.'
      location_id = -1
   else
 
   endif   
    
end subroutine find_boundary


!-----------------------------------------------
subroutine find_monitorpoint(location_id_c, namst,nostat,location_id)
  
   implicit none
   
   integer,          intent(in) :: nostat 
   character(20), dimension(nostat) , intent(in)  :: namst
   character(*)     :: location_id_c
   integer,          intent(out) :: location_id
   
   integer :: i, lenid
   logical :: lfound

   lenid = len(location_id_c)
   lenid = min(20,lenid)
   
   lfound = .false.
   do i = 1, nostat
 
      if (location_id_c(1:lenid)==namst(i)(1:lenid)) then
         lfound = .true.
         location_id = i
      endif
   enddo
   
   if (.not. lfound) then
      print *, 'WARNING: exchange item ',trim(location_id_c), ' has no corresponding monitor station name.'
      location_id = -1
   endif   
    
end subroutine find_monitorpoint


!-----------------------------------------------
subroutine get_location_id_and_quantity_id(exchange_item_id, location_id,quantity_id) 
    !arguments
    integer, intent(in) :: exchange_item_id
    integer, intent(out) :: location_id, quantity_id
    
    location_id  = exchange_item_id / 1000
    quantity_id = mod(exchange_item_id, 1000)

end subroutine get_location_id_and_quantity_id

!-------------------------------------------------

function get_values_count_for_time_span(instance, exchange_item_id, start_time, end_time) result(ret_val)
    !
    ! return value
    integer :: ret_val

    ! arguments
    integer         , intent(in) :: instance         ! model instance
    integer         , intent(in) :: exchange_item_id ! type and location of quantity 
                                                     ! (e.g. discharge or waterlevel at point M7)
    double precision, intent(in) :: start_time       ! start time of values
    double precision, intent(in) :: end_time         ! end time of values

    ! locals
    integer :: start_index   ! index in time series values for start_time
    integer :: end_index     ! index in time series values for end_time

    ! body

    ret_val = -1 ! indices not ok
    
    !if (valid_model_instance(instance)) then

    !endif

    ! quick solution:
    ! always get the latest value computed in postpr > tstat. So the answer is always 1.
    ret_val = 1


    if (ret_val .lt. 0) then
        write(*,'(A,I2)') 'Error in get_values_count_for_time_span: ', ret_val
    else
  !      write(*,'(A,I4,A,F8.2,A,F8.2,A,I2)') 'get_values_count_for_time_span(', &
  !                                  exchange_item_id,&
  !                                  ',', start_time, ',', end_time, '): ', ret_val
    endif

end function get_values_count_for_time_span

!-----------------------------------------------------------


function get_values_for_time_span(exchange_item_id, start_time, end_time, nvals,values) result(ret_val)
    use m_openda_quantities
    use gdp_entry
    
    include 'fsm.i'
    include 'tri-dyn.igd'
    
    ! return value
    integer :: ret_val

    ! arguments
    integer                           , intent(in)    :: exchange_item_id ! type and location of quantity 
                                                                          ! (e.g. discharge or waterlevel at point M7)
    double precision                  , intent(in)    :: start_time       ! start time of bc values
    double precision                  , intent(in)    :: end_time         ! end time of bc values
    integer                           , intent(in)    :: nvals            ! size of values array
    double precision, dimension(nvals), intent(inout) :: values           ! returned values

    integer :: location_id, quantity_id

    integer(pntrsize)            , pointer :: s1
    integer(pntrsize)            , pointer :: zcuru
    integer(pntrsize)            , pointer :: zcurv
    integer   , dimension(:,:)   , pointer :: mnstat
    
    ! body

    mnstat      => gdp%gdstations%mnstat
    s1          => gdp%gdr_i_ch%s1
   ! zwl          => gdp%gdr_i_ch%zwl
    zcuru        => gdp%gdr_i_ch%zcuru
    zcurv        => gdp%gdr_i_ch%zcurv
    
    ret_val = -1 ! indices not ok

    call get_location_id_and_quantity_id(exchange_item_id,location_id,quantity_id)

    ! note: for now, all quantities are 1D so values is always one-dimensional.


    if (quantity_id .eq. waterlevel) then
       call ei_copy_sep(r(s1), mnstat, values, location_id, gdp%d%nostat,     &
            gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub)
    
    elseif (quantity_id .eq. x_velocity) then   
       call ei_copy_vel(r(zcuru),values(1), location_id, gdp%d%nostat,gdp%d%kmax) 

    elseif (quantity_id .eq. y_velocity) then   
       call ei_copy_vel(r(zcurv),values(1), location_id, gdp%d%nostat,gdp%d%kmax) 
           
    elseif (quantity_id .eq. x_discharge) then 
      call ei_provide_copy_disch(r(zqxk),values(1), location_id, gdp%d%nostat,gdp%d%kmax) 

    elseif (quantity_id .eq. y_discharge) then 
      call ei_provide_copy_disch(r(zqyk),values(1), location_id, gdp%d%nostat,gdp%d%kmax)

    endif
    
    ret_val = 0

    if (ret_val /= 0) then
        write(*,'(A,I2)') 'Error in get_values_for_time_span: ', ret_val
    else
   !     write(*,'(A,I4,A,F8.2,A,F8.2,A)') 'get_values_for_time_span(', &
   !                                exchange_item_id,&
   !                                 ',', start_time, ',', end_time, '):'
   !     write(*,*) '   ', values
    endif

end function get_values_for_time_span

!------------------------------------------------------------
function set_noise_for_time_span(exchange_item_id, start_time, end_time, operation, nvals,values) result(ret_val)
    use m_openda_quantities
    use m_openda_exchange_items, only : set_openda_buffer
    use gdp_entry
    
    implicit none
    
    ! return value
    integer :: ret_val

    ! arguments
    integer                           , intent(in) :: exchange_item_id ! type and location of quantity 
                                                                       ! (e.g. discharge or waterlevel at point M7)
    double precision                  , intent(in) :: start_time       ! start time of bc values
    double precision                  , intent(in) :: end_time         ! end time of bc values
    integer                           , intent(in) :: operation        ! operation: oper_multiply, oper_add, oper_set
    integer                           , intent(in) :: nvals            ! size of values array
    double precision, dimension(nvals), intent(in) :: values           ! returned values

    ! locals
    integer                     :: location_id, quantity_id
    integer(pntrsize) , pointer :: disch


    ! body
    disch         => gdp%gdr_i_ch%disch
   
    ret_val = -1 ! indices not ok


    call get_location_id_and_quantity_id(exchange_item_id,location_id,quantity_id)

    ! note: for now, all quantities except windgu and windgv are 1D so values is always one-dimensional. 
    if (quantity_id .eq. src_discharge) then
    !  call ei_accept_copy_disch(values, r(disch), location_id, gdp%d%nsrc)

    elseif (quantity_id .eq. windu) then
       call set_openda_buffer(values(1),1,location_id, quantity_id, operation)
    elseif (quantity_id .eq. windv) then
       call set_openda_buffer(values(1),1,location_id, quantity_id, operation)

   ! wind noise fields: The noise field contains nval parameters. Corresponding locations are also provided,
   ! stored using triples (xloc, yloc, val) in values
    elseif (quantity_id .eq. windgu) then
       call set_openda_buffer(values(1:nvals),nvals, location_id, quantity_id, operation)
    elseif (quantity_id .eq. windgv) then
       call set_openda_buffer(values(1:nvals),nvals,location_id, quantity_id, operation)
    
    ! We also assume that for boundaries, ends a and b are simultaneously adjusted.
    ! Hence, also here, only one-dimensional values.
    elseif (quantity_id .eq. bound_HQ) then
       call set_openda_buffer(values(1),1,location_id, quantity_id, operation)

    elseif (quantity_id .eq. bound_temp) then
       call set_openda_buffer(values(1),1,location_id, quantity_id, operation)
       
    elseif (quantity_id .eq. bound_salt) then
       call set_openda_buffer(values(1),1,location_id, quantity_id, operation)       

    elseif (quantity_id .eq. bound_astroH) then
       call set_openda_buffer(values(1),1,location_id, quantity_id, operation)

    endif
    
    ret_val = 0

    if (ret_val /= 0) then
        write(*,'(A,I2)') 'Error in set_values_for_time_span: ', ret_val
    else
    !    write(*,'(A,I4,A)') 'set_values_for_time_span:', &
    !                              exchange_item_id,&
    !                                 '):'
    !    write(*,*) 'delta:   ', values
    endif

end function set_noise_for_time_span

!----------------------------------------------------------------

 subroutine ei_copy_sep(s1, mnstat, ei_openda, ii, nostat, nlb,nub,mlb,mub)
    use precision   ! for fp    
    implicit none
    integer :: ii, nostat, nlb,nub,mlb,mub
   
    real(hp), dimension(1)    :: ei_openda
    integer, dimension(2,nostat) :: mnstat
    integer :: m,n
    real(fp)  , dimension(nlb:nub, mlb:mub) , intent(in)  :: s1   

    m        = mnstat(1, ii)
    n        = mnstat(2, ii)
    ei_openda(1)  = s1(n, m)



  end subroutine ei_copy_sep
  
!------------------------------------------------------------------  

 subroutine ei_copy_vel(zcuruv, ei_openda, ii, nostat, kmax)
    use precision   ! for fp    
    implicit none
    integer :: ii, nostat, kmax
    real(fp), dimension(nostat,kmax)  :: zcuruv
    real(hp)   :: ei_openda

    ! for now, use velocities at top layer
    ei_openda = zcuruv(ii,1)
 
   ! or perform a vertical integration ...
  end subroutine ei_copy_vel

!-------------------------------------

 subroutine ei_provide_copy_disch(zqxyk, ei_openda, ii, nostat, kmax)
    use precision   ! for fp    
    implicit none
    integer :: ii, nostat, kmax
    real(fp), dimension(nostat,kmax)  :: zqxyk
    real(hp)   :: ei_openda
   ! for now, use discharges at top layer
    ei_openda = zqxyk(ii,1)

  end subroutine ei_provide_copy_disch

!-------------------------------------

 subroutine ei_accept_copy_disch(ei_openda, disch,ii, nsrc)
    use precision   ! for fp    
    implicit none
    integer :: ii, nsrc
    real(fp), dimension(nsrc)  :: disch
    real(hp)   :: ei_openda
   ! 
   write(*,*) 'FORCINGS: discharge source ',ii, 'of value ',disch(ii), 'adjusted with ',ei_openda 
   disch(ii)= disch(ii) + ei_openda 
  end subroutine ei_accept_copy_disch

!------------------------------------------------------------

function exchange_item_text(exchange_item_id) result (retval)
! return value
integer :: exchange_item_id
character*4 :: retval 

 write(retval, '(I4.4)') exchange_item_id

end function exchange_item_text
