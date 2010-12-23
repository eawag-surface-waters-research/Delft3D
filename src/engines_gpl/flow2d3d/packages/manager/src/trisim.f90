subroutine trisim (numdom, nummap, context_id, fsm_flags, fsm_tracefile, runid)
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
!!--description-----------------------------------------------------------------
!
!    Function: Main routine for the 2d / 3d program
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use SyncRtcFlow
    use timers
    use meteo
    !
    ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
    ! Activate the following line
    ! See also statements below
    !
    ! use ifcore
    !
    ! global data declaration; compare with include 'globdat.igd'
    !
    use globaldata
    implicit none
    type(globDat),pointer :: gdp
    !
    integer                       , pointer :: lundia
    integer                       , pointer :: lunprt
    integer                       , pointer :: iphisi
    integer, dimension(:)         , pointer :: ipmap
    logical                       , pointer :: dredge
    logical                       , pointer :: struct
    integer                       , pointer :: numdomains
    integer                       , pointer :: nummappers
    character(6)                  , pointer :: prognm
    integer                       , pointer :: rtcmod
    include 'flow_steps_f.inc'
    include 'fsm.i'
!
! Parameters
!
    integer       , intent(in)  :: context_id
    integer       , intent(in)  :: fsm_flags
    integer       , intent(in)  :: numdom        ! Number of subdomains (0 means single domain)
                                                 ! as detected by hydra
    integer       , intent(in)  :: nummap        ! Number of mappers (one for each DD boundaries connected with this subdomain)
                                                 ! as detected by hydra
    character(*)  , intent(in)  :: fsm_tracefile
    character(256)              :: runid
!
! Local variables
!
    integer            :: fsmstatus
    integer            :: i
    integer            :: ic           ! Length of character parameter CASE 
    integer            :: icheck
    integer            :: initi        ! Control parameter =1 initialization =2 initialization and read restart information from the communication file =3 no initialization 
    integer            :: it01         ! Reference date in yymmdd 
    integer            :: it02         ! Reference time in hhmmss 
    integer            :: itb          ! Start time of computational interval 
    integer            :: ite          ! End time of computational interval 
    integer            :: itima        ! Time to start simulation (N * tscale) according to DELFT3D conventions 
    integer            :: itlen        ! Lenght of the tide cycle 
    integer            :: lenid
    integer            :: lunid
    integer            :: luntri       ! Unit number for trigger file for TRISIM for running programs simultaniously 
    integer            :: nhystp
    integer, external  :: newlun
    integer, external  :: fsmtrf
    logical            :: alone        ! TRUE when flow runs stand-alone, FALSE when flow is part of morsys 
    logical            :: ex
    logical            :: init         ! Flag=TRUE when initialisation is re- quired (always the case if FLOW is used stand alone) 
    logical            :: lexist
    logical            :: mainys       ! Logical flag for FLOW is main porgram (TRUE) for writing output 
    logical            :: opend        ! Help logical var. to determine whether each of the output files was opened 
    real(fp)           :: tscale       ! Basic unit time 
    character(12)      :: filmrs       ! File name for DELFT3D_MOR FLOW input file (MD-flow.xxx) 
    character(12)      :: filsim       ! Name for trigger file for TRISIM for running programs simultaniously 
    character(256)     :: case         ! Project identification (a non-blank character string presumed) 
    character(256)     :: comfil       ! Communication file name 
    character(256)     :: filmd        ! File name for MD FLOW file 
    character(256)     :: trifil       ! File name for FLOW NEFIS output files (tri"h/m"-"casl""labl".dat/def) 
    character(4)       :: subsys       ! Sub-system definition of Delft3D here SUBSYS = 'flow' 
    character(5)       :: filid
    character(5)       :: versio       ! Version nr. of the current package 
    !
    ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
    ! Activate the following line
    ! See also statements below
    !
    ! INTEGER*4 OLD_FPE_FLAGS, NEW_FPE_FLAGS
!
!! executable statements -------------------------------------------------------
!
    !
    ! Initializes MPI
    !
    call dfinitmpi
    !
    ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
    ! Activate the following two lines
    ! See also use statement above
    !
    ! NEW_FPE_FLAGS = FPE_M_TRAP_OVF + FPE_M_TRAP_DIV0 + FPE_M_TRAP_INV
    ! OLD_FPE_FLAGS = FOR_SET_FPE (NEW_FPE_FLAGS)
    !
    ! Start simulation performance measurement
    !
    call StartComputation
    !
    ! Initialization using a semaphore
    ! Related vseminit is in tricom.f90
    !
    call pseminit
    !
    ! create and initialize GDP structure
    !
    allocate(gdp)
    call gdp_alloc(gdp)
    call initsafe(gdp)
    call timers_init(gdp)
    call timer_start(timer_total, gdp)
    call timer_start(timer_init, gdp)
    !
    ! esm/fsm initialization
    !
    fsmstatus = fsmini (context_id, fsm_flags)
    if (len (fsm_tracefile) > 0) then
       fsmstatus = fsmtrf (fsm_tracefile)
    endif
    !
    lundia       => gdp%gdinout%lundia
    lunprt       => gdp%gdinout%lunprt
    iphisi       => gdp%gdinttim%iphisi
    ipmap        => gdp%gdinttim%ipmap
    dredge       => gdp%gdprocs%dredge
    struct       => gdp%gdprocs%struct
    numdomains   => gdp%gdprognm%numdomains
    nummappers   => gdp%gdprognm%nummappers
    prognm       => gdp%gdprognm%prognm
    rtcmod       => gdp%gdrtc%rtcmod
    !
    ! Initialize local parameters, including IPHISI and IPMAP(1)
    ! in case program crashes the test below can be performed anyway
    !
    init     = .true.
    filmrs   = ' '
    subsys   = 'flow'
    alone    = .true.
    !
    iphisi   = 0
    ipmap(1) = -1
    !
    rtcmod   = noRTC
    icheck   = 0
    !
    ! Store numdom (counted by Hydra) in numdomains (in GDP-structure)
    ! For single domain cases, Hydra does not count at all and numdom is zero.
    !
    numdomains = max(1,numdom)
    !
    ! Store nummap (counted by Hydra) in nummapperss (in GDP-structure)
    !
    nummappers = nummap
    if (runid==' ') then
       !
       ! First try to read runid from file called RUNID
       ! This simplifies fluidmud synchronisation
       !
       filid = 'runid'
       inquire (file = filid, exist = ex)
       if (ex) then
          lunid = newlun(gdp)
          open (lunid, file = filid, form = 'formatted', status = 'old')
          read (lunid, '(a)') runid
          close (lunid)
       else
          runid = ' '
       endif
    else
       !
       ! Remove (possible) trailing '\0' from c-code
       !
       do i = 1, len(runid)
          if (ichar(runid(i:i)) == 0 .or. ichar(runid(i:i)) == 10) runid(i:i) = ' '
       enddo
    endif
    runid = adjustl(runid)
    !
    ! Platform dependent initialization
    !
    call pldep
    !
    ! Run TDATOM
    !
    if (.not.parll .or. inode == master) then
       call tdatmain(runid, alone, subsys, filmrs, icheck, gdp) 
    endif
    call dfbroadc(icheck, 1, dfint,gdp)
    call dfsync(gdp)
    if (icheck /= 0 ) then
       write (*, '(a)') 'ABORT: error returned by tdatmain'
       lundia = 0
       call d3stop(1,gdp)
    endif
    !
    ! Set program name (after running tdatom)
    !
    prognm = 'TRISIM'
    !
    ! Determine by trigger-file if RTC is running as well
    !
    luntri = newlun(gdp)
    filsim = 'TMP_SYNC.RUN'
    inquire (file = filsim, exist = lexist)
    if (lexist) then
       open (luntri, file = filsim, form = 'unformatted', status = 'unknown')
       read (luntri) icheck
       close (luntri)
       !
       ! Check 'RUNRTC' by telephone
       !
       if (icheck==786782) then
          rtcmod = dataFromRTCToFLOW
       else
          write (*, '(a)') 'Trigger-file TMP_SYNC.RUN not made by TDATOM'
          call d3stop(1         ,gdp       )
       endif
    endif
    !
    ! Initialize sub-system for Delft3D-FLOW
    !
    call defsub(subsys    ,gdp       )
    !
    ! Start FLOW simulation program
    !
    call noextspaces(runid     ,lenid     )
    if (init) then
       !
       ! Read  dimensions of arrays and declare array pointers
       !
       call tripoi(runid, filmrs, versio, filmd, &
                 & alone, gdp)
       if (gdp%errorcode > 0) then
          if (rtcmod == dataFromRTCToFLOW) then
             call timer_start(timer_wait, gdp)
             call syncflowrtc_quit
             call timer_stop(timer_wait, gdp)
             rtcmod = noRTC
          endif
          goto 9999
       endif
    endif
    !
    ! Initialize time frame parameters for stand alone program
    !
    initi  = 1
    !
    it01   = 0
    it02   = 0
    !
    itb    = 1
    ite    = -1
    itlen  = 0
    tscale = 1.0
    !
    itima  = 0
    !
    mainys = .true.
    itima  = 0
    !
    ! Initialize communication file name
    ! NOTE: case may never be only blanks
    !
    case = runid
    call noextspaces(case      ,ic        )
    !
    comfil(1:4) = 'com-'
    comfil(5:)  = case(1:ic)
    !
    trifil(1:5) = 'trix-'
    trifil(6:)  = case(1:ic)
    !
    ! Insert node number in file names
    !
    if ( parll ) then
       write(comfil(5+ic:5+ic+4),'(a,i3.3)') '-',inode
    endif
    !
    ! Start FLOW simulation
    !
    call tricom(tscale    ,it01      ,it02      ,itb       ,ite       , &
              & itlen     ,initi     ,itima     ,mainys    ,comfil    , &
              & runid     ,trifil    ,versio    ,alone     , &
              & gdp       )
    write(lundia,*)
    write(lundia,'(a)') '*** Simulation finished *******************************************************'
    write(lundia,*)
    !
    ! Write diagnostics and close file
    !
 9999 continue
    !
    call timer_stop(timer_total, gdp)
    call timers_finish(gdp)
    if (init) then
       call triend(runid,gdp)
       if (gdp%errorcode > 0) then
          !
          ! User interaction is removed
          !
          !
          ! To avoid statemachine problems:
          !
          nhystp = nxtstp(d3dflow_init, gdp)
       endif
    endif
    !
    ! Close intermediate files TMP//runid//.*
    !
    call delfil(runid     ,filmd     ,gdp       )
    !
    ! Close diagnostic file, print file and dredge file
    ! No prints requested (LUNPRT still unit 8 see SYSINI) delete file
    !
    inquire (lundia, opened = opend)
    if (opend) close (lundia)
    if (iphisi>0 .or. ipmap(1)>=0) then
       !
       ! Only then the tri-prt file can actually be present
       !
       inquire (lunprt, opened = opend)
       if (opend) then
          close (lunprt)
       endif
    endif
    !
    call deallocmeteo(gdp%runid)
    !! TODORE call gdp_dealloc(gdp)
    !! TODORE deallocate(gdp)
    !
    ! Finish using a semaphore
    ! Related psemnefis is in tricom.f90
    ! This used to be a vsemfinish
    !
    call vsemnefis
    !
    ! Tell gaws (Global ADI Wang Solver) and mapper we're done.
    !
    if (nummappers >= 1) then
        call gwsslv(-1)
        nhystp = nxtstp(d3dflow_finish, gdp)
    endif
    !
    ! Stop simulation performance measurement
    !
    call EndComputation
    !
    ! Finalizes MPI
    !
    if (parll) then
       call dfexitmpi(0)
    endif
end subroutine trisim
