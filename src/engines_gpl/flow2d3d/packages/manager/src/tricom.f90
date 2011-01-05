subroutine tricom(tscale    ,it01      ,it02      ,itb       ,ite       , &
                & itlen     ,initia    ,itima     ,mainys    ,comfil    , &
                & runid     ,trifil    ,versio    ,alone     ,gdp       )
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
!    Function: - Read md-file
!              - Initialize input arrays and verify input
!              - Perform a FLOW computation for the
!                time interval (tb, te)
!                In case of a stand-alone system ITB and ITE
!                are set 0. Besides if WAVE = TRUE then ITLEN
!                and TSCALE will be read from the communication
!                file, else they will also be set 0.
!              - Update the communication file
!              NOTE: TRICOM called by main module of DELFT3D then
!                    ITLEN and TSCALE always defined
!                    TRICOM called by module TRISIM then ITLEN=0
!                    and ITLEN can be read from comm. file or are
!                    defined by the FLOW time frame
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use meteo
    use SyncRtcFlow
    use sync_flm
    use sync_flowcouple
    use sync_flowwave
    use timers
    use D3DOnline
    use D3DPublish
    use D3D_Sobek 
    use globaldata
    use dfparall
    use d3d_olv_class
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'fsm.i'
    include 'flow_steps_f.inc'
    include 'tri-dyn.igd'
    real(fp)                            , pointer :: timnow
    real(fp)                            , pointer :: thr
    integer                             , pointer :: ncmax
    integer                             , pointer :: nmax
    integer                             , pointer :: mmax
    integer                             , pointer :: nlb
    integer                             , pointer :: nub
    integer                             , pointer :: mlb
    integer                             , pointer :: mub
    integer                             , pointer :: ddbound
    integer                             , pointer :: nmaxus
    integer                             , pointer :: kmax
    integer                             , pointer :: nmaxd
    integer                             , pointer :: mmaxd
    integer                             , pointer :: jstart
    integer                             , pointer :: nmmaxj
    integer                             , pointer :: nmmax
    integer                             , pointer :: lmax
    integer                             , pointer :: lsal
    integer                             , pointer :: lsts
    integer                             , pointer :: lstsc
    integer                             , pointer :: lstsci
    integer                             , pointer :: lsed
    integer                             , pointer :: lsedtot
    integer                             , pointer :: lsecfl
    integer                             , pointer :: lsec
    integer                             , pointer :: ltur
    integer                             , pointer :: ltur2d
    integer                             , pointer :: noroco
    integer                             , pointer :: norow
    integer                             , pointer :: nocol
    integer                             , pointer :: nto
    integer                             , pointer :: kc
    integer                             , pointer :: nrob
    integer                             , pointer :: nsrc
    integer                             , pointer :: nostat
    integer                             , pointer :: ntruv
    integer                             , pointer :: ntru
    integer                             , pointer :: nsluv
    integer                             , pointer :: itdate
    real(fp)                            , pointer :: tstart
    real(fp)                            , pointer :: tstop
    real(fp)                            , pointer :: dt
    real(fp)                            , pointer :: tunit
    real(fp)          , dimension(:)    , pointer :: dm
    real(fp)          , dimension(:)    , pointer :: dg
    real(fp)          , dimension(:,:)  , pointer :: frac
    real(fp)                            , pointer :: cp
    real(fp)                            , pointer :: sarea
    real(fp)                            , pointer :: fclou
    integer                             , pointer :: lunmd
    integer                             , pointer :: lundia
    integer                             , pointer :: lunprt
    integer                             , pointer :: lunscr
    real(fp)                            , pointer :: timsec
    real(fp)                            , pointer :: timmin
    real(fp)                            , pointer :: timhr
    integer                             , pointer :: itstrt
    integer                             , pointer :: itstop
    integer                             , pointer :: itlfsm
    integer                             , pointer :: itmapf
    integer                             , pointer :: itmapi
    integer                             , pointer :: itmapl
    integer                             , pointer :: ithisf
    integer                             , pointer :: ithisi
    integer                             , pointer :: ithisl
    integer                             , pointer :: itcomf
    integer                             , pointer :: itcomi
    integer                             , pointer :: itcoml
    integer                             , pointer :: itdrof
    integer                             , pointer :: itdroi
    integer                             , pointer :: itnflf
    integer                             , pointer :: itnfli
    integer                             , pointer :: itnfll
    integer                             , pointer :: itrsti
    integer                             , pointer :: iphisf
    integer                             , pointer :: iphisi
    integer                             , pointer :: iphisl
    integer           , dimension(:)    , pointer :: ipmap
    integer                             , pointer :: itimtt
    integer                             , pointer :: ittrtu
    integer                             , pointer :: itiwei
    integer                             , pointer :: itdiag
    integer                             , pointer :: julday
    integer                             , pointer :: ntstep
    real(fp)                            , pointer :: tmor
    real(fp)                            , pointer :: rdc
    integer                             , pointer :: itmor
    type (bedbndtype) , dimension(:)    , pointer :: morbnd
    logical                             , pointer :: densin
    logical                             , pointer :: multi
    character(256)                      , pointer :: mmsyncfilnam
    real(fp)                            , pointer :: hdt
    character(6)                        , pointer :: momsol
    real(fp)                            , pointer :: rhow
    real(fp)                            , pointer :: ag
    integer                             , pointer :: iro
    logical                             , pointer :: wind
    logical                             , pointer :: temp
    logical                             , pointer :: const
    logical                             , pointer :: culvert
    logical                             , pointer :: dredge
    logical                             , pointer :: drogue
    logical                             , pointer :: wave
    logical                             , pointer :: waveol
    logical                             , pointer :: threed
    logical                             , pointer :: secflo
    logical                             , pointer :: struct
    logical                             , pointer :: sedim
    logical                             , pointer :: htur2d
    logical                             , pointer :: flmd2l
    logical                             , pointer :: mudlay
    logical                             , pointer :: mudwave
    logical                             , pointer :: coupleact
    logical                             , pointer :: couplemod
    logical                             , pointer :: zmodel
    logical                             , pointer :: nonhyd
    logical                             , pointer :: roller
    logical                             , pointer :: wavcmp
    logical                             , pointer :: cnstwv
    logical                             , pointer :: lftrto
    logical                             , pointer :: snelli
    logical                             , pointer :: sbkol
    logical                             , pointer :: xbeach
    integer                             , pointer :: numdomains
    integer                             , pointer :: nummappers
    integer                             , pointer :: alfas
    integer                             , pointer :: ampbc
    integer                             , pointer :: c
    integer                             , pointer :: cgc
    integer                             , pointer :: ctr
    integer                             , pointer :: dircom
    integer                             , pointer :: dircos
    integer                             , pointer :: dirsin
    integer                             , pointer :: dis
    integer                             , pointer :: disch
    integer                             , pointer :: discom
    integer                             , pointer :: dp
    integer                             , pointer :: dpc
    integer                             , pointer :: dps
    integer                             , pointer :: dpu
    integer                             , pointer :: dpv
    integer                             , pointer :: ewabr0
    integer                             , pointer :: ewabr1
    integer                             , pointer :: ewave0
    integer                             , pointer :: ewave1
    integer                             , pointer :: fxw
    integer                             , pointer :: fyw
    integer                             , pointer :: grmasu
    integer                             , pointer :: grmasv
    integer                             , pointer :: gro
    integer                             , pointer :: gsqd
    integer                             , pointer :: gsqs
    integer                             , pointer :: guu
    integer                             , pointer :: guv
    integer                             , pointer :: gvu
    integer                             , pointer :: gvv
    integer                             , pointer :: hkru
    integer                             , pointer :: hkrv
    integer                             , pointer :: hrmcom
    integer                             , pointer :: hrms
    integer                             , pointer :: hu
    integer                             , pointer :: huvw
    integer                             , pointer :: hv
    integer                             , pointer :: msucom
    integer                             , pointer :: msvcom
    integer                             , pointer :: ombc
    integer                             , pointer :: phibc
    integer                             , pointer :: qu
    integer                             , pointer :: qxk
    integer                             , pointer :: qxkr
    integer                             , pointer :: qxkw
    integer                             , pointer :: qyk
    integer                             , pointer :: qykr
    integer                             , pointer :: qykw
    integer                             , pointer :: r0
    integer                             , pointer :: r1
    integer                             , pointer :: rbnd
    integer                             , pointer :: rbuff
    integer                             , pointer :: rho
    integer                             , pointer :: rint
    integer                             , pointer :: rlabda
    integer                             , pointer :: rob
    integer                             , pointer :: rtur1
    integer                             , pointer :: s0
    integer                             , pointer :: s1
    integer                             , pointer :: sbuu
    integer                             , pointer :: sbvv
    integer                             , pointer :: sig
    integer                             , pointer :: ssuu
    integer                             , pointer :: ssvv
    integer                             , pointer :: teta
    integer                             , pointer :: thetbc
    integer                             , pointer :: thick
    integer                             , pointer :: tp
    integer                             , pointer :: tpcom
    integer                             , pointer :: tps
    integer                             , pointer :: tpscom
    integer                             , pointer :: u1
    integer                             , pointer :: umean
    integer                             , pointer :: uorb
    integer                             , pointer :: ubot
    integer                             , pointer :: ubcom
    integer                             , pointer :: uvdist
    integer                             , pointer :: v1
    integer                             , pointer :: vmean
    integer                             , pointer :: voldis
    integer                             , pointer :: volum1
    integer                             , pointer :: wlen
    integer                             , pointer :: wlcom
    integer                             , pointer :: wphy
    integer                             , pointer :: ws
    integer                             , pointer :: wsu
    integer                             , pointer :: wsucom
    integer                             , pointer :: wsv
    integer                             , pointer :: wsvcom
    integer                             , pointer :: xcor
    integer                             , pointer :: xz
    integer                             , pointer :: ycor
    integer                             , pointer :: yz
    integer                             , pointer :: zdist
    integer                             , pointer :: dzs1
    integer                             , pointer :: res
    integer                             , pointer :: rl
    integer                             , pointer :: xj
    integer                             , pointer :: guz
    integer                             , pointer :: gvz
    integer                             , pointer :: gud
    integer                             , pointer :: gvd
    integer                             , pointer :: gsqiu
    integer                             , pointer :: gsqiv
    integer                             , pointer :: ibuff
    integer                             , pointer :: irocol
    integer                             , pointer :: iroll
    integer                             , pointer :: itdro
    integer                             , pointer :: kcs
    integer                             , pointer :: kcu
    integer                             , pointer :: kcv
    integer                             , pointer :: kfs
    integer                             , pointer :: kfu
    integer                             , pointer :: kfv
    integer                             , pointer :: kspu
    integer                             , pointer :: kspv
    integer                             , pointer :: kzs
    integer                             , pointer :: kzu
    integer                             , pointer :: kzv
    integer                             , pointer :: mnbnd
    integer                             , pointer :: mnksrc
    integer                             , pointer :: kfsmin
    integer                             , pointer :: kfsmax
    integer                             , pointer :: izmodl
    integer                             , pointer :: nambar
    integer                             , pointer :: nambnd
    integer                             , pointer :: namcon
    integer                             , pointer :: namsrc
    character(256)                      , pointer :: restid
    integer                             , pointer :: rtcmod
    logical                             , pointer :: rtcact
    integer                             , pointer :: rtc_domainnr
    character(256)                      , pointer :: sbkConfigFile
    logical                             , pointer :: tstprt
    logical                             , pointer :: sferic
    integer                             , pointer :: ditcof
    integer                             , pointer :: ditcol
    integer                             , pointer :: keva
    integer                             , pointer :: ktemp
    integer                             , pointer :: lturi
    integer                             , pointer :: nfltyp
    integer                             , pointer :: icreep
    integer                             , pointer :: nh_level
    real(fp)                            , pointer :: betac
    real(fp)                            , pointer :: dml
    real(fp)                            , pointer :: grdang
    real(fp)                            , pointer :: saleqs
    real(fp)                            , pointer :: temeqs
    character(1)                        , pointer :: temint
    character(1)                        , pointer :: evaint
    character(1)                        , pointer :: forfuv
    character(1)                        , pointer :: forfww
    character(4)                        , pointer :: rouflo
    character(4)                        , pointer :: rouwav
    character(8)                        , pointer :: method
    character(8)                        , pointer :: dischy
    character(8)                        , pointer :: solver
    character(8)                        , pointer :: disctr
    character(12)                       , pointer :: tkemod
    character(13)                       , pointer :: trasol
    character(19)                       , pointer :: prsmap
    character(21)                       , pointer :: selmap
    character(23)                       , pointer :: prshis
    character(23)                       , pointer :: selhis
    character(36)                       , pointer :: tgfcmp
    include 'delftonline.i'
!
! Local parameters
!
    integer, parameter :: maxtim = 1500
!
! Global variables
!
    integer                   :: initia  !!  if < 0: iteration process of morsys else  : equal to INITI =1 initialization =2 initialization and read restart information from the communication file =3 no initialization
    integer                   :: it01    !  Description and declaration in iidim.f90
    integer                   :: it02    !  Description and declaration in iidim.f90
    integer                   :: itb     !!  Start time of computational interval for a stand alone system the input value from FLOW: ITB = 1
    integer                   :: ite     !!  End time of computational interval for a stand alone system the input value from FLOW: ITE = -1
    integer                   :: itima   !!  Time to start simulation (N * tscale) according to DELFT3D conventions
    integer                   :: itlen   !  Description and declaration in iidim.f90
    logical      , intent(in) :: alone   !!  TRUE when flow runs stand-alone, FALSE when flow is part of morsys
    logical                   :: mainys  !!  Logical flag for FLOW is main program (TRUE) for writing output
    real(fp)                  :: tscale  !  Description and declaration in rjdim.f90
    character(*)              :: comfil  !!  Communication file name
    character(*)              :: runid   !!  Run identification code for the current simulation (used to determine the names of the in- /output files used by the system)
    character(*)              :: trifil  !!  File name for FLOW NEFIS output files (tri"h/m"-"casl""labl".dat/def)
    character(5)              :: versio  !!  Version nr. of the current package
!
! Local variables
!
    integer                                       :: icx
    integer                                       :: icy
    integer                                       :: initi         ! Control parameter 
    integer                                       :: iofset        ! Shift of inner part of matrix to remove strips
    integer                                       :: iphisc        ! Current time counter for printing history data 
    integer                                       :: iplus
    integer                                       :: istat
    integer                                       :: itcomc        ! Current time counter for the com- munication file 
    integer                                       :: itcur         ! Current time counter for the com- munication file, where starting point depend on CYCLIC 
    integer                                       :: itdroc        ! Current time counter for the dro- gue data file 
    integer                                       :: ithisc        ! Current time counter for the his- tory file 
    integer                                       :: itimc         ! Current time step counter for 2D system 
    integer                                       :: itiwec        ! Current time counter for the cali- bration of internal wave energy 
    integer                                       :: itlent        ! Lenght of the tide cycle in steps of ITP 
    integer                                       :: itmapc        ! Current time counter for the map file 
    integer                                       :: itp           ! Timestep for computation 2D system 
    integer                                       :: itrstc        ! Current time counter for the re- start file. Start writing after first interval is passed. Last time will always be written to file for ITRSTI > 0 
    integer                                       :: itwav         ! Current time counter for executation of a wave computation (online coupling with wave)
    integer                                       :: itrw          ! Time to read the wave information in case of online wave coupling
    integer                                       :: lunfil
    integer                                       :: maxmn         ! Maximum of MMAX and NMAX 
    integer                                       :: mmaxddb
    integer                            , external :: modlen
    integer                                       :: mp
    integer                            , external :: newlun
    integer                                       :: nhystp
    integer                                       :: nmaxddb
    integer                                       :: npmap         ! Current array counter for printing map data 
    integer                                       :: nst           ! Current time step counter 
    integer                                       :: nst2go        ! Number of timesteps left 
    integer                                       :: ntcur         ! Total number of timesteps on comm. file (to write to) 
    integer                                       :: ntmin
    integer                                       :: ntwav         ! Total number of timesteps on comm. file (to read from) for waves 
    integer                                       :: numtimesteps  ! total nr of timesteps 
    integer                                       :: timrst        ! Restart time in combination with restart option from comm. file 
    integer                                       :: trilen        ! Length of trifil 
    integer        , dimension(2)                 :: ifcore        ! Time indices (cell id's) of the wave functions which are in core available 
    integer        , dimension(maxtim)            :: timcur        ! Array with time steps on comm. file for restart option 
    integer        , dimension(maxtim)            :: timwav        ! Array with time steps on comm. file for wave results 
    integer                            , external :: gtcpnt
    integer                            , external :: gtipnt
    integer                            , external :: gtrpnt
    logical                                       :: commrd
    logical                                       :: cyclic        ! Flag = TRUE if cyclic system assumed 
    logical                                       :: error         ! Flag=TRUE if an error is encountered 
    logical                                       :: lrdok         ! Logical to check if reading phase has been passed. 
    logical                                       :: waverd        ! Flag = TRUE if wave process and communication file exist 
    logical                                       :: success       ! Flag = false when an error is encountered
    logical                                       :: ex            ! Help flag = TRUE when file is found 
    real(fp)                                      :: anglat        ! Angle of latitude of the model centre (used to determine the coeff. for the coriolis force) 
    real(fp)                                      :: dtmin         ! DT in minutes
    real(fp)                                      :: dtsec         ! DT in seconds 
    real(fp)                                      :: dx            ! Uniform grid-distance in the x-dir. 
    real(fp)                                      :: dy            ! Uniform grid-distance in the y-dir. 
    real(fp)                                      :: riglid        ! Rigid lid factor to reduce horizontal wet area (incompressible) 
    real(fp)                                      :: tdif
    real(fp)                                      :: vscale        ! Difference between the integer value of (dt*tunit) / tscale and it's real value (should be at most correct to machine accuarcy). 
    real(fp)                                      :: zini
    real(hp)                                      :: delta_T
    real(hp)                                      :: Tstart_Julian
    character(1)                                  :: roumet        ! Bed stress formulation specified: C : Chezy    W : White Colebrook M : Manning  Z : roughness par. 
    character(16)                                 :: simdat        ! Simulation date representing the flow condition at this date 
    character(256)                                :: filrgf        ! File name for the curvi-linear grid file (telmcrgf.xxx) !! file will be read formatted !! 
    character(256)                                :: filrol
    character(256)                                :: fixtri        ! fixed size version of trifil, needed for character concatenation 
    character(6)                                  :: soort         ! String containing to which output file version group or to diagnostic file should be written 
    character(30)  , dimension(10)                :: runtxt        ! Textual description of model input 
    character(60)                                 :: txtput        ! Text to be print
    character(300)                                :: message
    character(256)                                :: errstring
    type(olvhandle)                               :: olv_handle
!
!! executable statements -------------------------------------------------------
!
    thr                 => gdp%gdbetaro%thr
    ncmax               => gdp%d%ncmax
    nmax                => gdp%d%nmax
    mmax                => gdp%d%mmax
    nlb                 => gdp%d%nlb
    nub                 => gdp%d%nub
    mlb                 => gdp%d%mlb
    mub                 => gdp%d%mub
    ddbound             => gdp%d%ddbound
    nmaxus              => gdp%d%nmaxus
    kmax                => gdp%d%kmax
    nmaxd               => gdp%d%nmaxd
    mmaxd               => gdp%d%mmaxd
    jstart              => gdp%d%jstart
    nmmaxj              => gdp%d%nmmaxj
    nmmax               => gdp%d%nmmax
    lmax                => gdp%d%lmax
    lsal                => gdp%d%lsal
    lsts                => gdp%d%lsts
    lstsc               => gdp%d%lstsc
    lstsci              => gdp%d%lstsci
    lsed                => gdp%d%lsed
    lsedtot             => gdp%d%lsedtot
    lsecfl              => gdp%d%lsecfl
    lsec                => gdp%d%lsec
    ltur                => gdp%d%ltur
    ltur2d              => gdp%d%ltur2d
    noroco              => gdp%d%noroco
    norow               => gdp%d%norow
    nocol               => gdp%d%nocol
    nto                 => gdp%d%nto
    kc                  => gdp%d%kc
    nrob                => gdp%d%nrob
    nsrc                => gdp%d%nsrc
    nostat              => gdp%d%nostat
    ntruv               => gdp%d%ntruv
    ntru                => gdp%d%ntru
    nsluv               => gdp%d%nsluv
    itdate              => gdp%gdexttim%itdate
    tstart              => gdp%gdexttim%tstart
    tstop               => gdp%gdexttim%tstop
    dt                  => gdp%gdexttim%dt
    tunit               => gdp%gdexttim%tunit
    dm                  => gdp%gderosed%dm
    dg                  => gdp%gderosed%dg
    frac                => gdp%gderosed%frac
    cp                  => gdp%gdheat%cp
    sarea               => gdp%gdheat%sarea
    fclou               => gdp%gdheat%fclou
    lunmd               => gdp%gdinout%lunmd
    lundia              => gdp%gdinout%lundia
    lunprt              => gdp%gdinout%lunprt
    lunscr              => gdp%gdinout%lunscr
    timnow              => gdp%gdinttim%timnow
    timsec              => gdp%gdinttim%timsec
    timmin              => gdp%gdinttim%timmin
    timhr               => gdp%gdinttim%timhr
    itstrt              => gdp%gdinttim%itstrt
    itstop              => gdp%gdinttim%itstop
    itlfsm              => gdp%gdinttim%itlfsm
    itmapf              => gdp%gdinttim%itmapf
    itmapi              => gdp%gdinttim%itmapi
    itmapl              => gdp%gdinttim%itmapl
    ithisf              => gdp%gdinttim%ithisf
    ithisi              => gdp%gdinttim%ithisi
    ithisl              => gdp%gdinttim%ithisl
    itcomf              => gdp%gdinttim%itcomf
    itcomi              => gdp%gdinttim%itcomi
    itcoml              => gdp%gdinttim%itcoml
    itdrof              => gdp%gdinttim%itdrof
    itdroi              => gdp%gdinttim%itdroi
    itnflf              => gdp%gdinttim%itnflf
    itnfli              => gdp%gdinttim%itnfli
    itnfll              => gdp%gdinttim%itnfll
    itrsti              => gdp%gdinttim%itrsti
    iphisf              => gdp%gdinttim%iphisf
    iphisi              => gdp%gdinttim%iphisi
    iphisl              => gdp%gdinttim%iphisl
    ipmap               => gdp%gdinttim%ipmap
    itimtt              => gdp%gdinttim%itimtt
    ittrtu              => gdp%gdinttim%ittrtu
    itiwei              => gdp%gdinttim%itiwei
    itdiag              => gdp%gdinttim%itdiag
    julday              => gdp%gdinttim%julday
    ntstep              => gdp%gdinttim%ntstep
    tmor                => gdp%gdmorpar%tmor
    rdc                 => gdp%gdmorpar%rdc
    itmor               => gdp%gdmorpar%itmor
    morbnd              => gdp%gdmorpar%morbnd
    densin              => gdp%gdmorpar%densin
    multi               => gdp%gdmorpar%multi
    mmsyncfilnam        => gdp%gdmorpar%mmsyncfilnam
    hdt                 => gdp%gdnumeco%hdt
    momsol              => gdp%gdnumeco%momsol
    rhow                => gdp%gdphysco%rhow
    ag                  => gdp%gdphysco%ag
    iro                 => gdp%gdphysco%iro
    wind                => gdp%gdprocs%wind
    temp                => gdp%gdprocs%temp
    const               => gdp%gdprocs%const
    culvert             => gdp%gdprocs%culvert
    dredge              => gdp%gdprocs%dredge
    drogue              => gdp%gdprocs%drogue
    wave                => gdp%gdprocs%wave
    waveol              => gdp%gdprocs%waveol
    threed              => gdp%gdprocs%threed
    secflo              => gdp%gdprocs%secflo
    struct              => gdp%gdprocs%struct
    sedim               => gdp%gdprocs%sedim
    htur2d              => gdp%gdprocs%htur2d
    flmd2l              => gdp%gdprocs%flmd2l
    mudlay              => gdp%gdprocs%mudlay
    mudwave             => gdp%gdprocs%mudwave
    coupleact           => gdp%gdprocs%coupleact
    couplemod           => gdp%gdprocs%couplemod
    zmodel              => gdp%gdprocs%zmodel
    nonhyd              => gdp%gdprocs%nonhyd
    roller              => gdp%gdprocs%roller
    wavcmp              => gdp%gdprocs%wavcmp
    cnstwv              => gdp%gdprocs%cnstwv
    lftrto              => gdp%gdprocs%lftrto
    snelli              => gdp%gdprocs%snelli
    sbkol               => gdp%gdprocs%sbkol
    xbeach              => gdp%gdprocs%xbeach
    numdomains          => gdp%gdprognm%numdomains
    nummappers          => gdp%gdprognm%nummappers
    alfas               => gdp%gdr_i_ch%alfas
    ampbc               => gdp%gdr_i_ch%ampbc
    c                   => gdp%gdr_i_ch%c
    cgc                 => gdp%gdr_i_ch%cgc
    ctr                 => gdp%gdr_i_ch%ctr
    dircom              => gdp%gdr_i_ch%dircom
    dircos              => gdp%gdr_i_ch%dircos
    dirsin              => gdp%gdr_i_ch%dirsin
    dis                 => gdp%gdr_i_ch%dis
    disch               => gdp%gdr_i_ch%disch
    discom              => gdp%gdr_i_ch%discom
    dp                  => gdp%gdr_i_ch%dp
    dpc                 => gdp%gdr_i_ch%dpc
    dps                 => gdp%gdr_i_ch%dps
    dpu                 => gdp%gdr_i_ch%dpu
    dpv                 => gdp%gdr_i_ch%dpv
    ewabr0              => gdp%gdr_i_ch%ewabr0
    ewabr1              => gdp%gdr_i_ch%ewabr1
    ewave0              => gdp%gdr_i_ch%ewave0
    ewave1              => gdp%gdr_i_ch%ewave1
    fxw                 => gdp%gdr_i_ch%fxw
    fyw                 => gdp%gdr_i_ch%fyw
    grmasu              => gdp%gdr_i_ch%grmasu
    grmasv              => gdp%gdr_i_ch%grmasv
    gro                 => gdp%gdr_i_ch%gro
    gsqd                => gdp%gdr_i_ch%gsqd
    gsqs                => gdp%gdr_i_ch%gsqs
    guu                 => gdp%gdr_i_ch%guu
    guv                 => gdp%gdr_i_ch%guv
    gvu                 => gdp%gdr_i_ch%gvu
    gvv                 => gdp%gdr_i_ch%gvv
    hkru                => gdp%gdr_i_ch%hkru
    hkrv                => gdp%gdr_i_ch%hkrv
    hrmcom              => gdp%gdr_i_ch%hrmcom
    hrms                => gdp%gdr_i_ch%hrms
    hu                  => gdp%gdr_i_ch%hu
    huvw                => gdp%gdr_i_ch%huvw
    hv                  => gdp%gdr_i_ch%hv
    msucom              => gdp%gdr_i_ch%msucom
    msvcom              => gdp%gdr_i_ch%msvcom
    ombc                => gdp%gdr_i_ch%ombc
    phibc               => gdp%gdr_i_ch%phibc
    qu                  => gdp%gdr_i_ch%qu
    qxk                 => gdp%gdr_i_ch%qxk
    qxkr                => gdp%gdr_i_ch%qxkr
    qxkw                => gdp%gdr_i_ch%qxkw
    qyk                 => gdp%gdr_i_ch%qyk
    qykr                => gdp%gdr_i_ch%qykr
    qykw                => gdp%gdr_i_ch%qykw
    r0                  => gdp%gdr_i_ch%r0
    r1                  => gdp%gdr_i_ch%r1
    rbnd                => gdp%gdr_i_ch%rbnd
    rbuff               => gdp%gdr_i_ch%rbuff
    rho                 => gdp%gdr_i_ch%rho
    rint                => gdp%gdr_i_ch%rint
    rlabda              => gdp%gdr_i_ch%rlabda
    rob                 => gdp%gdr_i_ch%rob
    rtur1               => gdp%gdr_i_ch%rtur1
    s0                  => gdp%gdr_i_ch%s0
    s1                  => gdp%gdr_i_ch%s1
    sbuu                => gdp%gdr_i_ch%sbuu
    sbvv                => gdp%gdr_i_ch%sbvv
    sig                 => gdp%gdr_i_ch%sig
    ssuu                => gdp%gdr_i_ch%ssuu
    ssvv                => gdp%gdr_i_ch%ssvv
    teta                => gdp%gdr_i_ch%teta
    thetbc              => gdp%gdr_i_ch%thetbc
    thick               => gdp%gdr_i_ch%thick
    tp                  => gdp%gdr_i_ch%tp
    tpcom               => gdp%gdr_i_ch%tpcom
    tps                 => gdp%gdr_i_ch%tps
    tpscom              => gdp%gdr_i_ch%tpscom
    u1                  => gdp%gdr_i_ch%u1
    umean               => gdp%gdr_i_ch%umean
    uorb                => gdp%gdr_i_ch%uorb
    ubot                => gdp%gdr_i_ch%ubot
    ubcom               => gdp%gdr_i_ch%ubcom
    uvdist              => gdp%gdr_i_ch%uvdist
    v1                  => gdp%gdr_i_ch%v1
    vmean               => gdp%gdr_i_ch%vmean
    voldis              => gdp%gdr_i_ch%voldis
    volum1              => gdp%gdr_i_ch%volum1
    wlen                => gdp%gdr_i_ch%wlen
    wlcom               => gdp%gdr_i_ch%wlcom
    wphy                => gdp%gdr_i_ch%wphy
    ws                  => gdp%gdr_i_ch%ws
    wsu                 => gdp%gdr_i_ch%wsu
    wsucom              => gdp%gdr_i_ch%wsucom
    wsv                 => gdp%gdr_i_ch%wsv
    wsvcom              => gdp%gdr_i_ch%wsvcom
    xcor                => gdp%gdr_i_ch%xcor
    xz                  => gdp%gdr_i_ch%xz
    ycor                => gdp%gdr_i_ch%ycor
    yz                  => gdp%gdr_i_ch%yz
    zdist               => gdp%gdr_i_ch%zdist
    dzs1                => gdp%gdr_i_ch%dzs1
    res                 => gdp%gdr_i_ch%res
    rl                  => gdp%gdr_i_ch%rl
    xj                  => gdp%gdr_i_ch%xj
    guz                 => gdp%gdr_i_ch%guz
    gvz                 => gdp%gdr_i_ch%gvz
    gud                 => gdp%gdr_i_ch%gud
    gvd                 => gdp%gdr_i_ch%gvd
    gsqiu               => gdp%gdr_i_ch%gsqiu
    gsqiv               => gdp%gdr_i_ch%gsqiv
    ibuff               => gdp%gdr_i_ch%ibuff
    irocol              => gdp%gdr_i_ch%irocol
    iroll               => gdp%gdr_i_ch%iroll
    itdro               => gdp%gdr_i_ch%itdro
    kcs                 => gdp%gdr_i_ch%kcs
    kcu                 => gdp%gdr_i_ch%kcu
    kcv                 => gdp%gdr_i_ch%kcv
    kfs                 => gdp%gdr_i_ch%kfs
    kfu                 => gdp%gdr_i_ch%kfu
    kfv                 => gdp%gdr_i_ch%kfv
    kspu                => gdp%gdr_i_ch%kspu
    kspv                => gdp%gdr_i_ch%kspv
    kzs                 => gdp%gdr_i_ch%kzs
    kzu                 => gdp%gdr_i_ch%kzu
    kzv                 => gdp%gdr_i_ch%kzv
    mnbnd               => gdp%gdr_i_ch%mnbnd
    mnksrc              => gdp%gdr_i_ch%mnksrc
    kfsmin              => gdp%gdr_i_ch%kfsmin
    kfsmax              => gdp%gdr_i_ch%kfsmax
    izmodl              => gdp%gdr_i_ch%izmodl
    nambar              => gdp%gdr_i_ch%nambar
    nambnd              => gdp%gdr_i_ch%nambnd
    namcon              => gdp%gdr_i_ch%namcon
    namsrc              => gdp%gdr_i_ch%namsrc
    rtcmod              => gdp%gdrtc%rtcmod
    rtcact              => gdp%gdrtc%rtcact
    rtc_domainnr        => gdp%gdrtc%rtc_domainnr
    restid              => gdp%gdrestart%restid
    sbkConfigFile       => gdp%gdsobek%sbkConfigFile
    tstprt              => gdp%gdtricom%tstprt
    sferic              => gdp%gdtricom%sferic
    ditcof              => gdp%gdtricom%ditcof
    ditcol              => gdp%gdtricom%ditcol
    keva                => gdp%gdtricom%keva
    ktemp               => gdp%gdtricom%ktemp
    lturi               => gdp%gdtricom%lturi
    nfltyp              => gdp%gdtricom%nfltyp
    icreep              => gdp%gdtricom%icreep
    nh_level            => gdp%gdnonhyd%nh_level
    betac               => gdp%gdtricom%betac
    dml                 => gdp%gdtricom%dml
    grdang              => gdp%gdtricom%grdang
    saleqs              => gdp%gdtricom%saleqs
    temeqs              => gdp%gdtricom%temeqs
    temint              => gdp%gdtricom%temint
    evaint              => gdp%gdtricom%evaint
    forfuv              => gdp%gdtricom%forfuv
    forfww              => gdp%gdtricom%forfww
    rouflo              => gdp%gdtricom%rouflo
    rouwav              => gdp%gdtricom%rouwav
    method              => gdp%gdtricom%method
    dischy              => gdp%gdtricom%dischy
    solver              => gdp%gdtricom%solver
    disctr              => gdp%gdtricom%disctr
    tkemod              => gdp%gdtricom%tkemod
    trasol              => gdp%gdtricom%trasol
    prsmap              => gdp%gdtricom%prsmap
    selmap              => gdp%gdtricom%selmap
    prshis              => gdp%gdtricom%prshis
    selhis              => gdp%gdtricom%selhis
    tgfcmp              => gdp%gdtricom%tgfcmp
    !
    call timer_start(timer_tricomtot, gdp)
    !
    icx       = 0
    icy       = 0
    mmaxddb   = mmax + 2*gdp%d%ddbound
    nmaxddb   = nmax + 2*gdp%d%ddbound
    iofset  = 2*nmaxddb

    !
    initi     = abs(initia)
    error     = .false.
    commrd    = .true.
    !
    itstrt    = -1
    itstop    = -1
    !
    ifcore(1) = 0
    ifcore(2) = 0
    !
    maxmn     = max(mmax, nmax)
    !
    rtcact    = .false.
    lrdok     = .false.
    !
    ! Define pointers
    !
    call gtptrs(gdp)
    !
    ! Test value of ITLEN in combination with INITI
    !
    if (itlen==0 .and. initi==3) then
       call prterr(lundia    ,'D001'    ,' '       )
       error = .true.
       goto 9996
    endif
    !
    ! Put header on the screen
    !
    if (initi==1 .or. initi==2) then
       txtput = 'Part IV   - Reading complete MD-file...'
       if (.not.parll .or. (parll .and. inode == master)) then
          write (lunscr, '(a)') txtput
       endif
       !
       ! Read  md-file, only if initialisation is wanted
       !
       call readmd(lunmd     ,lundia    ,lunscr    ,error     ,runid     ,runtxt    , &
                 & filrgf    ,dx        ,dy        ,sferic    , &
                 & anglat    ,grdang    ,tgfcmp    ,roumet    ,rouwav    , &
                 & temeqs    ,saleqs    ,betac     ,dml       , &
                 & restid    ,icreep    ,trasol    , &
                 & forfuv    ,forfww    ,ktemp     ,keva      , &
                 & temint    ,evaint    , &
                 & lturi     ,tkemod    ,riglid    , &
                 & tstprt    ,prsmap    ,prshis    ,selmap    , &
                 & selhis    ,filrol    ,gdp       )
       call dfsync (gdp)
       if (error) goto 9996
       !
       ! Initialize 2-Layer Fluid Mud communication
       !
       if (flmd2l) then
          call timer_start(timer_wait, gdp)
          call syncom_init(mudlay, densin, mlb, mub, nlb, nub)
          call timer_stop(timer_wait, gdp)
       endif
       !
       ! Initialize Couple-communication
       !
       if (couplemod) then
          call timer_start(timer_wait, gdp)
          write(*,*) 'Initializing communication with COUPLE ...'
          call syncflowcouple_init(error, gdp%runid       , gdp%gdcoup%flowtocouple, &
                                 &       gdp%gdcoup%pars , gdp%gdcoup%locs        )
          write(*,*) '... continuing'
          call timer_stop(timer_wait, gdp)
          if (error) then
             coupleact = .false.
             call prterr(lundia    ,'J020'    ,'SyncFlowCouple_Init')
             goto 9996
          else
             coupleact = .true.
          endif
       endif
       !
       ! Define time differences for writing output files
       ! for MAP print the times should be defined ( >= 0)
       !
       ! No resetting of MAP and HIS times
       !
       ditcof = itstop - itcomf
       ditcol = itstop - itcoml
       !
       ! Initialize numerical parameters
       !
       call inimet(lundia    ,error     ,versio    ,wave      ,trasol    , &
                 & momsol    ,method    ,dischy    ,solver    ,icreep    , &
                 & disctr    ,gdp       )
       if (error) goto 9996
    endif
    !
    ! Check conflicting settings
    !
    call chkset(lundia    ,error     ,sferic    ,method    ,trasol    , &
              & dischy    ,solver    ,disctr    ,ktemp     , &
              & keva      ,iphisi    ,gdp       )
    if (error) goto 9996
    !
    ! Test Comm. file version for consistency
    !
    soort = 'com'
    call chkcom(lundia    ,error     ,comfil    ,soort     ,gdp       )
    if (error) goto 9996
    !
    ! Read TSCALE and ITLEN from comm. file for ITLEN = 0,
    ! writing will be done in WRPARM (TRICOM-INIPPR-WRCOMI-WRPARM)
    ! for ITCOMI > 0
    !
    if (itlen==0) then
       call rdtimc(comfil    ,lundia    ,error     ,commrd    ,itlen     , &
                 & tscale    ,gdp       )
       if (error) goto 9996
       !
       ! Test value COMMRD in combination with INITI
       !
       if (.not.commrd) then
          tscale = dt*tunit
          if (.not.waveol) then
             itlen  = itstop + 1
          endif
       else
          !
          ! setting itlen to 0 here:
          ! - makes a mess of time management in tricom and the use of periodic input
          ! - enables the use of offline wave results from the com-file
          !
          itlen = 0
       endif
       itb   = 1
       ite   = -1
       itima = itstrt
    endif
    !
    ! Test if DT*TUNIT is an integer multiple of TSCALE as defined,
    ! relative test
    !
    vscale = dt*tunit - int((dt*tunit)/tscale + 1.E-6)*tscale
    if (abs(vscale) > tscale*1.0e-6) then
       call prterr(lundia    ,'D002'    ,' '       )
       if (commrd) then
          write(lundia,'(3a)') '          Tscale is read from files ''com-', trim(runid), '.def'' and ''.dat''. Remove them if not intended.'
       endif
       error = .true.
       goto 9996
    endif
    !
    ! Initialize time parameters for stand alone (and 2D/3D system)
    ! stand alone and no initialisation not permitted !!
    !
    itp = nint(dt*tunit/tscale)
    if (ite < itb) then
       if (initi == 3) then
          call prterr(lundia    ,'D003'    ,' '       )
          error = .true.
          goto 9996
       endif
       it01 = itdate
       it02 = 0
       itb  = itstrt*itp
       ite  = itstop*itp
       if (itb > itlen .and. itlen /= 0) then
          call prterr(lundia    ,'D004'    ,' '       )
          error = .true.
          goto 9996
       endif
    endif
    !
    ! Initialize time parameters regarding to 2D/3D system parameters
    ! the time frame can be set N cycli back if ITLEN < ITSTOP
    !
    itlent = itlen/itp
    itstrt = modlen(itb, itlen)/itp
    itstop = itstrt + (ite - itb)/itp
    !
    ! Initialize itrw on a not used value
    ! itrw will get a relevant value when the wave executable has run
    !
    itrw   = itstop + 1
    !
    ! Calculate Julian start day for use in TRISOL
    !
    call juldat(itdate    ,julday    )
    !
    ! Define start time and timesteps (in seconds)
    !
    dtsec = dt * tunit
    dtmin = dtsec / 60.0
    hdt   = 0.5 * dtsec
    !
    timnow = real(itstrt,fp)
    call setcurrentdatetime(timnow, gdp)
    !
    ! According to this new time frame the related times must be redefined
    ! No resetting of MAP and HIS
    !
    itcomf = itstop - ditcof
    itcoml = itstop - ditcol
    !
    ! If ITLENT =< ITSTOP the simulation is cyclic.
    ! NOTE: notify that the time-dependent input is checked and the
    !       corresponding files are created according to the timeframe
    !       of the MD-file. If the input is cyclic this will not give
    !       problems (assuming that the data starts before the beginning
    !       of the first cycle (t=0), and will be large enough to
    !       cover the total simulation period).
    !
    cyclic = (itstop>=itlent) .and. (itlent /= 0)
    if (itcomi == 0) then
       if (cyclic) then
          call prterr(lundia    ,'D005'    ,' '       )
       endif
    endif
    if (.not.cyclic) then
       if (it01 /= itdate) then
          call prterr(lundia    ,'D006'    ,' '       )
          error = .true.
          goto 9996
       endif
    endif
    !
    ! Initialisation and Checking
    !
    if (initi==1 .or. initi==2) then
       !
       ! Put header on the screen
       !
       txtput = 'Part V    - Initialisation & checking input...'
       if (.not.parll .or. (parll .and. inode == master)) then
          write (lunscr, '(a)') txtput
       endif
       !
       ! calculate timestep number for starting morphological changes
       ! Need to have delay from simulation start so that can have a
       ! spin-up time each time simulation starts if have multiple
       ! continuing simulations
       !
       if (sedim) then
          tdif  = tmor + itstrt*dt
          itmor = nint(tdif/dt)
          if (abs(itmor*dt-tdif) > (0.1*dt)) then
             error  = .true.
             txtput = 'Morphological calculation start time'
             call prterr(lundia, 'U044', txtput)
          endif
          write(txtput,'(a,i0)') 'Morphological Changes Start Time (step) : ',itmor
          call prterr(lundia, 'G051', txtput)
       endif
       !
       ! Initialize input arrays and verify input
       ! INCHKI is placed between semaphores; in case of a fluidmud
       ! calculation and when both water and mud fraction use the same
       ! grid-file, they may try to read the grid-file at the same time.
       ! This was first noticed on an XP-platform. The use of semaphores
       ! reduces the collapse chance, but does not guarantee that it
       ! will not happen.
       !
       call inchki(lundia    ,error     ,runid     ,sferic    ,filrgf    , &
                 & dx        ,dy        ,anglat    ,grdang    , &
                 & tgfcmp    ,riglid    , &
                 & temeqs    ,saleqs    ,ktemp     ,fclou     , &
                 & sarea     ,roumet    ,rouflo    ,restid    , &
                 & lturi     ,gdp       )
       if (error) goto 9996
       !
       ! Set some single variables in the memory
       ! comform how they are declared in the initial part of the
       ! communication file
       !
       i(gtipnt('NMAX', gdp))        = nmax
       i(gtipnt('NMAXUS', gdp))      = nmaxus
       i(gtipnt('MMAX', gdp))        = mmax
       i(gtipnt('NOROW', gdp))       = norow
       i(gtipnt('NOCOL', gdp))       = nocol
       i(gtipnt('NOROCO', gdp))      = noroco
       i(gtipnt('NTO', gdp))         = nto
       i(gtipnt('NROB', gdp))        = nrob
       i(gtipnt('KMAX', gdp))        = kmax
       i(gtipnt('NSRC', gdp))        = nsrc
       i(gtipnt('ITLEN', gdp))       = itlen
       i(gtipnt('IT01', gdp))        = it01
       i(gtipnt('IT02', gdp))        = it02
       r(gtrpnt('AG', gdp))          = ag
       r(gtrpnt('RHOW', gdp))        = rhow
       r(gtrpnt('DT', gdp))          = dt
       r(gtrpnt('TSCALE', gdp))      = tscale
       ch(gtcpnt('ROUFLO', gdp))     = rouflo(1:1)
       ch(gtcpnt('ROUFLO', gdp) + 1) = rouflo(2:2)
       ch(gtcpnt('ROUFLO', gdp) + 2) = rouflo(3:3)
       ch(gtcpnt('ROUFLO', gdp) + 3) = rouflo(4:4)
       !
       ! User defined functions (reading and checking)
       !
       call usrdef(lundia    ,error     ,grdang    ,secflo    ,gdp       )
       if (error) goto 9996
       !
       ! Balance output?
       !
       call rdmassbal(r(xz)     ,r(yz)     ,i(kcs)    ,r(gsqs)   , &
                    & mmax      ,nmax      ,nmaxus    ,nmmax     , &
                    & gdp       )
       !
       ! Read the file with wave components
       !
       if (roller) then
          if (wavcmp) then
             call rbsig(ncmax     ,r(ampbc)  ,r(ombc)   ,r(phibc)  ,r(thetbc) , &
                      & filrol    ,lundia    ,gdp       )
          endif
       endif
       if (dredge) then
          !
          ! Read dredge input and initialize related data
          !
          call rddredge(r(xcor)   ,r(ycor)   ,r(xz)     ,r(yz)     ,r(gsqs)   , &
                      & mmax      ,nmax      ,nmaxus    ,nmmax     ,lsedtot   , &
                      & gdp       )
       endif
       if (multi) then
          !
          ! Initialise parallel online mor run for multiple conditions
          !
          call initmerge(nmmax, lsedtot, runid, gdp)
       endif
       !
       ! Read culvert input and initialize related data
       !
       if (culvert) then
          call rdcul(nsrc, ch(namsrc), i(mnksrc) ,r(voldis), gdp)       
       endif
    endif
    !
    ! Re-define Diagnostic mode start time for INITI = 3
    !
    if (initi == 3) itdiag = itstop + 1
    !
    ! Put header on the screen
    !
    txtput = 'Part VI   - Initialisation & checking second part...'
    if (.not.parll .or. (parll .and. inode == master)) then
       write (lunscr, '(a)') txtput
    endif
    !
    ! Read time array in case of wave interaction only if waverd
    ! Initial value of WAVERD := WAVE .and. COMMRD
    ! If an error occurred while reading the communication file then
    ! WAVERD will be set .false.
    !
    waverd = wave .and. commrd
    if (waverd) then
       call rdtimw(comfil    ,lundia    ,error     ,ntwav     ,timwav    , &
                 & maxtim    ,waverd    ,nmaxus    ,mmax      ,gdp       )
       if (error) goto 9996
    endif
    !
    ! Wave is true and not able to read wave information from comm-file:
    ! - Stop when running stand alone
    ! - Continue when running as part of MOR
    ! - Continue when online coupling with waves is applied
    !
    if (wave .and. .not.waverd) then
       if (alone .and. .not. (cnstwv .or. snelli) .and. .not. waveol .and. .not. xbeach) then
          error = .true.
          call prterr(lundia    ,'D007'    ,' '       )
          goto 9996
       else
          ! Warning about missing COM-file removed.
       endif
    endif
    ! Read restart information from the communication file
    ! NOTE: In the past the test was on INITT=2 and gave wrong results
    !       for INITI=3 In case INITI=3 and CYCLIC=.false. the result
    !       should be tested for a) read from comm. file or b) not read
    !
    if (initi /= 1) then
       timrst = modlen(itstrt*itp, itlen)
       !
       ! Read the time array and corresponding water-level and
       ! velocities, rbuff is used as work array and set smoothing
       ! time to 0.
       !
       itlfsm = 0
       if (initia > 0) then
          call rstcom(comfil    ,lundia    ,error     ,mmax      ,nmax      , &
                    & kmax      ,nmaxus    ,lstsci    ,lsecfl    ,lsec      , &
                    & timrst    ,itlen     ,timcur    ,maxtim    ,r(s1)     , &
                    & r(u1)     ,r(v1)     ,r(r1)     ,r(rbuff)  ,gdp       )
          if (error) goto 9996
       endif
    endif
    !
    ! Check timeframe before reading/writing to comm. files.
    ! This check should be executed for all possible initial values
    !
    call chktim(lundia    ,nostat    ,ntruv     ,itstrt    ,itstop    , &
              & prsmap    ,prshis    ,selmap    ,selhis    ,ipmap     , &
              & mxprt     ,itmapf    ,itmapl    ,itmapi    ,iphisf    , &
              & iphisl    ,iphisi    ,ithisf    ,ithisl    ,ithisi    , &
              & itcomf    ,itcoml    ,itcomi    ,itrsti    ,itnflf    , &
              & itnfli    ,itnfll    ,error     ,gdp       )
    if (error) goto 9996
    !
    ! Write DP to comm. file for INITI=1 and ITCOMI > 0
    ! or read  DP from comm. file for INITI=2 or 3, a comm. file is
    ! presumed and if not this will generate an error message
    ! Subroutines RWBOTC, CALDPS and CALDPU should not be called when
    ! initi=3 and lsed>0: MOR has calculated dp as the averages of
    ! dpu/dpv.
    !
    if (initi/=3 .or. lsed==0) then
       !
       ! Calculate DPS depending on DPSOPT
       ! NFLTYP must be set in CALDPS because it is being used by
       ! MOR-transport
       !
       icx = nmaxddb
       icy = 1
       call caldps(nmmax     ,nfltyp    ,icx       , &
                 & icy       ,i(kcs)    ,r(dp)     ,d(dps)    ,gdp       )
       if (waveol) then
          !
          ! In case of wave online: write DPS to comm-file instead of DP
          !
          if (prec == hp) then
             call rwbotc_double(comfil    ,lundia    ,error     ,initi     ,itima     , &
                              & itcomi    ,mmax      ,nmax      ,nmaxus    ,d(dps)    , &
                              & r(rbuff)  ,ite       ,gdp       )
          else
             call rwbotc(comfil    ,lundia    ,error     ,initi     ,itima     , &
                       & itcomi    ,mmax      ,nmax      ,nmaxus    ,d(dps)    , &
                       & r(rbuff)  ,ite       ,gdp       )
          endif
       else
          call rwbotc(comfil    ,lundia    ,error     ,initi     ,itima     , &
                    & itcomi    ,mmax      ,nmax      ,nmaxus    ,r(dp)     , &
                    & r(rbuff)  ,ite       ,gdp       )
       endif
       if (error) goto 9996
       !
       ! DD code added:
       ! This code must be performed before inchkr and caldpu is called
       !
       i(gtipnt('lstsci', gdp)) = lstsci
       i(gtipnt('ltur'  , gdp)) = ltur
       i(gtipnt('lsed'  , gdp)) = lsed
       i(gtipnt('lsedtt', gdp)) = lsedtot
       r(gtrpnt('hdt'   , gdp)) = hdt
       i(gtipnt('ddb'   , gdp)) = gdp%d%ddbound
       i(gtipnt('mmaxdb', gdp)) = mmaxddb
       i(gtipnt('nmaxdb', gdp)) = nmaxddb
       !
       if (zmodel) then
          izmodl = 1
       else
          izmodl = 0
       endif
       i(gtipnt('izmodl', gdp)) = izmodl
       !
       if (roller) then
          iroll = 1
       else
          iroll = 0
       endif
       i(gtipnt('iroll', gdp)) = iroll
    endif
    !
    ! Synchronisation point 1
    ! =======================
    ! - close semaphore
    ! - DD synchronisation via nxtstp
    ! - put new semaphore
    ! - safe jump to the next synchronisation point (9997) in case of error, inside the new semaphore
    !
 9996 continue
    !
    ! Related pseminit is in trisim.f90
    !
    call vseminit
    !
    ! The call to nxtstp:
    ! - synchronises all subdomains up to this point
    ! - signals the mappers that they can initialize themselve
    ! - continues execution of the subdomains after all mappers are initialized
    !
    call timer_start(timer_d3dflowinit, gdp)
    nhystp = nxtstp(d3dflow_init, gdp)
    call timer_stop(timer_d3dflowinit, gdp)
    !
    ! related vseminit is in tricom.f90, at label 9997
    !
    call pseminit
    !
    if (error) goto 9997
    !
    ! End of synchronisation point 1
    ! ==============================
    !
    if (initi/=3 .or. lsed==0) then
       !
       ! Calculate DPU/DPV (depth at velocity points)
       !
       call caldpu(lundia    ,mmax      ,nmaxus    ,kmax      , &
                 & zmodel    , &
                 & i(kcs)    ,i(kcu)    ,i(kcv)    , &
                 & i(kspu)   ,i(kspv)   ,r(hkru)   ,r(hkrv)   , &
                 & r(umean)  ,r(vmean)  ,r(dp)     ,r(dpu)    ,r(dpv)    , &
                 & d(dps)    ,r(dzs1)   ,r(u1)     ,r(v1)     ,r(s1)     , &
                 & r(thick)  ,gdp       )
    endif
    !
    ! After Flowmapper has adjusted the KC[UVS] arrays, recalculate some
    ! geometric information in D3dFlow in vicinity of interfaces
    ! inigeo_dd must be called before xzyz_dd
    !
    if (.not.parll) then
       call inigeo_dd(lundia    ,mmax      ,nmax      ,nmaxus    ,r(guu)    , &
                    & r(gvv)    ,r(guv)    ,r(gvu)    ,r(gsqs)   ,r(gsqd)   , &
                    & r(guz)    ,r(gvz)    ,r(gud)    ,r(gvd)    ,r(gsqiu)  , &
                    & r(gsqiv)  ,i(kfu)    ,i(kfv)    ,i(kfs)    ,i(kcu)    , &
                    & i(kcv)    ,i(kcs)    ,gdp       )
    endif
    !
    ! Compute coordinates of water level points at internal subdomain boundaries
    ! Needed for WaveOL and coarse wind grid
    !
    call xzyz_dd (r(xz)  ,r(yz)  ,i(kcs) ,nmax  ,mmax  ,&
               & nmaxus ,r(xcor),r(ycor),gdp          )
    !
    ! xz and yz are needed by the meteo module
    !
    success   = gridtometeo(gdp%runid, nmaxus   , mmax     , &
                          & gdp%d%nlb, gdp%d%nub, gdp%d%mlb, gdp%d%mub, &
                          & i(kcs)   , r(xz)    , r(yz)    )
    call checkmeteoresult(success, gdp)
    !
    ! Set single variable NFLTYP in the memory (initialized in CALDPS)
    ! comform how they are declared in the initial part of the
    ! communication file
    !
    i(gtipnt('NFLTYP', gdp)) = nfltyp
    !
    ! Read and interpolate the wave information
    ! some array names are different in wave group on comm. file
    ! DIR := teta  , DISS := dis   , FX := wsu, FY := wsv,
    ! MX  := grmasu, MY   := grmasv
    !
    itimc = modlen(itstrt*itp, itlen)
    if (waverd) then
       call setwav(comfil    ,lundia    ,error     ,mmax      ,nmax      , &
                 & nmaxus    ,itimc     ,ntwav     ,itlen     ,timwav    , &
                 & norow     ,noroco    ,i(irocol) ,ifcore    ,d(dps)    , &
                 & r(s1)     ,r(uorb)   ,r(tp)     ,r(teta)   ,r(dis)    , &
                 & r(wsu)    ,r(wsv)    ,r(grmasu) ,r(grmasv) ,r(hrms)   , &
                 & r(tps)    ,r(ubot)   ,r(wlen)   ,r(hrmcom) ,r(tpcom)  , &
                 & r(dircom) ,r(discom) ,r(wsucom) ,r(wsvcom) ,r(msucom) , &
                 & r(msvcom) ,r(tpscom) ,r(ubcom)  ,r(wlcom)  ,r(rlabda) , &
                 & r(dircos) ,r(dirsin) ,r(ewave1) ,roller    ,wavcmp    , &
                 & r(ewabr1) ,gdp       )
       if (error) goto 9997
    endif
    !
    ! DD code added end
    !
    ! Initialize arithmetic arrays, depending on initial input as well
    ! as on previous defined input (read from comm. file or in arrays)
    ! inchkr should not be called when initi == 3 (warm restart in MOR)
    !
    if (initi==1 .or. initi==2) then
       call inchkr(lundia    ,error     ,runid     ,timhr     ,dischy    , &
                 & cyclic    ,sferic    ,grdang    ,temeqs    ,saleqs    , &
                 & lturi     ,rouflo    ,rouwav    ,ktemp     ,temint    , &
                 & evaint    ,initia    ,gdp       )
       if (error) goto 9997
    endif
    !
    ! Initialize Delft3D-Sobek after coordinates etc have been set 
    !
    if (sbkol) then
       numTimeSteps  = itstop-itstrt 
       delta_T       = dt*60.0_hp
       Tstart_Julian = real(julday,hp) + real(tstart,hp)/1440.0_hp - 0.5_hp
       write (message, '(3a)') 'Reading Sobek-Online configuration file "', trim(sbkConfigFile), '"'
       call prterr(lundia, 'G051', trim(message))
       call timer_start(timer_wait, gdp)
       call D3S_setup('D3D-FLOW'    ,    runid   , lundia        , sbkConfigFile , &
                      gdp%d%mlb     , gdp%d%mub  , gdp%d%nlb     , gdp%d%nub     , &
                      r(xcor)       , r(ycor)    , nto           , ch(nambnd)    , i(mnbnd)    , &
                      nostat        , gdp%gdstations%namst       , gdp%gdstations%mnstat       , gdp%gdbcdat%ext_bnd, &
                      Tstart_Julian , delta_T    , numTimeSteps  , errstring     , success       )
       call timer_stop(timer_wait, gdp)
       !
       if (.not. success) then
          write(message,'(2a)') '*** error D3S_Init: ', trim(errstring)
          call prterr(lundia, 'P004', trim(message))
          call d3stop(1, gdp)
       endif
       !
       call timer_start(timer_wait, gdp)
       write(*,*) 'Initialisation: D3S_put_levels'
       call D3S_put_levels(0        , &
                           gdp%d%mlb, gdp%d%mub, &
                           gdp%d%nlb, gdp%d%nub, &
                           r(s1)    , i(kfs)     )
       write(*,*) 'Initialisation: D3S_put_levels DONE'
       call timer_stop(timer_wait, gdp)
    endif
    !
    ! Initialize RTC-communication
    !
    call rtc_comm_init(error     ,ch(nambar),i(kfs)    ,i(kfsmin) , &
                     & i(kfsmax) ,r(sig)    ,r(sig)    ,r(s1)     , &
                     & d(dps)    ,r(r0)     ,gdp)
    if (error) goto 9997
    !
    ! Initial reading phase has been passed
    !
    lrdok = .true.
    !
    ! Put header on the screen
    !
    txtput = 'Part VII  - Initialisation output...'
    if (.not.parll .or. (parll .and. inode == master)) then
       write (lunscr, '(a)') txtput
    endif
    !
    ! CMT will create dummy files for all output. Delete FLOW Nefis
    ! files first for INITI=1 and file requested (times <> 0)
    !
    if (initi == 1) then
       trilen = min(len(fixtri), len(trifil))
       fixtri(1:trilen) = trifil(1:trilen)
       if (ithisi /= 0) then
          call delnef(fixtri(1:3) // 'h' // fixtri(5:trilen)     ,gdp       )
       endif
       if (itmapi /= 0) then
          call delnef(fixtri(1:3) // 'm' // fixtri(5:trilen)     ,gdp       )
       endif
       if (drogue) then
          call delnef(fixtri(1:3) // 'd' // fixtri(5:trilen)     ,gdp       )
       endif
       !
       ! The following files may be generated for "debug" purpose
       ! Remove them if they are already there from a previous run
       !
       call rmdel('tstprt.'//trim(gdp%runid), gdp) 
       call rmdel('discharge.'//trim(gdp%runid), gdp) 
       call rmdel('averagetemp.'//trim(gdp%runid), gdp) 
       call rmdel('pdfs.'//trim(gdp%runid), gdp) 
    endif
    !
    ! Initialise trachytope roughness time parameter for periodic calculations
    !
    ittrtu = itstrt + itimtt
    !
    ! Initialise FILE time parameters and test values
    !
    npmap = 1
    do mp = 2, mxprt
       if (ipmap(mp) == 0) then
          exit
       endif
       if (ipmap(mp) <= itstrt) npmap = mp
    enddo
    iphisc = iphisf
    !
    if (itmapi == 0) then
       itmapc = -1
    elseif (itmapf < itstrt) then
       iplus = ((itstrt - itmapf)/itmapi)*itmapi
       if (iplus < (itstrt-itmapf)) iplus = iplus + itmapi
    else
       iplus = 0
    endif
    itmapc = min(itmapf + iplus, itmapl)
    !
    if (ithisi == 0) then
       ithisc = -1
    elseif (ithisf < itstrt) then
       iplus = ((itstrt - ithisf)/ithisi)*ithisi
       if (iplus < (itstrt-ithisf)) iplus = iplus + ithisi
    else
       iplus = 0
    endif
    ithisc = min(ithisf + iplus, ithisl)
    !
    if (ithisi == 0) ithisc = -1
    itdroc = itdrof
    if (itdroi == 0) itdroc = -1
    itrstc = min(itstrt + itrsti, itstop)
    if (itrsti == 0) itrstc = -1
    itiwec = itstrt
    if (itiwei == 0) itiwec = -1
    !
    itcomc = itcomf
    if (itcomi > 0) then
       if (cyclic) then
          ntcur = (itlent - 1)/itcomi + 1
          ! Check if all time points on com-file will be filled
          ! Note: error message has to be tidied up.
          ntmin = (itcoml - itcomf)/itcomi + 1
          if (ntmin < ntcur) then
             write (lundia, '(a,a)') '*** error',                              &
                              & 'not all time points on com-file will be filled '
             error = .true.
             goto 9997
          endif
          itcur = itcomc/itcomi + 1
          if (itcur >  ntcur) itcur = mod(itcur, ntcur)
          if (itcur == 0    ) itcur = ntcur
       else
          ntcur = (itcoml - itcomf)/itcomi + 1
          itcur = 1
       endif
    endif
    !
    ! Write initial input to output files after checking output file times
    !
    call inippr(lundia    ,error     ,trifil    ,comfil    ,mainys    , &
              & initi     ,selhis    ,selmap    ,tscale    , &
              & itlen     ,itcur     ,itimc     , &
              & it01      ,it02      ,sferic    ,grdang    , &
              & rouflo    ,nfltyp    , &
              & runtxt    ,gdp       )
    if (error) goto 9997
    !
    ! Re-define default values in depth array for online visualisation
    ! Hence all 999.999 will be set to 0.0
    !
    call dp999(r(dp)     ,nmax      ,mmax      ,gdp       )
    !
    ! DD code added:
    !
    ! The new geometric information influences the check on dry points
    ! A check is necessary at couple boundaries
    !
    icx = nmaxddb
    icy = 1
    call chkdry_dd(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                 & icy       ,i(kcu)    ,i(kcv)    ,i(kcs)    ,i(kfu)    , &
                 & i(kfv)    ,i(kfs)    ,r(hu)     ,r(hv)     ,r(guu)    , &
                 & r(gvv)    ,r(thick)  ,r(u1)     ,r(v1)     ,r(qxk)    , &
                 & r(qyk)    ,gdp       )
    call chkrefinement(gdp)
    !
    ! DD code added end
    !
    if (wave .and. waveol) then
       !
       ! Onlinecoupling with waves
       ! Initialise communication with Waves
       ! Waves needs numdomains, not nummappers
       !
       call timer_start(timer_wait, gdp)
       success = flow_to_wave_init(runid     , it01   , tscale       , &
                                 & numdomains, mudlay )
       call timer_stop(timer_wait, gdp)
       if (.not. success) then
          txtput = 'Initialization of DelftIO communication with waves failed'
          write(lunscr,'(a)') trim(txtput)
          call prterr(lundia, 'P004', trim(txtput))
          call d3stop(1, gdp)
       endif
    endif
    if (mudwave) then
       !
       ! Onlinecoupling between mud and waves
       ! Initialise wave pointering
       !
       call timer_start(timer_wait, gdp)
       success = flow_to_wave_init(runid     , it01   , tscale, &
                                 & numdomains, mudlay )
       call timer_stop(timer_wait, gdp)
       if (.not. success) then
          txtput = 'Initialization of DelftIO communication with waves failed'
          write(lunscr,'(a)') trim(txtput)
          call prterr(lundia, 'P004', trim(txtput))
          call d3stop(1, gdp)
       endif
    endif
    !
    if ((lsedtot>0) .and. (.not.flmd2l)) then
       call morbndfill(i(kcs)    ,r(guu)    ,r(gvv)    ,icx       , &
                     & icy       ,i(mnbnd)  ,nto       ,gdp       )
    endif
    !
    ! Write conversion from "n and m" to "nm" and vice versa to diag file
    !
    if (tstprt) then
       call nm_to_diag(gdp)
    endif
    !
    ! Make D3D data available to online applications
    !
    olv_handle = new_olv()
    call publishGDP(olv_handle, gdp, runid, zmodel)
    !
    ! Not multi threaded
    !
    call publishUtils(olv_handle)
    call setEndTimeStep(olv_handle, itstop)
    
    !
    ! Synchronisation point 2
    ! =======================
    ! - close semaphore
    ! - DD synchronisation via initfinished
    ! - safe jump to the next synchronisation point (9998) in case of error, outside any semaphore
    !
 9997 continue
    !
    ! related pseminit is in tricom.f90 at label 9996
    !
    call vseminit
    !
    ! The call to initfinished synchronises all subdomains up to this point
    ! (necessary in case of multiple domains and wave online)
    !
    call timer_start(timer_d3dflowinit, gdp)
    call initfinished(numdomains)
    call timer_stop(timer_d3dflowinit, gdp)
    !
    if (error) goto 9998
    !
    ! End of synchronisation point 2
    ! ==============================
    !
    ! Put header on the screen
    !
    txtput = 'Part VIII - Start Simulation...'
    if (.not.parll .or. (parll .and. inode == master)) then
       call psemnefis
       write (lunscr, '(a)') txtput
       write (lunscr, *)
       call vsemnefis
    endif
    !
    call timer_stop(timer_init, gdp)
    call timer_start(timer_simulation, gdp)
    !
    ! Simulation time loop
    !
    call setRunningFlag( olv_handle, 0, itstrt)    !status is: started
    do nst = itstrt, itstop - 1, 1
       call timer_start(timer_timeintegr, gdp)
       !
       nst2go = itstop - nst
       ntstep = ntstep + 1
       !
       call timer_start(timer_step2screen, gdp)
       call psemnefis
       if (.not.parll .or. (parll .and. inode == master)) then
          call step_to_screen(nst2go, itstrt, itstop, nst, gdp)
       endif
       call vsemnefis
       call timer_stop(timer_step2screen, gdp)
       call D3DOL_Timestep (nst)
       !
       ! Status is: simulation is running / iteration
       !
       call setRunningFlag(olv_handle, 1, nst)
       !
       ! Set timsec and current date and time.
       !
       itimc  = modlen(nst*itp, itlen)
       itwav  = itcomc
       !
       ! Mormerge synchronisation
       !
       if (multi) then
          if (mod(nst2go,5) == 0) then
             call timer_start(timer_tricom_rest, gdp)
             lunfil = newlun(gdp)
             open (lunfil, file=mmsyncfilnam, position='append', action='write', iostat=istat)
             if (istat /= 0) then
                write(*,*)' *** WARNING: unable to write in file ',trim(mmsyncfilnam)
             else
                write(lunfil,'(i0)') nst2go
                close(lunfil)
             endif
             call timer_stop(timer_tricom_rest, gdp)
          endif
       endif
       !
       ! Calculate post processing info and write to files if required.
       !
       call updmassbal(.true.   ,r(qxk)    ,r(qyk)    ,i(kcs)    ,r(r1)     , &
                     & r(volum1),r(sbuu)   ,r(sbvv)   ,r(ssuu)   ,r(ssvv)   , &
                     & r(gsqs)  ,r(guu)    ,r(gvv)    ,d(dps)    ,gdp       )
       call psemnefis
       call timer_start(timer_postpr, gdp)
       call postpr(lundia    ,lunprt    ,error     ,versio    ,comfil    , &
                 & trifil    ,mainys    ,runid     ,prsmap    ,prshis    , &
                 & selmap    ,selhis    ,rhow      ,grdang    , &
                 & initi     ,dtsec     , &
                 & nst       ,iphisc    ,npmap     ,itcomc    ,itimc     , &
                 & itcur     ,ntcur     ,ithisc    ,itmapc    ,itdroc    , &
                 & itrstc    ,ktemp     ,gdp       )
       call timer_stop(timer_postpr, gdp)
       call vsemnefis
       if (error) goto 9998
       call psemnefis
       if (wave .and. xbeach) then
          ! call xbeach_in_delft3d(r(xz+iofset),r(yz+iofset), &
          !    &              r(dps+iofset),r(s1+iofset),r(u1+iofset),r(v1+iofset),     &
          !                   r(uorb+iofset),r(tp+iofset),r(teta+iofset),r(dis+iofset), &
          !                   r(wsu+iofset),r(wsv+iofset),r(fxw+iofset),r(fyw+iofset),    &
          !                   r(grmasu+iofset),r(grmasv+iofset),                        &
          !                   r(hrms+iofset),r(rlabda+iofset),                            &
          !                    mmax,nmax,nmaxus,2*timnow*hdt)
       endif
       if (wave .and. waveol .and. (.not.xbeach)) then
          !
          ! Command to wave module to execute wave computation on
          ! times for writing to com file
          !
          if (nst == itwav) then
             call timer_start(timer_tricom_rest, gdp)
             if (prec == hp) then
                call rwbotc_double(comfil    ,lundia    ,error     ,initi     ,nst     , &
                                 & itcomi    ,mmax      ,nmax      ,nmaxus    ,d(dps)  , &
                                 & r(rbuff)  ,nst       ,gdp       )
             else
                call rwbotc(comfil    ,lundia    ,error     ,initi     ,nst     , &
                          & itcomi    ,mmax      ,nmax      ,nmaxus    ,d(dps)  , &
                          & r(rbuff)  ,nst       ,gdp       )
             endif
             call timer_stop(timer_tricom_rest, gdp)
             !
             ! Wave calculation is only executed after updating the comm-files
             ! of all sub domains.
             ! That's why flow_to_wave_command needs numdomains.
             !
             call timer_start(timer_wait, gdp)
             success = flow_to_wave_command(flow_wave_comm_perform_step, &
                                          & numdomains, mudlay, nst)
             call timer_stop(timer_wait, gdp)
             if (.not. success) then
                txtput = 'Delftio command to waves failed'
                write(lunscr,'(a)') trim(txtput)
                call prterr(lundia    ,'P004'    ,trim(txtput)    )
                call d3stop(1, gdp)
             endif
             itrw = nst + 1
          endif
          if (nst == itrw) then
             call rdtimw(comfil    ,lundia    ,error     ,ntwav     ,timwav    , &
                       & maxtim    ,waverd    ,nmaxus    ,mmax      ,gdp       )
             waverd = .true.
             ifcore(1) = 0
             ifcore(2) = 0
          else
             waverd = .false.
          endif
       endif
       if (mudwave) then
          !
          ! Command to wave module to execute wave computation on
          ! times for writing to com file
          !
          if (nst == itwav) then
             !
             ! Wave calculation is only executed after updating the comm-files
             ! of all sub domains.
             ! That's why flow_to_wave_command needs numdomains.
             !
             call timer_start(timer_wait, gdp)
             success = flow_to_wave_command(flow_wave_comm_perform_step, &
                                          & numdomains, mudlay, nst)
             call timer_stop(timer_wait, gdp)
             if (.not. success) then
                txtput = 'Delftio command to waves failed'
                write(lunscr,'(a)') trim(txtput)
                call prterr(lundia    ,'P004'    ,trim(txtput)    )
                call d3stop(1, gdp)
             endif
          endif
       endif
       !
       ! Read and interpolate the wave information
       ! some array names are different in wave group on comm. file
       ! DIR := teta  , DISS := dis   , FX := wsu, FY := wsv,
       ! MX  := grmasu, MY   := grmasv
       !
       if (waverd) then
          call timer_start(timer_tricom_rest, gdp)
          call setwav(comfil    ,lundia    ,error     ,mmax      ,nmax      , &
                    & nmaxus    ,itimc     ,ntwav     ,itlen     ,timwav    , &
                    & norow     ,noroco    ,i(irocol) ,ifcore    ,d(dps)    , &
                    & r(s0)     ,r(uorb)   ,r(tp)     ,r(teta)   ,r(dis)    , &
                    & r(wsu)    ,r(wsv)    ,r(grmasu) ,r(grmasv) ,r(hrms)   , &
                    & r(tps)    ,r(ubot)   ,r(wlen)   ,r(hrmcom) ,r(tpcom)  , &
                    & r(dircom) ,r(discom) ,r(wsucom) ,r(wsvcom) ,r(msucom) , &
                    & r(msvcom) ,r(tpscom) ,r(ubcom)  ,r(wlcom)  ,r(rlabda) , &
                    & r(dircos) ,r(dirsin) ,r(ewave0) ,roller    ,wavcmp    , &
                    & r(ewabr0) ,gdp       )
          call timer_stop(timer_tricom_rest, gdp)
          if (error) goto 9998
       endif
       call vsemnefis
       if (roller) then
          call timer_start(timer_tricom_rest, gdp)
          if (snelli) then
             call snel(mmax      ,nmax      ,norow     ,noroco   ,r(ubot)  , &
                     & i(irocol) ,d(dps)    ,r(s0)     ,r(alfas) ,r(uorb)  , &
                     & r(tp)     ,r(teta)   ,r(hrms)   ,r(rlabda), &
                     & r(ewave0) ,r(wlen)   ,gdp      )
          endif
          call qkwcg(r(tp)     ,r(rlabda) ,r(teta)   ,r(qxkw)   ,r(qykw)   , &
                   & r(qxkr)   ,r(qykr)   ,d(dps)    ,r(s0)     ,i(kcs)    , &
                   & r(guu)    ,r(gvv)    ,r(cgc)    ,r(c)      ,i(irocol) , &
                   & norow     ,nocol     ,i(kfu)    ,i(kfv)    ,nmax      , &
                   & mmax      ,gdp       )
          call timer_stop(timer_tricom_rest, gdp)
          if (error) goto 9998
       endif
       call timer_start(timer_tricom_rest, gdp)
       call psemnefis
       if (cnstwv) then
          call constwave(nmmax     ,d(dps)    ,r(s0)     ,r(alfas)  ,r(ubot)   , &
                       & r(uorb)   ,r(tp)     ,r(teta)   ,r(hrms)   ,r(rlabda) , &
                       & r(wlen)   ,gdp       )
       endif
       call vsemnefis
       call timer_stop(timer_tricom_rest, gdp)
       !
       ! Solve FLOW-hydrodynamic
       !
       call timer_start(timer_trisol, gdp)
       if (.not.zmodel) then
          call trisol(dischy    ,solver    ,icreep    ,ithisc    , &
                    & timnow    ,nst       ,itiwec    ,trasol    ,forfuv    , &
                    & forfww    ,nfltyp    , &
                    & saleqs    ,temeqs    , &
                    & sferic    ,grdang    ,ktemp     ,temint    ,keva      , &
                    & evaint    ,anglat    ,rouflo    ,rouwav    , &
                    & betac     ,tkemod    ,comfil    , &
                    & error     ,gdp       )
       else
          if (nh_level == nh_full) then
             call z_trisol_nhfull(dischy    ,solver    ,icreep    , &
                                & timnow    ,nst       ,itiwec    ,trasol    ,forfuv    , &
                                & forfww    ,nfltyp    , &
                                & saleqs    ,temeqs    , &
                                & sferic    ,grdang    ,ktemp     ,temint    ,keva      , &
                                & evaint    ,anglat    ,rouflo    ,rouwav    , &
                                & betac     ,tkemod    ,gdp       )
          else
             call z_trisol(dischy    ,solver    ,icreep    , &
                         & timnow    ,nst       ,itiwec    ,trasol    ,forfuv    , &
                         & forfww    ,nfltyp    , &
                         & saleqs    ,temeqs    , &
                         & sferic    ,grdang    ,ktemp     ,temint    ,keva      , &
                         & evaint    ,anglat    ,rouflo    ,rouwav    , &
                         & betac     ,tkemod    ,gdp       )
          endif
       endif
       call timer_stop(timer_trisol, gdp)
       call timer_stop(timer_timeintegr, gdp)
    enddo
    
    ! The sequence of the 2 next calls is important for the OLV client.
    !
    call D3DOL_Timestep (itstop)
    call setEndFlag( olv_handle, 1 ) !Tells the DOL client that the simulation has ended by passing an exception

    !
    call timer_stop(timer_simulation, gdp)
    !
    ! Synchronisation point 3
    ! =======================
    ! - start finish semaphore
    ! - safe jump to error handling/communication closing (9999), inside the new semaphore
    !
  9998 continue
    !
    call timer_start(timer_close, gdp)
    !
    ! Finish using a semaphore
    ! Related vsemnefis is in trisim.F90
    ! This used to be a psemfinish
    !
    call psemnefis
    !
    if (error) goto 9999
    !
    ! End of synchronisation point 3
    ! ==============================
    !
    write (*, *)
    !
    ! Test routine print at last time step:
    !
    if (tstprt) then
       call pppsol(mmax      ,nmax      ,kmax      ,lstsci    ,ltur      , &
                 & runid     ,i(kcu)    ,i(kcv)    ,i(kcs)    ,i(kfu)    , &
                 & i(kfv)    ,i(kfs)    ,r(u1)     ,r(v1)     ,r(s1)     , &
                 & r(r1)     ,r(rtur1)  ,d(dps)    ,gdp       )
    endif
    !
    ! Output data of last time step to file if required
    !
    nst    = itstop
    nst2go = 0
    itimc  = modlen(itstop*itp, itlen)
    !
    ! Write (conditionaly) updated bathymetry to com-file
    ! The local parameter initi is improperly used. Fortunately it is
    ! not needed in the rest of this subroutine.
    !
    if (lsed > 0) then
       if (initi == 3) then
          initi = 5
       else
          initi = 4
       endif
       !
       call rwbotc(comfil    ,lundia    ,error     ,initi     ,itima     , &
                 & itcomi    ,mmax      ,nmax      ,nmaxus    ,r(dp)     , &
                 & r(rbuff)  ,ite       ,gdp       )
       if (error) goto 9999
    endif
    !
    ! Calculate post processing info and write to files if required
    !
    call postpr(lundia    ,lunprt    ,error     ,versio    ,comfil    , &
              & trifil    ,mainys    ,runid     ,prsmap    ,prshis    , &
              & selmap    ,selhis    ,rhow      ,grdang    , &
              & initi     ,dtsec     , &
              & nst       ,iphisc    ,npmap     ,itcomc    ,itimc     , &
              & itcur     ,ntcur     ,ithisc    ,itmapc    ,itdroc    , &
              & itrstc    ,ktemp     ,gdp       )
    if (waveol) then
       !
       ! Perform (last) wave computation (if required) and close communication with
       ! wave module
       !
       if (nst == itwav) then
          if (prec == hp) then
             call rwbotc_double(comfil    ,lundia    ,error     ,initi     ,itima     , &
                              & itcomi    ,mmax      ,nmax      ,nmaxus    ,d(dps)    , &
                              & r(rbuff)  ,ite       ,gdp       )
          else
             call rwbotc(comfil    ,lundia    ,error     ,initi     ,itima     , &
                       & itcomi    ,mmax      ,nmax      ,nmaxus    ,d(dps)    , &
                       & r(rbuff)  ,ite       ,gdp       )
          endif
          call timer_start(timer_wait, gdp)
          success = flow_to_wave_command(flow_wave_comm_finalize, &
                                        & numdomains, mudlay, nst)
          call timer_stop(timer_wait, gdp)
       else
          call timer_start(timer_wait, gdp)
          success = flow_to_wave_command(flow_wave_comm_finalize, &
                                        & numdomains, mudlay, -1)
          call timer_stop(timer_wait, gdp)
       endif
       if (.not. success) then
          txtput = 'Delftio command to waves failed'
          write(lunscr,'(a)') trim(txtput)
          call prterr(lundia    ,'P004'    ,trim(txtput)    )
          call d3stop(1, gdp)
       endif
    endif
    if (mudwave) then
       !
       ! Perform (last) wave computation (if required) and close communication with
       ! wave module
       !
       call timer_start(timer_wait, gdp)
       if (nst == itwav) then
          success = flow_to_wave_command(flow_wave_comm_finalize, &
                                        & numdomains, mudlay, nst)
       else
          success = flow_to_wave_command(flow_wave_comm_finalize, &
                                        & numdomains, mudlay, -1)
       endif
       call timer_stop(timer_wait, gdp)
       if (.not. success) then
          txtput = 'Delftio command to waves failed'
          write(lunscr,'(a)') trim(txtput)
          call prterr(lundia    ,'P004'    ,trim(txtput)    )
          call d3stop(1, gdp)
       endif
    endif
    !
 9999 continue
    if (error) then
       !
       ! Check if reading phase was passed
       !
       if (.not. lrdok) then
          write(*,*) 'error occured during reading phase, see tri-diag file for details.'
          !
          ! Send message to RTC and Couple to shutdown
          !
          if (rtcmod == dataFromRTCToFLOW .and. rtc_domainnr == 1) then
             write(*,*) 'Sending quit signal to RTC ...'
             call timer_start(timer_wait, gdp)
             call syncflowrtc_quit
             call timer_stop(timer_wait, gdp)
             write(*,*) '... continue'
          endif
          if (couplemod) then
             write(*,*) 'Sending quit signal to COUPLE ...'
             call timer_start(timer_wait, gdp)
             call syncflowcouple_quit(gdp%runid   , gdp%gdcoup%flowtocouple, &
                                    & gdp%gdcoup%pars    , gdp%gdcoup%locs        , &
                                    & gdp%gdcoup%putvalue)
             call timer_stop(timer_wait, gdp)
             write(*,*) '... continue'
          endif
       endif
       gdp%errorcode = 2
    endif
    !
    ! Close Communication with RTC if active after normal end
    !
    if (rtcact .and. rtcmod == dataFromRTCToFLOW .and. rtc_domainnr == 1) then
       write(*,*) 'Sending close signal to  RTC ...'
       call timer_start(timer_wait, gdp)
       call syncflowrtc_close
       call timer_stop(timer_wait, gdp)
       write(*,*) '... continue'
       rtcact = .false.
    endif
    !
    ! Close Communication with Couple if active after normal end    
    !
    if (couplemod .and. coupleact) then
       write(*,*) 'Sending close signal to  COUPLE ...'
       call timer_start(timer_wait, gdp)
       call syncflowcouple_close(gdp%gdcoup%flowtocouple, gdp%gdcoup%coupletoflow)
       call timer_stop(timer_wait, gdp)
       write(*,*) '... continue'
       coupleact = .false.
    endif
    !
    ! Close Communication with Sobek if active after normal end
    !
    if (sbkol) then
       write(*,*) 'Sending finalize signal to  Sobek ...'
       call timer_start(timer_wait, gdp)
       call D3S_Finalize('D3D-FLOW', 'Sobek')
       call timer_stop(timer_wait, gdp)
       write(*,*) '... continue'
    endif
    !
    ! Mormerge synchronisation
    !
    if (multi) then
       lunfil = newlun(gdp)
       open (lunfil, file=mmsyncfilnam, position='append', action='write', iostat=istat)
       if (istat /= 0) then
          write(*,*)' *** WARNING: unable to write in file ',trim(mmsyncfilnam)
       else
          write(lunfil,'(a)') 'Finished'
          close(lunfil)
       endif
    endif
    !
    call timer_stop(timer_close, gdp)
    call timer_stop(timer_tricomtot, gdp)
end subroutine tricom
