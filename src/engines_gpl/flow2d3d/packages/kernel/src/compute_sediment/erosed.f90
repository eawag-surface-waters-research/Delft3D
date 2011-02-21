subroutine erosed(nmmax     ,kmax      ,icx       ,icy       ,lundia    , &
                & nst       ,lsed      ,lsedtot   ,lsal      ,ltem      , &
                & lsecfl    ,kfs       ,kfu       ,kfv       ,sig       , &
                & r0        ,u0eul     ,v0eul     ,s0        ,dps       , &
                & z0urou    ,z0vrou    ,sour      ,sink      ,rhowat    , &
                & ws        ,rsedeq    ,z0ucur    ,z0vcur    ,sigmol    , &
                & taubmx    ,s1        ,uorb      ,tp        ,sigdif    , &
                & lstsci    ,thick     ,dicww     ,kmxsed    ,kcs       , &
                & kcu       ,kcv       ,guv       ,gvu       ,sbuu      , &
                & sbvv      ,seddif    ,hrms      ,dis       ,ltur      , &
                & teta      ,rlabda    ,aks       ,kfsed     ,saleqs    , &
                & sbuut     ,sbvvt     ,entr      ,wstau     ,hu        , &
                & hv        ,rca       ,dss       ,ubot      , &
                & temeqs    ,gdp       )
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
!!--description-----------------------------------------------------------------
!
!    Function: Computes sediment fluxes at the bed using
!              the Partheniades-Krone formulations.
!              Arrays SOURSE and SINKSE are filled and added
!              to arrays SOUR and SINK
!              Computes bed load transport for sand sediment
!              Arrays SBUU and SBVV are filled.
!              Computes vertical sediment diffusion coefficient
!              Array SEDDIF is filled
!              Includes wave asymmetry effects on sand bed-load
!              transport
!              Bed slope effects computed at the U and V velocity
!              points
! Method used: Attention: pointer ll for 'standard' FLOW
!              arrays is shifted with lstart
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use bedcomposition_module
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                             , pointer :: gammax
    real(fp)                             , pointer :: rhow
    real(fp)                             , pointer :: ag
    real(fp)                             , pointer :: z0
    real(fp)                             , pointer :: z0v
    real(fp)                             , pointer :: vicmol
    integer                              , pointer :: nmudfrac
    real(fp)      , dimension(:)         , pointer :: rhosol
    real(fp)      , dimension(:,:,:)     , pointer :: logseddia
    real(fp)      , dimension(:)         , pointer :: logsedsig
    real(fp)      , dimension(:)         , pointer :: sedd10
    real(fp)      , dimension(:)         , pointer :: sedd50
    real(fp)      , dimension(:)         , pointer :: sedd90
    real(fp)      , dimension(:)         , pointer :: sedd50fld
    real(fp)      , dimension(:)         , pointer :: dstar
    real(fp)      , dimension(:)         , pointer :: taucr
    real(fp)      , dimension(:)         , pointer :: tetacr
    real(fp)      , dimension(:)         , pointer :: ws0
    real(fp)      , dimension(:)         , pointer :: salmax
    real(fp)      , dimension(:)         , pointer :: mudcnt
    integer       , dimension(:)         , pointer :: nseddia
    integer       , dimension(:)         , pointer :: sedtyp
    logical                              , pointer :: anymud
    real(fp)                             , pointer :: thresh
    real(fp)                             , pointer :: bed
    real(fp)                             , pointer :: susw
    real(fp)                             , pointer :: sedthr
    real(fp)                             , pointer :: bedw
    integer                              , pointer :: i10
    integer                              , pointer :: i50
    integer                              , pointer :: i90
    integer                              , pointer :: nxx
    real(fp)              , dimension(:) , pointer :: xx
    logical                              , pointer :: multi
    logical                              , pointer :: wind
    logical                              , pointer :: salin
    logical                              , pointer :: wave
    logical                              , pointer :: struct
    logical                              , pointer :: sedim
    real(fp)                             , pointer :: eps
    integer                              , pointer :: ifirst
    real(fp), dimension(:)               , pointer :: bc_mor_array
    real(fp), dimension(:,:)             , pointer :: dbodsd
    real(fp), dimension(:)               , pointer :: dcwwlc
    real(fp), dimension(:)               , pointer :: dm
    real(fp), dimension(:)               , pointer :: dg
    real(fp), dimension(:,:)             , pointer :: dxx
    real(fp), dimension(:)               , pointer :: dzduu
    real(fp), dimension(:)               , pointer :: dzdvv
    real(fp), dimension(:)               , pointer :: epsclc
    real(fp), dimension(:)               , pointer :: epswlc
    real(fp), dimension(:,:)             , pointer :: fixfac
    real(fp), dimension(:,:)             , pointer :: frac
    real(fp), dimension(:)               , pointer :: mudfrac
    real(fp), dimension(:,:)             , pointer :: hidexp
    real(fp), dimension(:)               , pointer :: rsdqlc
    real(fp), dimension(:,:)             , pointer :: sbcu
    real(fp), dimension(:,:)             , pointer :: sbcv
    real(fp), dimension(:,:)             , pointer :: sbcuu
    real(fp), dimension(:,:)             , pointer :: sbcvv
    real(fp), dimension(:,:)             , pointer :: sbwu
    real(fp), dimension(:,:)             , pointer :: sbwv
    real(fp), dimension(:,:)             , pointer :: sbwuu
    real(fp), dimension(:,:)             , pointer :: sbwvv
    real(fp), dimension(:)               , pointer :: sddflc
    real(fp), dimension(:,:)             , pointer :: sswu
    real(fp), dimension(:,:)             , pointer :: sswv
    real(fp), dimension(:,:)             , pointer :: sswuu
    real(fp), dimension(:,:)             , pointer :: sswvv
    real(fp), dimension(:,:)             , pointer :: sutot
    real(fp), dimension(:,:)             , pointer :: svtot
    real(fp), dimension(:,:)             , pointer :: sinkse
    real(fp), dimension(:,:)             , pointer :: sourse
    real(fp), dimension(:,:)             , pointer :: taurat
    real(fp), dimension(:)               , pointer :: ust2
    real(fp), dimension(:)               , pointer :: umod
    real(fp), dimension(:)               , pointer :: uuu
    real(fp), dimension(:)               , pointer :: vvv
    real(fp), dimension(:)               , pointer :: wslc
    real(fp), dimension(:)               , pointer :: zumod
    logical                              , pointer :: scour
    integer,        dimension(:)         , pointer :: iform
    real(fp),       dimension(:,:)       , pointer :: par
    real(fp)                             , pointer :: factcr
    integer                              , pointer :: ihidexp
    real(fp)                             , pointer :: asklhe
    real(fp)                             , pointer :: mwwjhe
    real(fp)                             , pointer :: ffthresh
    real(fp), dimension(:)               , pointer :: rksr
    real(fp)                             , pointer :: sus
    real(fp)                             , pointer :: espir
    real(fp)                             , pointer :: vonkar
    logical                              , pointer :: epspar
    logical                              , pointer :: ubot_from_com
    real(fp)                             , pointer :: timsec
    real(fp)                             , pointer :: camax
    real(fp)                             , pointer :: aksfac
    real(fp)                             , pointer :: rwave
    real(fp)                             , pointer :: rdc
    real(fp)                             , pointer :: rdw
    real(fp)                             , pointer :: pangle
    real(fp)                             , pointer :: fpco
    integer                              , pointer :: iopsus
    integer                              , pointer :: iopkcw
    integer                              , pointer :: subiw
    integer                              , pointer :: max_integers
    integer                              , pointer :: max_reals
    integer                              , pointer :: max_strings
    character(256), dimension(:)         , pointer :: dll_function
    integer,        dimension(:)         , pointer :: dll_handle
    integer       , dimension(:)         , pointer :: dll_integers
    real(hp)      , dimension(:)         , pointer :: dll_reals
    character(256), dimension(:)         , pointer :: dll_strings
    character(256), dimension(:)         , pointer :: dll_usrfil
    logical                              , pointer :: bsskin
    real(fp)      , dimension(:)         , pointer :: thcmud
    real(fp)                             , pointer :: kssilt
    real(fp)                             , pointer :: kssand
    logical                          , pointer :: oldmudfrac
    logical                          , pointer :: flmd2l
    real(fp)      , dimension(:,:)   , pointer :: tcrdep
    real(fp)      , dimension(:,:)   , pointer :: tcrero
    real(fp)      , dimension(:,:)   , pointer :: eropar
    include 'flow_steps_f.inc'
    include 'sedparams.inc'
!
! Local parameters
!
    integer, parameter :: kmax2d       = 20
!
! Global variables
!
    integer                                                   , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                   , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                   , intent(in)  :: kmax   !  Description and declaration in iidim.f90
    integer                                                   , intent(in)  :: lsal   !  Description and declaration in dimens.igs
    integer                                                   , intent(in)  :: lsed   !  Description and declaration in iidim.f90
    integer                                                   , intent(in)  :: lsedtot!  Description and declaration in iidim.f90
    integer                                                   , intent(in)  :: lstsci !  Description and declaration in iidim.f90
    integer                                                   , intent(in)  :: ltem   !  Description and declaration in dimens.igs
    integer                                                   , intent(in)  :: lsecfl !  Description and declaration in dimens.igs    
    integer                                                                 :: ltur   !  Description and declaration in iidim.f90
    integer                                                                 :: lundia !  Description and declaration in inout.igs
    integer                                                   , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                   , intent(in)  :: nst    !!
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcs    !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcu    !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcv    !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfs    !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfsed  !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfu    !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfv    !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, lsed)                      :: kmxsed !  Description and declaration in iidim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: aks    !  Description and declaration in rjdim.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps    !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: entr   !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: guv    !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gvu    !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: hrms   !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dis    !  Description and declaration in rjdim.f90     
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: hu     !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: hv     !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: rlabda !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s0     !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s1     !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: sbuut
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: sbvvt
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: taubmx !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: teta   !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: tp     !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: uorb   !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: ubot   !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: wstau  !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0ucur
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0urou !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0vcur
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0vrou !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: dicww  !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, *)   , intent(in)  :: ws     !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, lsed)              :: seddif !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: rhowat !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: u0eul  !!  EULARIAN U-velocities at old time level
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: v0eul  !!  EULARIAN V-velocities at old time level
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, *)                   :: r0     !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, *)                   :: sink   !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, *)                   :: sour   !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lsed)                :: rsedeq !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, lsed)                      :: dss    !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, lsed)        , intent(out) :: rca    !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot)                   :: sbuu   !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot)                   :: sbvv   !  Description and declaration in rjdim.f90
    real(fp)  , dimension(kmax)                               , intent(in)  :: sig    !  Description and declaration in rjdim.f90
    real(fp)  , dimension(kmax)                               , intent(in)  :: thick  !  Description and declaration in rjdim.f90
    real(fp)  , dimension(lstsci)                             , intent(out) :: sigdif !  Description and declaration in rjdim.f90
    real(fp)  , dimension(lstsci)                                           :: sigmol !  Description and declaration in rjdim.f90
    real(fp)                                                  , intent(in)  :: saleqs
    real(fp)                                                  , intent(in)  :: temeqs
!
! Local variables
!
    integer                       :: istat
    integer                       :: k
    integer                       :: k2d
    integer                       :: kmaxsd
    integer                       :: kn
    integer                       :: ku
    integer                       :: kv
    integer                       :: l
    integer                       :: ll
    integer                       :: lstart
    integer                       :: m
    integer                       :: n
    integer                       :: ndm
    integer                       :: nhystp
    integer                       :: nm
    integer                       :: nmd
    integer                       :: nmu
    integer                       :: num
    logical                       :: error
    logical                       :: suspfrac  ! suspended component sedtyp(l)/=SEDTYP_NONCOHESIVE_TOTALLOAD
    real(fp)                      :: akstmp
    real(fp)                      :: ce_nm
    real(fp)                      :: ce_nmtmp
    real(fp)                      :: crep
    real(fp)                      :: d10
    real(fp)                      :: d90
    real(fp)                      :: di50
    real(fp)                      :: difbot
    real(fp)                      :: drho
    real(fp)                      :: dstari
    real(fp)                      :: fi
    real(fp)                      :: h0
    real(fp)                      :: h1
    real(fp)                      :: hrmsnm
    real(fp)                      :: rlnm
    real(fp)                      :: salinity
    real(fp)                      :: spirint   ! local variable for spiral flow intensity r0(nm,1,lsecfl)
    real(fp)                      :: tauadd
    real(fp)                      :: taub
    real(fp)                      :: tdss      ! temporary variable for dss
    real(fp)                      :: temperature
    real(fp)                      :: tetanm
    real(fp)                      :: thick0
    real(fp)                      :: thick1
    real(fp)                      :: tpnm
    real(fp)                      :: trsedeq   ! temporary variable for rsedeq
    real(fp)                      :: tsalmax
    real(fp)                      :: tsd
    real(fp)                      :: tsigmol   ! temporary variable for sigmol
    real(fp)                      :: tws0
    real(fp)                      :: ubed
    real(fp)                      :: umean
    real(fp)                      :: uorbnm
    real(fp)                      :: ustarc
    real(fp)                      :: vbed
    real(fp)                      :: vmean
    real(fp)                      :: z0cur
    real(fp)                      :: z0rou
    real(fp), dimension(0:kmax2d) :: dcww2d
    real(fp), dimension(0:kmax2d) :: sddf2d
    real(fp), dimension(0:kmax2d) :: ws2d
    real(fp), dimension(kmax2d)   :: rsdq2d
    real(fp), dimension(kmax2d)   :: sig2d
    real(fp), dimension(kmax2d)   :: thck2d
    real(fp), dimension(kmax)     :: concin3d
    real(fp), dimension(kmax2d)   :: concin2d
    character(256)    :: errmsg
    !
    data thck2d/0.1747, 0.1449, 0.1202, 0.0997, 0.0827, 0.0686, 0.0569, 0.0472, &
       & 0.0391, 0.0325, 0.0269, 0.0223, 0.0185, 0.0154, 0.0127, 0.0106, 0.0088,&
       & 0.0073, 0.0060, 0.0050/
    data sig2d/ - 0.0874, -0.2472, -0.3797, -0.4897, -0.5809, -0.6565, -0.7193, &
       & -0.7713, -0.8145, -0.8503, -0.8800, -0.9046, -0.9250, -0.9419, -0.9560,&
       & -0.9676, -0.9773, -0.9854, -0.9920, -0.9975/
!
!! executable statements -------------------------------------------------------
!
    wind                => gdp%gdprocs%wind
    salin               => gdp%gdprocs%salin
    wave                => gdp%gdprocs%wave
    struct              => gdp%gdprocs%struct
    sedim               => gdp%gdprocs%sedim
    nmudfrac            => gdp%gdsedpar%nmudfrac
    rhosol              => gdp%gdsedpar%rhosol
    logseddia           => gdp%gdsedpar%logseddia
    logsedsig           => gdp%gdsedpar%logsedsig
    sedd10              => gdp%gdsedpar%sedd10
    sedd50              => gdp%gdsedpar%sedd50
    sedd90              => gdp%gdsedpar%sedd90
    sedd50fld           => gdp%gdsedpar%sedd50fld
    dstar               => gdp%gdsedpar%dstar
    taucr               => gdp%gdsedpar%taucr
    tetacr              => gdp%gdsedpar%tetacr
    ws0                 => gdp%gdsedpar%ws0
    salmax              => gdp%gdsedpar%salmax
    mudcnt              => gdp%gdsedpar%mudcnt
    nseddia             => gdp%gdsedpar%nseddia
    sedtyp              => gdp%gdsedpar%sedtyp
    anymud              => gdp%gdsedpar%anymud
    thresh              => gdp%gdmorpar%thresh
    bed                 => gdp%gdmorpar%bed
    susw                => gdp%gdmorpar%susw
    sedthr              => gdp%gdmorpar%sedthr
    bedw                => gdp%gdmorpar%bedw
    i10                 => gdp%gdmorpar%i10
    i50                 => gdp%gdmorpar%i50
    i90                 => gdp%gdmorpar%i90
    nxx                 => gdp%gdmorpar%nxx
    xx                  => gdp%gdmorpar%xx
    multi               => gdp%gdmorpar%multi
    factcr              => gdp%gdmorpar%factcr
    ihidexp             => gdp%gdmorpar%ihidexp
    asklhe              => gdp%gdmorpar%asklhe
    mwwjhe              => gdp%gdmorpar%mwwjhe
    ffthresh            => gdp%gdmorpar%thresh
    rhow                => gdp%gdphysco%rhow
    ag                  => gdp%gdphysco%ag
    z0                  => gdp%gdphysco%z0
    z0v                 => gdp%gdphysco%z0v
    vicmol              => gdp%gdphysco%vicmol
    gammax              => gdp%gdnumeco%gammax
    eps                 => gdp%gdconst%eps
    ifirst              => gdp%gderosed%ifirst
    bc_mor_array        => gdp%gderosed%bc_mor_array
    dbodsd              => gdp%gderosed%dbodsd
    dcwwlc              => gdp%gderosed%dcwwlc
    dm                  => gdp%gderosed%dm
    dg                  => gdp%gderosed%dg
    dxx                 => gdp%gderosed%dxx
    dzduu               => gdp%gderosed%dzduu
    dzdvv               => gdp%gderosed%dzdvv
    epsclc              => gdp%gderosed%epsclc
    epswlc              => gdp%gderosed%epswlc
    fixfac              => gdp%gderosed%fixfac
    frac                => gdp%gderosed%frac
    mudfrac             => gdp%gderosed%mudfrac
    hidexp              => gdp%gderosed%hidexp
    rsdqlc              => gdp%gderosed%rsdqlc
    sbcu                => gdp%gderosed%sbcu
    sbcv                => gdp%gderosed%sbcv
    sbcuu               => gdp%gderosed%sbcuu
    sbcvv               => gdp%gderosed%sbcvv
    sbwu                => gdp%gderosed%sbwu
    sbwv                => gdp%gderosed%sbwv
    sbwuu               => gdp%gderosed%sbwuu
    sbwvv               => gdp%gderosed%sbwvv
    sddflc              => gdp%gderosed%sddflc
    sswu                => gdp%gderosed%sswu
    sswv                => gdp%gderosed%sswv
    sswuu               => gdp%gderosed%sswuu
    sswvv               => gdp%gderosed%sswvv
    sutot               => gdp%gderosed%sutot
    svtot               => gdp%gderosed%svtot
    sinkse              => gdp%gderosed%sinkse
    sourse              => gdp%gderosed%sourse
    taurat              => gdp%gderosed%taurat
    ust2                => gdp%gderosed%ust2
    umod                => gdp%gderosed%umod
    uuu                 => gdp%gderosed%uuu
    vvv                 => gdp%gderosed%vvv
    wslc                => gdp%gderosed%wslc
    zumod               => gdp%gderosed%zumod
    scour               => gdp%gdscour%scour
    iform               => gdp%gdeqtran%iform
    par                 => gdp%gdeqtran%par
    rksr                => gdp%gdbedformpar%rksr
    sus                 => gdp%gdmorpar%sus
    bed                 => gdp%gdmorpar%bed
    susw                => gdp%gdmorpar%susw
    bedw                => gdp%gdmorpar%bedw
    espir               => gdp%gdmorpar%espir
    epspar              => gdp%gdmorpar%epspar 
    rhow                => gdp%gdphysco%rhow
    ag                  => gdp%gdphysco%ag
    vonkar              => gdp%gdphysco%vonkar
    vicmol              => gdp%gdphysco%vicmol
    wave                => gdp%gdprocs%wave
    scour               => gdp%gdscour%scour
    timsec              => gdp%gdinttim%timsec
    camax               => gdp%gdmorpar%camax
    aksfac              => gdp%gdmorpar%aksfac
    rwave               => gdp%gdmorpar%rwave
    rdc                 => gdp%gdmorpar%rdc
    rdw                 => gdp%gdmorpar%rdw
    pangle              => gdp%gdmorpar%pangle
    fpco                => gdp%gdmorpar%fpco
    iopsus              => gdp%gdmorpar%iopsus
    iopkcw              => gdp%gdmorpar%iopkcw
    subiw               => gdp%gdmorpar%subiw
    ubot_from_com       => gdp%gdprocs%ubot_from_com
    max_integers        => gdp%gdeqtran%max_integers
    max_reals           => gdp%gdeqtran%max_reals
    max_strings         => gdp%gdeqtran%max_strings
    dll_function        => gdp%gdeqtran%dll_function
    dll_handle          => gdp%gdeqtran%dll_handle
    dll_integers        => gdp%gdeqtran%dll_integers
    dll_reals           => gdp%gdeqtran%dll_reals
    dll_strings         => gdp%gdeqtran%dll_strings
    dll_usrfil          => gdp%gdeqtran%dll_usrfil
    bsskin              => gdp%gdsedpar%bsskin
    thcmud              => gdp%gdsedpar%thcmud
    kssilt              => gdp%gdsedpar%kssilt
    kssand              => gdp%gdsedpar%kssand
    oldmudfrac          => gdp%gdmorpar%oldmudfrac
    flmd2l              => gdp%gdprocs%flmd2l
    tcrdep              => gdp%gdsedpar%tcrdep
    tcrero              => gdp%gdsedpar%tcrero
    eropar              => gdp%gdsedpar%eropar
    !
    if (ifirst == 1) then
       ifirst = 0
       !
       ! Allocate using the gdp structure itself instead of the local pointers
       !
                     allocate (gdp%gderosed%bc_mor_array(lsedtot*2)              , stat = istat)
       if (istat==0) allocate (gdp%gderosed%dbodsd(lsedtot,gdp%d%nmlb:gdp%d%nmub), stat = istat)
       if (istat==0) allocate (gdp%gderosed%dcwwlc(0:kmax)                       , stat = istat)
       if (istat==0) allocate (gdp%gderosed%dzduu (gdp%d%nmlb:gdp%d%nmub)        , stat = istat)
       if (istat==0) allocate (gdp%gderosed%dzdvv (gdp%d%nmlb:gdp%d%nmub)        , stat = istat)
       if (istat==0) allocate (gdp%gderosed%epsclc(0:kmax)                       , stat = istat)
       if (istat==0) allocate (gdp%gderosed%epswlc(0:kmax)                       , stat = istat)
       if (istat==0) allocate (gdp%gderosed%fixfac(gdp%d%nmlb:gdp%d%nmub,lsedtot), stat = istat)
       if (istat==0) allocate (gdp%gderosed%rsdqlc(kmax)                         , stat = istat)
       if (istat==0) allocate (gdp%gderosed%sbcu  (gdp%d%nmlb:gdp%d%nmub,lsedtot), stat = istat)
       if (istat==0) allocate (gdp%gderosed%sbcuu (gdp%d%nmlb:gdp%d%nmub,lsedtot), stat = istat)
       if (istat==0) allocate (gdp%gderosed%sbcv  (gdp%d%nmlb:gdp%d%nmub,lsedtot), stat = istat)
       if (istat==0) allocate (gdp%gderosed%sbcvv (gdp%d%nmlb:gdp%d%nmub,lsedtot), stat = istat)
       if (istat==0) allocate (gdp%gderosed%sbwu  (gdp%d%nmlb:gdp%d%nmub,lsedtot), stat = istat)
       if (istat==0) allocate (gdp%gderosed%sbwuu (gdp%d%nmlb:gdp%d%nmub,lsedtot), stat = istat)
       if (istat==0) allocate (gdp%gderosed%sbwv  (gdp%d%nmlb:gdp%d%nmub,lsedtot), stat = istat)
       if (istat==0) allocate (gdp%gderosed%sbwvv (gdp%d%nmlb:gdp%d%nmub,lsedtot), stat = istat)
       if (istat==0) allocate (gdp%gderosed%sddflc(0:kmax)                       , stat = istat)
       if (istat==0) allocate (gdp%gderosed%sinkse(gdp%d%nmlb:gdp%d%nmub,lsed)   , stat = istat)
       if (istat==0) allocate (gdp%gderosed%sourse(gdp%d%nmlb:gdp%d%nmub,lsed)   , stat = istat)
       if (istat==0) allocate (gdp%gderosed%sswu  (gdp%d%nmlb:gdp%d%nmub,lsedtot), stat = istat)
       if (istat==0) allocate (gdp%gderosed%sswuu (gdp%d%nmlb:gdp%d%nmub,lsedtot), stat = istat)
       if (istat==0) allocate (gdp%gderosed%sswv  (gdp%d%nmlb:gdp%d%nmub,lsedtot), stat = istat)
       if (istat==0) allocate (gdp%gderosed%sswvv (gdp%d%nmlb:gdp%d%nmub,lsedtot), stat = istat)
       if (istat==0) allocate (gdp%gderosed%sutot (gdp%d%nmlb:gdp%d%nmub,lsedtot), stat = istat)
       if (istat==0) allocate (gdp%gderosed%svtot (gdp%d%nmlb:gdp%d%nmub,lsedtot), stat = istat)
       if (istat==0) allocate (gdp%gderosed%taurat(gdp%d%nmlb:gdp%d%nmub,lsedtot), stat = istat)
       if (istat==0) allocate (gdp%gderosed%umod  (gdp%d%nmlb:gdp%d%nmub)        , stat = istat)
       if (istat==0) allocate (gdp%gderosed%ust2  (gdp%d%nmlb:gdp%d%nmub)        , stat = istat)
       if (istat==0) allocate (gdp%gderosed%uuu   (gdp%d%nmlb:gdp%d%nmub)        , stat = istat)
       if (istat==0) allocate (gdp%gderosed%vvv   (gdp%d%nmlb:gdp%d%nmub)        , stat = istat)
       if (istat==0) allocate (gdp%gderosed%wslc  (0:kmax)                       , stat = istat)
       if (istat==0) allocate (gdp%gderosed%zumod (gdp%d%nmlb:gdp%d%nmub)        , stat = istat)
       if (istat/=0) then
          call prterr(lundia, 'U021', 'Erosed: memory alloc error')
          call d3stop(1, gdp)
       endif
       !
       ! update local pointers
       !
       bc_mor_array        => gdp%gderosed%bc_mor_array
       dbodsd              => gdp%gderosed%dbodsd
       dcwwlc              => gdp%gderosed%dcwwlc
       dzduu               => gdp%gderosed%dzduu
       dzdvv               => gdp%gderosed%dzdvv
       epsclc              => gdp%gderosed%epsclc
       epswlc              => gdp%gderosed%epswlc
       fixfac              => gdp%gderosed%fixfac
       rsdqlc              => gdp%gderosed%rsdqlc
       sbcu                => gdp%gderosed%sbcu
       sbcuu               => gdp%gderosed%sbcuu
       sbcv                => gdp%gderosed%sbcv
       sbcvv               => gdp%gderosed%sbcvv
       sbwu                => gdp%gderosed%sbwu
       sbwuu               => gdp%gderosed%sbwuu
       sbwv                => gdp%gderosed%sbwv
       sbwvv               => gdp%gderosed%sbwvv
       sddflc              => gdp%gderosed%sddflc
       sinkse              => gdp%gderosed%sinkse
       sourse              => gdp%gderosed%sourse
       sswu                => gdp%gderosed%sswu
       sswuu               => gdp%gderosed%sswuu
       sswv                => gdp%gderosed%sswv
       sswvv               => gdp%gderosed%sswvv
       sutot               => gdp%gderosed%sutot
       svtot               => gdp%gderosed%svtot
       taurat              => gdp%gderosed%taurat
       umod                => gdp%gderosed%umod
       ust2                => gdp%gderosed%ust2
       uuu                 => gdp%gderosed%uuu
       vvv                 => gdp%gderosed%vvv
       wslc                => gdp%gderosed%wslc
       zumod               => gdp%gderosed%zumod
       !
       ust2   = 0.0_fp
       dbodsd = 0.0_fp
       fixfac = 1.0_fp
       hidexp = 1.0_fp
    endif
    if (scour) then
       !
       ! Second parameter is zero: save taubmx(*) in gdp%gdscour
       !
       call shearx(taubmx, 0, gdp)
    endif
    !
    ! Determine total thickness of the mud layers
    ! to be used in computation of skin friction (Soulsby 2004)
    !
    if (bsskin) then
       call detthcmud(gdp%gdmorlyr  ,sedtyp    ,thcmud    )
    endif
    !
    ! Initialisation:
    ! reset sediment sources and sinks
    !     set default kmxsed layer
    !     set kfsed
    !
    lstart = max(lsal, ltem)
    !
    ! Reset Sourse and Sinkse arrays for all (nm,l)
    !
    kmxsed = 1
    sourse = 0.0_fp
    sinkse = 0.0_fp
    !
    ! Reset Sediment diffusion arrays for (nm,k,l)
    !
    seddif = 0.0_fp
    rca    = 0.0_fp
    !
    ! Reset Bed Shear Ratio for all nm and l = 1:lsedtot
    !                        
    taurat = 0.0_fp
    !
    ! Set zero bedload transport for all nm and l = 1:lsedtot
    !
    sbuu  = 0.0_fp
    sbvv  = 0.0_fp
    sbcu  = 0.0_fp
    sbcv  = 0.0_fp
    sbcuu = 0.0_fp
    sbcvv = 0.0_fp
    sbwu  = 0.0_fp
    sbwv  = 0.0_fp
    sbwuu = 0.0_fp
    sbwvv = 0.0_fp
    sswu  = 0.0_fp
    sswv  = 0.0_fp
    sswuu = 0.0_fp
    sswvv = 0.0_fp
    sutot = 0.0_fp
    svtot = 0.0_fp
    !
    call dfexchg( dps,1, 1, dfloat, gdp)
    !
    do nm = 1, nmmax
       if ((s1(nm) + real(dps(nm),fp))*kfs(nm) > sedthr) then
          kfsed(nm) = 1
       else
          kfsed(nm) = 0
       endif
    enddo
    !
    call dfexchg( kfsed,1, 1, dfint, gdp)
    !
    ! Determine fractions of all sediments the top layer and
    ! compute the mud fraction.
    !
    if (lsedtot > 1) then
       call getfrac(gdp%gdmorlyr,frac      ,sedtyp    ,anymud    , &
                  & mudcnt      ,mudfrac   ,gdp%d%nmlb,gdp%d%nmub)
    endif
    !
    ! Calculate velocity components and magnitude at the zeta points
    ! based on velocity in the bottom computational layer
    !
    call dwnvel(nmmax     ,kmax      ,icx       ,kcs       ,kfu       , &
              & kfv       ,kcu       ,kcv       ,s1        ,dps       , &
              & u0eul     ,v0eul     ,uuu       ,vvv       ,umod      , &
              & zumod     ,sig       ,hu        ,hv        ,kfsed     , &
              & gdp       )
    call dfexchg( uuu,1, 1, dfloat, gdp)
    call dfexchg( vvv,1, 1, dfloat, gdp)
    call dfexchg( umod,1, 1, dfloat, gdp)
    call dfexchg( zumod,1, 1, dfloat, gdp)
    !
    ! Get the reduction factor if thickness of sediment at bed is less than
    ! user specified threshold
    !
    call getfixfac(gdp%gdmorlyr, gdp%d%nmlb, gdp%d%nmub, lsedtot, &
                 & nmmax       , fixfac    , ffthresh  )
    !
    ! in case of multiple (non-mud) fractions, the following quantities
    ! --- that are initialized in INISED --- may be time-dependent and
    ! they must be updated here or after updating the bed levels in
    ! BOTT3D. Since we do it here, these quantities will lag a half time
    ! step behind on the output files. If these statements are moved to
    ! BOTT3D, the GETFRAC call above must be shifted too.
    !
    if (lsedtot-nmudfrac > 1) then
       !
       ! calculate arithmetic mean sediment diameter Dm
       ! calculate geometric mean sediment diameter Dg
       ! calculate percentiles Dxx
       !
       call compdiam(frac      ,sedd50    ,sedd50    ,sedtyp    ,lsedtot   , &
                   & logsedsig ,nseddia   ,logseddia ,nmmax     ,gdp%d%nmlb, &
                   & gdp%d%nmub,xx        ,nxx       ,sedd50fld ,dm        , &
                   & dg        ,dxx       )
       !
       ! determine hiding & exposure factors
       !
       call comphidexp(frac      ,dm        ,nmmax     ,lsedtot   , &
                     & sedd50    ,hidexp    ,ihidexp   ,asklhe    , &
                     & mwwjhe    ,gdp%d%nmlb,gdp%d%nmub)
    endif
    !
    do nm = 1, nmmax
       !
       ! calculate and store bed slopes at U and V points
       !
       nmu = nm + icx
       num = nm + icy
       if (kcu(nm) > 0) then
          dzduu(nm) = (real(dps(nmu),fp) - real(dps(nm),fp))/gvu(nm)
       else
          dzduu(nm) = 0.0_fp
       endif
       if (kcv(nm) > 0) then
          dzdvv(nm) = (real(dps(num),fp) - real(dps(nm),fp))/guv(nm)
       else
          dzdvv(nm) = 0.0_fp
       endif
    enddo
    !
    call dfexchg( dzduu,1, 1, dfloat, gdp)
    call dfexchg( dzdvv,1, 1, dfloat, gdp)
    !
    !================================================================
    !    Start of sand part
    !================================================================
    !
    ! Start of main loop over sediment fractions for suspended sediment
    ! sources, sinks, equilibrium concentrations and vertical diffusion
    ! coefficients, and bed-load transport vector components at water
    ! level points
    !
    call dfexchg( z0ucur,1, 1, dfloat, gdp)
    call dfexchg( z0vcur,1, 1, dfloat, gdp)
    call dfexchg( z0urou,1, 1, dfloat, gdp)
    call dfexchg( z0vrou,1, 1, dfloat, gdp)
    do l = 1, lsedtot
       call dfexchg( ws(:,:,l),0, kmax, dfloat, gdp)
    enddo
    !
    if (scour) then
       !
       ! Calculate extra stress (tauadd) for point = nm,
       ! if so required by user input.
       !
       call shearx(tauadd, nm, gdp)
    else
       tauadd = 0.0_fp
    endif
    !
    do nm = 1, nmmax
       if (kfs(nm)/=1 .or. kcs(nm)>2) cycle
       !
       ! do not calculate sediment sources, sinks, and bed load
       ! transport in areas with very shallow water.
       !
       if (kfsed(nm) == 0) then
          !
          ! Very shallow water:
          ! set sediment diffusion coefficient
          ! and set zero equilibrium concentrations
          !
          if (kmax>1) then
             do l = 1, lsed
                do k = 1, kmax
                   seddif(nm, k, l) = dicww(nm, k)
                   rsedeq(nm, k, l) = 0.0_fp
                enddo
             enddo
          endif
          cycle
       endif
       !
       ! kfsed(nm) == 1
       !
       h0   = max(0.01_fp, s0(nm) + real(dps(nm),fp))
       h1   = max(0.01_fp, s1(nm) + real(dps(nm),fp))
       nmd  = nm - icx
       ndm  = nm - icy
       call nm_to_n_and_m(nm, n, m, gdp)
       !
       ! Compute depth-averaged velocity components at cell centre
       !
       ku = max(1,kfu(nmd) + kfu(nm))
       kv = max(1,kfv(ndm) + kfv(nm))
       umean = 0.0
       vmean = 0.0
       do k = 1, kmax
          umean = umean + thick(k)*(u0eul(nm,k) + u0eul(nmd,k))/ku
          vmean = vmean + thick(k)*(v0eul(nm,k) + v0eul(ndm,k))/kv
       enddo
       ubed = (u0eul(nm,kmax) + u0eul(nmd,kmax))/ku
       vbed = (v0eul(nm,kmax) + v0eul(ndm,kmax))/kv
       !
       ! Calculate current related roughness
       !
       kn    = max(1, kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm))
       z0cur = (  kfu(nmd)*z0ucur(nmd) + kfu(nm)*z0ucur(nm) &
             &  + kfv(ndm)*z0vcur(ndm) + kfv(nm)*z0vcur(nm)  )/kn
       !
       ! Calculate total (possibly wave enhanced) roughness
       !
       z0rou = (  kfu(nmd)*z0urou(nmd) + kfu(nm)*z0urou(nm) &
             &  + kfv(ndm)*z0vrou(ndm) + kfv(nm)*z0vrou(nm)  )/kn
       !
       ! bed shear stress as used in flow, or
       ! skin fiction following Soulsby; "Bed shear stress under
       ! combined waves and currents on rough and smoooth beds"
       ! Estproc report TR137, 2004
       !
       if (bsskin) then
          !
          ! Compute bed stress resulting from skin friction
          !
          call compbsskin   (umean   , vmean     , h1      , wave    , &
                           & uorb(nm), tp  (nm)  , teta(nm), kssilt  , &
                           & kssand  , thcmud(nm), taub    , rhowat(nm,kmax), &
                           & vicmol  )
       else
          !
          ! use max bed shear stress, rather than mean
          !
          taub = taubmx(nm)
       endif
       !
       if (wave) then
          hrmsnm = min(gammax*h1, hrms(nm))
          tpnm   = tp(nm)
          tetanm = teta(nm)
          rlnm   = rlabda(nm)
          uorbnm = uorb(nm)
       else
          hrmsnm = 0.0_fp
          tpnm   = 0.0_fp
          tetanm = 0.0_fp
          rlnm   = 0.0_fp
          uorbnm = 0.0_fp
       endif
       if (lsal > 0) then
          salinity = r0(nm, kmax, lsal)
       else
          salinity = saleqs
       endif
       if (ltem > 0) then
          temperature = r0(nm, kmax, ltem)
       else
          temperature = temeqs
       endif
       !
       d10  = dxx(nm,i10)
       d90  = dxx(nm,i90)
       !
       if (max_integers < 4) then
          write(errmsg,'(a,a,a)') 'Insufficient space to pass integer values to transport routine.'
          call prterr (lundia,'U021', trim(errmsg))
          error = .true.
          return
       endif
       dll_integers( 1) = nm
       dll_integers( 2) = n
       dll_integers( 3) = m
       !
       if (max_strings < 2) then
          write(errmsg,'(a,a,a)') 'Insufficient space to pass strings to transport routine.'
          call prterr (lundia,'U021', trim(errmsg))
          error = .true.
          return
       endif
       dll_strings( 1) = gdp%runid
       !
       do l = 1, lsedtot
          !
          dll_integers( 4) = l
          dll_strings ( 2) = dll_usrfil(l)
          !
          if (sedtyp(l)==SEDTYP_COHESIVE) then
             !
             ! sediment type COHESIVE
             !
             do k = 0, kmax
                wslc(k)   = ws(nm, k, l)
                dcwwlc(k) = dicww(nm, k)
             enddo
             !
             call erosilt(l        ,thick    ,rhowat(nm,kmax)   ,rlnm     ,vicmol     , &
                        & kmax     ,hrmsnm   ,uorbnm  ,tpnm     ,tetanm   ,wslc       , &
                        & wstau(nm),entr(nm) ,dcwwlc  ,sddflc   ,lundia   ,rhosol(l)  , &
                        & nm       ,h0       ,h1      ,z0rou    ,tauadd   ,umean      , &
                        & vmean    ,ubed     ,vbed    ,taub     ,salinity ,temperature, &
                        & error   ,ag       ,vonkar   ,fixfac     , &
                        & frac     ,sinkse   ,sourse  ,oldmudfrac,flmd2l  ,tcrdep     , &
                        & tcrero   ,eropar   ,timsec  ,iform    , &
                        & max_integers,max_reals      ,max_strings  ,dll_function(l),dll_handle(l), &
                        & dll_integers,dll_reals      ,dll_strings  )
             if (error) call d3stop(1, gdp)
             !
             do k = 1, kmax
                seddif(nm, k, l) = sddflc(k)
             enddo 
             kmxsed(nm, l) = kmax
             cycle
          endif
          !
          ! sediment type NONCOHESIVE_SUSPENDED or NONCOHESIVE_TOTALLOAD
          !
          ll = lstart + l
          suspfrac = sedtyp(l)/=SEDTYP_NONCOHESIVE_TOTALLOAD
          !
          ! Calculation for sand or bedload
          !
          ! Reset Prandtl-Schmidt number for sand fractions
          !
          if (suspfrac) then
             sigdif(ll) = 1.0_fp
          endif
          tsd = -999.0_fp
          di50 = sedd50(l)
          if (di50 < 0.0_fp) then
             !
             ! Space varying sedd50 specified in array sedd50fld:
             ! Recalculate dstar, tetacr and taucr for each nm,l - point
             ! This code is copied from inised (uniform sedd50)
             !
             di50     = sedd50fld(nm)
             drho     = (rhosol(l)-rhow) / rhow
             dstar(l) = di50 * (drho*ag/vicmol**2)**0.3333_fp
             if (dstar(l) < 1.0_fp) then
                if (iform(l) == -2) then
                   tetacr(l) = 0.115_fp / (dstar(l)**0.5_fp)
                else
                   tetacr(l) = 0.24_fp / dstar(l)
                endif
             elseif (dstar(l) <= 4.0_fp) then
                if (iform(l) == -2) then
                   tetacr(l) = 0.115_fp / (dstar(l)**0.5_fp)
                else
                   tetacr(l) = 0.24_fp / dstar(l)
                endif
             elseif (dstar(l)>4.0_fp .and. dstar(l)<=10.0_fp) then
                tetacr(l) = 0.14_fp  / (dstar(l)**0.64_fp)
             elseif (dstar(l)>10.0_fp .and. dstar(l)<=20.0_fp) then
                tetacr(l) = 0.04_fp  / (dstar(l)**0.1_fp)
             elseif (dstar(l)>20.0_fp .and. dstar(l)<=150.0_fp) then
                tetacr(l) = 0.013_fp * (dstar(l)**0.29_fp)
             else
                tetacr(l) = 0.055_fp
             endif
             taucr(l) = factcr * (rhosol(l)-rhow) * ag * di50 * tetacr(l)
          endif
          !
          ! SWITCH 2DH/3D SIMULATIONS
          !
          if (kmax > 1) then
             !
             ! 3D CASE
             !
             if (suspfrac) then
                !
                ! Fill local 1dv arrays with fall velocity and
                ! diffusivity
                !
                do k = 0, kmax
                   wslc(k)   = ws(nm, k, l)
                   dcwwlc(k) = dicww(nm, k)
                enddo
                !
                tsigmol = sigmol(ll)
                tdss    = dss(nm, l)
                tsalmax = salmax(l)
                tws0    = ws0(l)
             else
                !
                ! use dummy values for bedload fractions
                !
                tsigmol =  1.0_fp
                tdss    = di50
                tsalmax = 30.0_fp
                tws0    =  0.0_fp
             endif
             !
             do k = 1, kmax
                concin3d(k) = max(0.0_fp , r0(nm,k,ll))
             enddo
             !
             ! Solve equilibrium concentration vertical and
             ! integrate over vertical
             !
             call eqtran(nm          ,l              ,sig          ,thick        ,kmax      , &
                       & h1          ,aks(nm)        ,ustarc       ,wslc         ,ltur      , &
                       & frac(nm,l)  ,tpnm           ,dstar(l)     ,hrmsnm       ,rlnm      , &
                       & di50        ,d90            ,tsigmol      ,rhosol(l)    ,uuu(nm)   , &
                       & vvv(nm)     ,umod(nm)       ,zumod(nm)    ,z0rou        , &
                       & ce_nm       ,taurat(nm,l)   ,dcwwlc       ,sddflc       ,rsdqlc    , &
                       & kmaxsd      ,crep           ,sbcu(nm,l )  ,sbcv(nm,l)   ,sbwu(nm,l), &
                       & sbwv(nm,l)  ,sswu(nm,l)     ,sswv(nm,l)   ,lundia       , &
                       & uorbnm      ,rhowat(nm,kmax),z0cur        ,tetanm       ,taucr(l)  , &
                       & d10         ,taubmx(nm)     ,tdss         ,rksr(nm)     ,3         , &
                       & ce_nmtmp    ,akstmp         ,mudfrac(nm)  ,lsecfl       ,spirint   , &
                       & hidexp(nm,l),suspfrac       ,ust2(nm)     ,tetacr(l)    ,salinity  , &
                       & tsalmax     ,tws0           ,tsd          ,dis(nm)      ,concin3d  , &
                       & dzduu(nm)   ,dzdvv(nm)      ,ubot(nm)     ,temperature  ,tauadd    , &
                       & sus         ,bed            ,susw         ,bedw         ,espir     , &
                       & rhow        ,ag             ,vonkar       ,vicmol       ,wave      , &
                       & scour       ,epspar         ,ubot_from_com,timsec       ,camax     , &
                       & aksfac      ,rwave          ,rdc          ,rdw          ,pangle    , &
                       & fpco        ,iopsus         ,iopkcw       ,subiw        ,eps       , &
                       & iform(l)     ,par(1,l)  , &
                       & max_integers,max_reals      ,max_strings  ,dll_function(l),dll_handle(l), &
                       & dll_integers,dll_reals      ,dll_strings  ,error     )
             if (error) call d3stop(1, gdp)
             if (suspfrac) then
                dss(nm, l) = tdss
                !
                ! Copy results into arrays
                !
                kmxsed(nm, l) = kmaxsd
                do k = 1, kmax
                   seddif(nm, k, l) = sddflc(k)
                   rsedeq(nm, k, l) = rsdqlc(k)
                enddo 
                !
                ! Source and sink terms for main 3d computation
                ! note: terms are part explicit, part implicit, see
                ! thesis of Giles Lesser, May 2000
                !
                thick0        = thick(kmaxsd) * h0
                thick1        = thick(kmaxsd) * h1
                call soursin_3d  (h1                ,thick0         ,thick1             , &
                               &  sig(kmaxsd)       ,thick(kmaxsd)  ,r0(nm,kmaxsd,ll)   , &
                               &  vicmol            ,sigmol(ll)     ,seddif(nm,kmaxsd,l), &
                               &  rhosol(l)         ,ce_nmtmp       ,ws(nm,kmaxsd,l)    , &
                               &  akstmp            ,sourse(nm,l)   ,sinkse(nm,l) )
                ! Impose relatively large vertical diffusion
                ! coefficients for sediment in layer interfaces from
                ! bottom of reference cell downwards, to ensure little
                ! gradient in sed. conc. exists in this area.
                !
                difbot = 10.0_fp * ws(nm,kmaxsd,l) * thick1
                do k = kmaxsd, kmax
                   seddif(nm, k, l) = difbot
                enddo
             endif ! suspfrac
          else
             !
             ! kmax = 1
             ! 2D CASE (Numerical approximation)
             !
             if (suspfrac) then
                !
                ! Fill local 1dv arrays with fall velocity and
                ! diffusivity
                !
                do k2d = 0, kmax2d
                   ws2d(k2d)   = ws(nm, 1, l)
                   dcww2d(k2d) = 0.0_fp
                enddo
                !
                tsigmol = sigmol(ll)
                tdss    = dss(nm, l)
                trsedeq = rsedeq(nm, 1, l)
                tsalmax = salmax(l)
                tws0    = ws0(l)
             else
                tsigmol =  1.0_fp
                tdss    = di50
                trsedeq =  0.0_fp
                tsalmax = 30.0_fp
                tws0    =  0.0_fp
            endif
             !
             if (lsecfl > 0) then
                spirint = r0(nm,1,lsecfl)
             else
                spirint = 0.0_fp
             endif
             !
             ! Solve equilibrium concentration vertical and
             ! integrate over vertical; compute bedload
             ! transport excluding slope effects.
             !
             call eqtran(nm          ,l              ,sig2d        ,thck2d       ,kmax2d     , &
                       & h1          ,aks(nm)        ,ustarc       ,ws2d         ,ltur       , &
                       & frac(nm,l)  ,tpnm           ,dstar(l)     ,hrmsnm       ,rlnm       , &
                       & di50        ,d90            ,tsigmol      ,rhosol(l)    ,uuu(nm)    , &
                       & vvv(nm)     ,umod(nm)       ,zumod(nm)    ,z0rou        , &
                       & ce_nm       ,taurat(nm,l)   ,dcww2d       ,sddf2d       ,rsdq2d     , &
                       & kmaxsd      ,trsedeq        ,sbcu(nm,l)   ,sbcv(nm,l)   ,sbwu(nm,l) , &
                       & sbwv(nm,l)  ,sswu(nm,l)     ,sswv(nm,l)   ,lundia       , &
                       & uorbnm      ,rhowat(nm,kmax),z0cur        ,tetanm       ,taucr(l)   , &
                       & d10         ,taubmx(nm)     ,tdss         ,rksr(nm)     ,2          , &
                       & ce_nmtmp    ,akstmp         ,mudfrac(nm)  ,lsecfl       ,spirint    , &
                       & hidexp(nm,l),suspfrac       ,ust2(nm)     ,tetacr(l)    ,salinity   , &
                       & tsalmax     ,tws0           ,tsd          ,dis(nm)      ,concin2d   , &
                       & dzduu(nm)   ,dzdvv(nm)      ,ubot(nm)     ,temperature  ,tauadd     , &
                       & sus         ,bed            ,susw         ,bedw         ,espir      , &
                       & rhow        ,ag             ,vonkar       ,vicmol       ,wave       , &
                       & scour       ,epspar         ,ubot_from_com,timsec       ,camax      , &
                       & aksfac      ,rwave          ,rdc          ,rdw          ,pangle     , &
                       & fpco        ,iopsus         ,iopkcw       ,subiw        ,eps        , &
                       & iform(l)     ,par(1,l)   , &
                       & max_integers,max_reals      ,max_strings  ,dll_function(l),dll_handle(l), &
                       & dll_integers,dll_reals      ,dll_strings  ,error      )
             if (error) call d3stop(1, gdp)
             if (suspfrac) then
                dss   (nm, l)    = tdss
                rsedeq(nm, 1, l) = trsedeq
                kmxsed(nm, l)    = 1
                !
                ! Galappatti time scale and source and sink terms
                !
                call soursin_2d(umod(nm)      ,ustarc        ,h0            ,h1        , &
                              & ws(nm,1,l)    ,tsd           ,rsedeq(nm,1,l),            &
                              & sourse(nm,l)  ,sinkse(nm,l)  ,gdp                      )
             endif ! suspfrac
          endif ! kmax = 1
          if (suspfrac) then
             rca(nm, l) = ce_nm * rhosol(l)
          endif
       enddo ! next sediment fraction
    enddo ! next nm point
    !
    ! Reduce the source and sink terms to avoid large bed level changes
    ! Note: previous implementation forgot to multiply source/
    !       sink terms with the thickness for the 2Dh case
    !
    call   red_soursin (nmmax     ,kmax      ,thick     ,kmxsed    , &
                      & lsal      ,ltem      ,lsed      ,lsedtot   , &
                      & dps       ,s0        ,s1        ,r0        , &
                      & rsedeq    ,nst       , &
                      & gdp       )
    !
    ! Fill sutot and svtot
    !
    do l = 1,lsedtot
       call dfexchg( sbcu(:,l) ,1, 1, dfloat, gdp)
       call dfexchg( sbwu(:,l) ,1, 1, dfloat, gdp)
       call dfexchg( sswu(:,l) ,1, 1, dfloat, gdp)
       call dfexchg( sbcv(:,l) ,1, 1, dfloat, gdp)
       call dfexchg( sbwv(:,l) ,1, 1, dfloat, gdp)
       call dfexchg( sswv(:,l) ,1, 1, dfloat, gdp)
       if (sedtyp(l)/=SEDTYP_COHESIVE) then
          do nm = 1, nmmax
             sutot(nm, l) = sbcu(nm, l) + sbwu(nm, l) + sswu(nm, l)
             svtot(nm, l) = sbcv(nm, l) + sbwv(nm, l) + sswv(nm, l)
          enddo
       endif
    enddo
    !
    ! Upwind scheme for bed load and wave driven transport
    ! Convert sand bed load transport to U and V points using upwind scheme
    !
    if (bed > 0.0_fp) then
       !
       ! Upwind bed load transport
       !
       call upwbed(sbcu      ,sbcv      ,sbcuu     ,sbcvv     ,kfu       , &
                 & kfv       ,kcs       ,kfsed     ,lsedtot   , &
                 & nmmax     ,icx       ,icy       ,sutot     ,svtot     , &
                 & gdp       )
    endif
    !
    if (bedw>0.0_fp .and. wave) then
       !
       ! Upwind wave-related bed load load transports
       !
       call upwbed(sbwu      ,sbwv      ,sbwuu     ,sbwvv     ,kfu       , &
                 & kfv       ,kcs       ,kfsed     ,lsedtot   , &
                 & nmmax     ,icx       ,icy       ,sutot     ,svtot     , &
                 & gdp       )
    endif
    !
    if (susw>0.0_fp .and. wave) then
       !
       ! Upwind wave-related suspended load transports
       !
       call upwbed(sswu      ,sswv      ,sswuu     ,sswvv     ,kfu       , &
                 & kfv       ,kcs       ,kfsed     ,lsedtot   , &
                 & nmmax     ,icx       ,icy       ,sutot     ,svtot     , &
                 & gdp       )
    endif
    !

    !
    ! Bed-slope and sediment availability effects for
    ! current-related bed load transport
    !
    if (bed > 0.0_fp) then
       call adjust_bedload(nmmax     ,icx       ,icy       ,kcs       , &
              & kcu       ,kcv       ,kfu       ,kfv       ,lsedtot   , &
              & sbcuu     ,sbcvv     ,sbuut     ,sbvvt     ,dzduu     , &
              & dzdvv     ,taurat    ,frac      ,fixfac    ,ust2      , &
              & hu        ,hv        ,dm        ,hidexp    ,.true.    , &
              & gdp       )
    endif
    !
    ! Bed-slope and sediment availability effects for
    ! wave-related bed load transport
    !
    if (bedw>0.0_fp .and. wave) then
       call adjust_bedload(nmmax     ,icx       ,icy       ,kcs       , &
              & kcu       ,kcv       ,kfu       ,kfv       ,lsedtot   , &
              & sbwuu     ,sbwvv     ,sbuut     ,sbvvt     ,dzduu     , &
              & dzdvv     ,taurat    ,frac      ,fixfac    ,ust2      , &
              & hu        ,hv        ,dm        ,hidexp    ,.true.    , &
              & gdp       )
    endif
    !
    ! Sediment availability effects for
    ! wave-related suspended load transport
    !
    if (susw>0.0_fp .and. wave) then
       call adjust_bedload(nmmax     ,icx       ,icy       ,kcs       , &
              & kcu       ,kcv       ,kfu       ,kfv       ,lsedtot   , &
              & sswuu     ,sswvv     ,sbuut     ,sbvvt     ,dzduu     , &
              & dzdvv     ,taurat    ,frac      ,fixfac    ,ust2      , &
              & hu        ,hv        ,dm        ,hidexp    ,.false.   , &
              & gdp       )
    endif
    !
    ! Summation of current-related and wave-related transports
    !
    do l = 1,lsedtot
       if (sedtyp(l)/=SEDTYP_COHESIVE) then
          do nm = 1, nmmax
             sbuu(nm, l) = sbcuu(nm, l) + sbwuu(nm, l) + sswuu(nm, l)
             sbvv(nm, l) = sbcvv(nm, l) + sbwvv(nm, l) + sswvv(nm, l)
          enddo
       endif
    enddo
    !
    ! Finally fill sour and sink arrays for both sand and silt
    ! note that sourse and sinkse arrays are required for BOTT3D
    !
    do l = 1, lsed
       ll = lstart + l
       do nm = 1, nmmax
          k = kmxsed(nm,l)
          sour(nm, k, ll) = sour(nm, k, ll) + sourse(nm, l)
          sink(nm, k, ll) = sink(nm, k, ll) + sinkse(nm, l)
       enddo
       call dfexchg( sour(:,:,l),1, kmax, dfloat, gdp)
       call dfexchg( sink(:,:,l),1, kmax, dfloat, gdp)
    enddo
    !
    ! DD-Mapper: copy sbuu and sbvv
    !
    nhystp = nxtstp(d3dflow_sediment, gdp)
end subroutine erosed
