!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2018.                                
!                                                                               
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

!  $Id: fm_erosed.f90 52266 2017-09-02 11:24:11Z klecz_ml $
!  $HeadURL: https://repos.deltares.nl/repos/ds/branches/dflowfm/20161017_dflowfm_codecleanup/engines_gpl/dflowfm/packages/dflowfm_kernel/src/fm_erosed.f90 $

subroutine fm_erosed()
!!                   (nmmax     ,kmax      ,icx       ,icy       ,lundia    , &
!!                  & nst       ,lsed      ,lsedtot   ,lsal      ,ltem      , &
!!                  & lsecfl    ,kfs       ,kfu       ,kfv       ,dzs1      , &
!!                  & r0        ,u0eul     ,v0eul     ,s0        ,dps       , &
!!                  & z0urou    ,z0vrou    ,sour      ,sink      ,rhowat    , &
!!                  & ws        ,rsedeq    ,z0ucur    ,z0vcur    ,sigmol    , &
!!                  & taubmx    ,s1        ,uorb      ,tp        ,sigdif    , &
!!                  & lstsci    ,thick     ,dicww     ,kcs       , &
!!                  & kcu       ,kcv       ,guv       ,gvu       ,sbuu      , &
!!                  & sbvv      ,seddif    ,hrms      ,ltur      , &
!!                  & teta      ,rlabda    ,aks       ,saleqs    , &
!!                  & sbuut     ,sbvvt     ,entr      ,wstau     ,hu        , &
!!                  & hv        ,rca       ,dss       ,ubot      ,rtur0     , &
!!                  & temeqs    ,gsqs      ,guu       ,gvv       ,kfsmin    , &
!!                  & kfsmax    ,dzs0      ,kfumin    ,kfumax    ,kfvmin    , &
!!                  & kfvmax    ,dzu1      ,dzv1      ,gdp       )
!!!!--description-----------------------------------------------------------------
!!!
!!!    Function: Computes sediment fluxes at the bed using
!!!              the Partheniades-Krone formulations.
!!!              Arrays SOURSE and SINKSE are filled and added
!!!              to arrays SOUR and SINK
!!!              Computes bed load transport for sand sediment
!!!              Arrays SBUU and SBVV are filled.
!!!              Computes vertical sediment diffusion coefficient
!!!              Array SEDDIF is filled
!!!              Includes wave asymmetry effects on sand bed-load
!!!              transport
!!!              Bed slope effects computed at the U and V velocity
!!!              points
!!! Method used: Attention: pointer ll for 'standard' FLOW
!!!              arrays is shifted with lstart
!!!
!!!

!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts, only: pi
    use bedcomposition_module
    use morphology_data_module
    use sediment_basics_module
    use m_physcoef, only: ag, vonkar, sag, ee
    use m_sediment, only: stmpar, sedtra, stm_included, mtd
    use m_flowgeom, only: ndxi, bl, kfs, lnxi, lnx, ln, dxi, Ndx  !, csu, snu
    use m_flow, only: s0, s1, ucx, ucy, kbot, ktop, kmx, kmxn, plotlin
    use m_flowtimes, only: julrefdat, dts, time1
    use unstruc_files, only: mdia
    use message_module, only: write_error
    use m_transport, only: ised1
    use dfparall
    use m_alloc
    use m_missing
    use m_physcoef, only: frcuni, ifrctypuni
    use m_turbulence, only: vicwws
    !
    implicit none
    !
    logical, pointer :: wave
    real(fp) :: eps = 1.0e-6_fp
!     real(fp) :: ag from m_physcoef
    real(fp) :: vicmol = 1.3e-6_fp
    real(fp) :: gammax = 1.0_fp
    logical :: scour = .false.
    real(fp)         , dimension(:)      , pointer :: rksr
!     real(fp) :: vonkar from m_physcoef
!!    real(fp)                             , pointer :: timsec
!!    real(fp)                             , pointer :: timhr
!!    integer                              , pointer :: julday
    logical :: ubot_from_com = .true. !! promoted approach, so only option in FM
    ! sedpar
    integer                              , pointer :: nmudfrac
    real(fp)         , dimension(:)      , pointer :: rhosol
    real(fp)         , dimension(:)      , pointer :: cdryb
    real(fp)         , dimension(:,:,:)  , pointer :: logseddia
    real(fp)         , dimension(:)      , pointer :: logsedsig
    real(fp)         , dimension(:)      , pointer :: sedd10
    real(fp)         , dimension(:)      , pointer :: sedd50
    real(fp)         , dimension(:)      , pointer :: sedd90
    real(fp)         , dimension(:)      , pointer :: sedd50fld
    real(fp)         , dimension(:)      , pointer :: dstar
    real(fp)         , dimension(:)      , pointer :: taucr
    real(fp)         , dimension(:)      , pointer :: tetacr
    real(fp)         , dimension(:)      , pointer :: mudcnt
    real(fp)         , dimension(:)      , pointer :: pmcrit
    integer          , dimension(:)      , pointer :: nseddia
    integer          , dimension(:)      , pointer :: sedtyp
    logical                              , pointer :: anymud
    real(fp)         , dimension(:)      , pointer :: sedtrcfac
    logical                              , pointer :: bsskin
    real(fp)         , dimension(:)      , pointer :: thcmud
    real(fp)                             , pointer :: kssilt
    real(fp)                             , pointer :: kssand
    ! morpar
    real(fp)                             , pointer :: thresh
    real(fp)                             , pointer :: sus
    real(fp)                             , pointer :: bed
    real(fp)                             , pointer :: susw
    real(fp)                             , pointer :: sedthr
    real(fp)                             , pointer :: bedw
    integer                              , pointer :: i10
    integer                              , pointer :: i50
    integer                              , pointer :: i90
    integer                              , pointer :: nxx
    real(fp)         , dimension(:)      , pointer :: xx
    real(fp)                             , pointer :: morfac
    logical                              , pointer :: varyingmorfac
    logical                              , pointer :: multi
    real(fp)                             , pointer :: factcr
    integer                              , pointer :: ihidexp
    real(fp)                             , pointer :: asklhe
    real(fp)                             , pointer :: mwwjhe
    real(fp)                             , pointer :: ffthresh
    real(fp)                             , pointer :: espir
    logical                              , pointer :: epspar
    real(fp)                             , pointer :: camax
    real(fp)                             , pointer :: aksfac
    real(fp)                             , pointer :: rdc
    integer                              , pointer :: iopkcw
    logical                              , pointer :: oldmudfrac
    integer                              , pointer :: iflufflyr
    real(fp)         , dimension(:,:)    , pointer :: sinkf
    real(fp)         , dimension(:,:)    , pointer :: sourf
    real(fp)         , dimension(:,:)    , pointer :: depfac
    real(fp)         , dimension(:,:)    , pointer :: mfluff
    ! trapar
    integer          , dimension(:)      , pointer :: iform
    real(fp)         , dimension(:,:)    , pointer :: par
    integer                              , pointer :: max_integers
    integer                              , pointer :: max_reals
    integer                              , pointer :: max_strings
    character(256)   , dimension(:)      , pointer :: dll_function
    integer(pntrsize), dimension(:)      , pointer :: dll_handle
    integer          , dimension(:)      , pointer :: dll_integers
    real(hp)         , dimension(:)      , pointer :: dll_reals
    character(256)   , dimension(:)      , pointer :: dll_strings
    character(256)   , dimension(:)      , pointer :: dll_usrfil
    ! sedtra
    real(fp)         , dimension(:)      , pointer :: bc_mor_array
    real(fp)         , dimension(:,:)    , pointer :: dbodsd
    real(fp)         , dimension(:)      , pointer :: dcwwlc
    real(fp)         , dimension(:)      , pointer :: dm
    real(fp)         , dimension(:)      , pointer :: dg
    real(fp)         , dimension(:)      , pointer :: dgsd
    real(fp)         , dimension(:,:)    , pointer :: dxx
    real(fp)         , dimension(:)      , pointer :: e_dzdn
    real(fp)         , dimension(:)      , pointer :: e_dzdt
    real(fp)         , dimension(:)      , pointer :: epsclc
    real(fp)         , dimension(:)      , pointer :: epswlc
    real(fp)         , dimension(:,:)    , pointer :: fixfac
    real(fp)         , dimension(:,:)    , pointer :: frac
    integer          , dimension(:)      , pointer :: kfsed
    integer          , dimension(:,:)    , pointer :: kmxsed
    real(fp)         , dimension(:)      , pointer :: mudfrac
    real(fp)         , dimension(:)      , pointer :: sandfrac
    real(fp)         , dimension(:,:)    , pointer :: hidexp
    real(fp)         , dimension(:)      , pointer :: rsdqlc
    real(fp)         , dimension(:,:)    , pointer :: sbcx
    real(fp)         , dimension(:,:)    , pointer :: sbcy
    real(fp)         , dimension(:,:)    , pointer :: e_sbcn
    real(fp)         , dimension(:,:)    , pointer :: e_sbct
    real(fp)         , dimension(:,:)    , pointer :: sbwx
    real(fp)         , dimension(:,:)    , pointer :: sbwy
    real(fp)         , dimension(:,:)    , pointer :: e_sbwn
    real(fp)         , dimension(:,:)    , pointer :: e_sbwt
    real(fp)         , dimension(:)      , pointer :: sddflc
    real(fp)         , dimension(:,:)    , pointer :: srcmax
    real(fp)         , dimension(:,:)    , pointer :: sswx
    real(fp)         , dimension(:,:)    , pointer :: sswy
    real(fp)         , dimension(:,:)    , pointer :: e_sswn
    real(fp)         , dimension(:,:)    , pointer :: e_sswt
    real(fp)         , dimension(:,:)    , pointer :: sxtot
    real(fp)         , dimension(:,:)    , pointer :: sytot
    real(fp)         , dimension(:,:)    , pointer :: sinkse
    real(fp)         , dimension(:,:)    , pointer :: sourse
    real(fp)         , dimension(:,:)    , pointer :: sour_im
    real(fp)         , dimension(:,:)    , pointer :: taurat
    real(fp)         , dimension(:)      , pointer :: ust2
    real(fp)         , dimension(:)      , pointer :: umod
    real(fp)         , dimension(:)      , pointer :: uuu
    real(fp)         , dimension(:)      , pointer :: vvv
    real(fp)         , dimension(:)      , pointer :: wslc
    real(fp)         , dimension(:)      , pointer :: zumod
    !
    integer                              , pointer :: iunderlyr
    real(prec)       , dimension(:,:)    , pointer :: bodsed 
!!    include 'flow_steps_f.inc'
!
! Local parameters
!
    integer, parameter :: kmax2d = 20
!
! Global variables
!
!!    integer                                                   , intent(in)  :: icx     !  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
!!    integer                                                   , intent(in)  :: icy     !  Increment in the Y-dir. (see ICX)
!!    integer                                                   , intent(in)  :: kmax    !  Description and declaration in esm_alloc_int.f90
!!    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmin  !  Description and declaration in iidim.f90
!!    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmax  !  Description and declaration in iidim.f90
!!    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfumin  !  Description and declaration in iidim.f90
!!    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfumax  !  Description and declaration in iidim.f90
!!    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfvmin  !  Description and declaration in iidim.f90
!!    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfvmax  !  Description and declaration in iidim.f90
!!    integer                                                   , intent(in)  :: lsal    !  Description and declaration in dimens.igs
    integer, pointer :: lsed
    integer, pointer :: lsedtot
!!    integer                                                   , intent(in)  :: lstsci  !  Description and declaration in esm_alloc_int.f90
!!    integer                                                   , intent(in)  :: ltem    !  Description and declaration in dimens.igs
    integer  :: lsecfl  = 0
    integer  :: ltur    = 0
!!    integer                                                                 :: lundia  !  Description and declaration in inout.igs
!!    integer                                                   , intent(in)  :: nmmax   !  Description and declaration in dimens.igs
!!    integer                                                   , intent(in)  :: nst
!!    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcs     !  Description and declaration in esm_alloc_int.f90
!!    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcu     !  Description and declaration in esm_alloc_int.f90
!!    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcv     !  Description and declaration in esm_alloc_int.f90
!!    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfs     !  Description and declaration in esm_alloc_int.f90
!!    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfu     !  Description and declaration in esm_alloc_int.f90
!!    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfv     !  Description and declaration in esm_alloc_int.f90
!!    real(fp)                                                  , intent(in)  :: dt
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, lsed)                      :: aks     !  Description and declaration in esm_alloc_real.f90
!!    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps     !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: entr    !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: guv     !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gvu     !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gsqs    !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: guu     !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gvv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(:), pointer :: hrms
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: hu      !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: hv      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(:), pointer :: rlabda
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, ltur), intent(in)  :: rtur0   !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s0      !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s1      !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: sbuut
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: sbvvt
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: taubmx  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(:), pointer :: teta
    real(fp), dimension(:), pointer :: tp
    real(fp), dimension(:), pointer :: uorb
    real(fp), dimension(:), pointer :: ubot
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: wstau   !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0ucur
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0urou  !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0vcur
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0vrou  !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: dicww   !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, *)   , intent(in)  :: ws      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(:,:), pointer :: seddif
    real(fp)  , dimension(:), pointer :: rhowat
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: u0eul   !  EULARIAN U-velocities at old time level
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: v0eul   !  EULARIAN V-velocities at old time level
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, *)                   :: r0      !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, *)                   :: sink    !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, *)                   :: sour    !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lsed)                :: rsedeq  !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, lsed)                      :: dss     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(:,:), pointer :: caksrho
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot)                   :: sbuu    !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot)        
!!    real(fp)  , dimension(kmax)                               , intent(in)  :: thick   !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(lstsci)                             , intent(out) :: sigdif  !  Description and declaration in esm_alloc_real.f90
!!    real(fp)  , dimension(lstsci)                                           :: sigmol  !  Description and declaration in esm_alloc_real.f90
!!    real(fp)                                                  , intent(in)  :: saleqs
!!    real(fp)                                                  , intent(in)  :: temeqs
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzs1   !  Description and declaration in rjdim.f90 
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzs0   !  Description and declaration in rjdim.f90 
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzu1   !  Description and declaration in rjdim.f90 
!!    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzv1   !  Description and declaration in rjdim.f90 
!
! Local variables
!
    integer                       :: i
    integer                       :: iln
    integer                       :: istat
    integer                       :: j
    integer                       :: k
    integer                       :: k2d
    integer                       :: kbed
    integer                       :: kmaxsd
!!    integer                       :: kn
!!    integer                       :: ku
!!    integer                       :: kv
    integer                       :: l
!!    integer                       :: ll
    integer                       :: lstart
!!    integer                       :: m
!!    integer                       :: n
!!    integer                       :: ndm
!!    integer                       :: nhystp
    integer                       :: nm
!!    integer                       :: nmd
!!    integer                       :: nm_pos    ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!!    integer                       :: nmu
!!    integer                       :: num
    logical                       :: error
    integer                       :: klc
    integer                       :: kmaxlc    
    logical                       :: suspfrac  ! suspended component sedtyp(l)/=SEDTYP_NONCOHESIVE_TOTALLOAD
    real(fp)                      :: aks_ss3d
    real(fp)                      :: caks
    real(fp)                      :: caks_ss3d
    real(fp)                      :: chezy
!!    real(fp)                      :: conc2d
    real(fp)                      :: delr
    real(fp)                      :: di50
!!    real(fp)                      :: difbot
    real(fp)                      :: drho
!!    real(fp)                      :: dstari
    real(fp)                      :: dtmor
    real(fp)                      :: dzdx
    real(fp)                      :: dzdy
!!    real(fp)                      :: fi
    real(fp)                      :: fracf
    real(fp)                      :: grkg
    real(fp)                      :: grm2
    real(fp)                      :: grlyrs
    real(fp)                      :: h0
    real(fp)                      :: h1
    real(fp)                      :: rc
    real(fp)                      :: mfltot
    real(fp)                      :: salinity
!!    real(fp)                      :: sinkfluff
    real(fp)                      :: sinktot
    real(fp)                      :: sourfluff
    real(fp)                      :: spirint   ! local variable for spiral flow intensity r0(nm,1,lsecfl)
    real(fp)                      :: taks
    real(fp)                      :: taks0
    real(fp)                      :: tauadd
    real(fp)                      :: taub
    real(fp)                      :: tauc
    real(fp)                      :: tdss      ! temporary variable for dss
    real(fp)                      :: temperature
    real(fp), dimension(kmx+1)    :: thicklc 
    real(fp)                      :: thick0
    real(fp)                      :: thick1
    real(fp)                      :: trsedeq   ! temporary variable for rsedeq
    real(fp)                      :: tsd
    real(fp)                      :: tsigmol   ! temporary variable for sigmol
    real(fp)                      :: twsk
    real(fp)                      :: u
    real(fp)                      :: ubed
    real(fp)                      :: umean
    real(fp)                      :: ustarc
    real(fp)                      :: utot
    real(fp)                      :: v
    real(fp)                      :: vbed
    real(fp)                      :: velb
    real(fp)                      :: velm
    real(fp)                      :: vmean
    real(fp)                      :: z0cur
    real(fp)                      :: z0rou
    real(fp)                      :: zvelb
    real(fp), dimension(:), allocatable :: E            ! erosion velocity [m/s]
    real(fp), dimension(0:kmax2d) :: dcww2d
    real(fp), dimension(0:kmax2d) :: sddf2d
    real(fp), dimension(0:kmax2d) :: ws2d
    real(fp), dimension(kmax2d)   :: rsdq2d
    real(fp), dimension(kmax2d), save :: sig2d = &
       (/ -0.0874, -0.2472, -0.3797, -0.4897, -0.5809, -0.6565, -0.7193, &
        & -0.7713, -0.8145, -0.8503, -0.8800, -0.9046, -0.9250, -0.9419, -0.9560,&
        & -0.9676, -0.9773, -0.9854, -0.9920, -0.9975 /)

    real(fp), dimension(kmax2d), save :: thck2d = &
       (/ 0.1747, 0.1449, 0.1202, 0.0997, 0.0827, 0.0686, 0.0569, 0.0472, &
        & 0.0391, 0.0325, 0.0269, 0.0223, 0.0185, 0.0154, 0.0127, 0.0106, 0.0088,&
        & 0.0073, 0.0060, 0.0050 /)
!!    real(fp), dimension(kmax)     :: concin3d
    real(fp), dimension(kmax2d)   :: concin2d
    character(256)                :: errmsg    
    double precision              :: cz_dum
    
    !
!
!! executable statements -------------------------------------------------------
!
!   exit the routine immediately if sediment transport (and morphology) is NOT included in the simulation
!
! test 3 useful feature from trunk
    error = .false.
    if (.not.stm_included) return
    wave                => mtd%have_waves
!!    vicmol              => gdp%gdphysco%vicmol
!!    gammax              => gdp%gdnumeco%gammax
!!    eps                 => gdp%gdconst%eps
!!    scour               => gdp%gdscour%scour
    hrms                => mtd%hrms
    tp                  => mtd%tp
    teta                => mtd%teta
    rlabda              => mtd%rlabda
    uorb                => mtd%uorb
    ubot                => mtd%ubot
    rksr                => mtd%rksr
    rhowat              => mtd%rhowat
    seddif              => mtd%seddif
    caksrho             => mtd%caksrho
    
!!    timsec              => gdp%gdinttim%timsec
!!    timhr               => gdp%gdinttim%timhr
!!    julday              => gdp%gdinttim%julday
!!    ubot_from_com       => gdp%gdprocs%ubot_from_com
!!    flmd2l              => gdp%gdprocs%flmd2l
    ! stmpar
    lsed                => stmpar%lsedsus
    lsedtot             => stmpar%lsedtot
    ! sedpar
    nmudfrac            => stmpar%sedpar%nmudfrac
    rhosol              => stmpar%sedpar%rhosol
    cdryb               => stmpar%sedpar%cdryb
    logseddia           => stmpar%sedpar%logseddia
    logsedsig           => stmpar%sedpar%logsedsig
    sedd10              => stmpar%sedpar%sedd10
    sedd50              => stmpar%sedpar%sedd50
    sedd90              => stmpar%sedpar%sedd90
    sedd50fld           => stmpar%sedpar%sedd50fld
    dstar               => stmpar%sedpar%dstar
    taucr               => stmpar%sedpar%taucr
    tetacr              => stmpar%sedpar%tetacr
    mudcnt              => stmpar%sedpar%mudcnt
    pmcrit              => stmpar%sedpar%pmcrit
    nseddia             => stmpar%sedpar%nseddia
    sedtyp              => stmpar%sedpar%sedtyp
    anymud              => stmpar%sedpar%anymud
    sedtrcfac           => stmpar%sedpar%sedtrcfac
    bsskin              => stmpar%sedpar%bsskin
    thcmud              => stmpar%sedpar%thcmud
    kssilt              => stmpar%sedpar%kssilt
    kssand              => stmpar%sedpar%kssand
    ! morpar
    thresh              => stmpar%morpar%thresh
    sus                 => stmpar%morpar%sus
    bed                 => stmpar%morpar%bed
    susw                => stmpar%morpar%susw
    sedthr              => stmpar%morpar%sedthr
    bedw                => stmpar%morpar%bedw
    i10                 => stmpar%morpar%i10
    i50                 => stmpar%morpar%i50
    i90                 => stmpar%morpar%i90
    nxx                 => stmpar%morpar%nxx
    xx                  => stmpar%morpar%xx
    multi               => stmpar%morpar%multi
    factcr              => stmpar%morpar%factcr
    ihidexp             => stmpar%morpar%ihidexp
    asklhe              => stmpar%morpar%asklhe
    mwwjhe              => stmpar%morpar%mwwjhe
    ffthresh            => stmpar%morpar%thresh
    morfac              => stmpar%morpar%morfac
    varyingmorfac       => stmpar%morpar%varyingmorfac
    espir               => stmpar%morpar%espir
    epspar              => stmpar%morpar%epspar 
    camax               => stmpar%morpar%camax
    aksfac              => stmpar%morpar%aksfac
    rdc                 => stmpar%morpar%rdc
    iopkcw              => stmpar%morpar%iopkcw
    oldmudfrac          => stmpar%morpar%oldmudfrac
    sinkf               => stmpar%morpar%flufflyr%sinkf
    sourf               => stmpar%morpar%flufflyr%sourf
    iflufflyr           => stmpar%morpar%flufflyr%iflufflyr
    depfac              => stmpar%morpar%flufflyr%depfac
    mfluff              => stmpar%morpar%flufflyr%mfluff
    ! trapar
    iform               => stmpar%trapar%iform
    par                 => stmpar%trapar%par
    max_integers        => stmpar%trapar%max_integers
    max_reals           => stmpar%trapar%max_reals
    max_strings         => stmpar%trapar%max_strings
    dll_function        => stmpar%trapar%dll_function
    dll_handle          => stmpar%trapar%dll_handle
    dll_integers        => stmpar%trapar%dll_integers
    dll_reals           => stmpar%trapar%dll_reals
    dll_strings         => stmpar%trapar%dll_strings
    dll_usrfil          => stmpar%trapar%dll_usrfil
    ! sedtra
    bc_mor_array        => sedtra%bc_mor_array
    dbodsd              => sedtra%dbodsd
    dcwwlc              => sedtra%dcwwlc
    dm                  => sedtra%dm
    dg                  => sedtra%dg
    dgsd                => sedtra%dgsd
    dxx                 => sedtra%dxx
    e_dzdn              => sedtra%e_dzdn
    e_dzdt              => sedtra%e_dzdt
    epsclc              => sedtra%epsclc
    epswlc              => sedtra%epswlc
    fixfac              => sedtra%fixfac
    frac                => sedtra%frac
    kfsed               => sedtra%kfsed
    kmxsed              => sedtra%kmxsed
    mudfrac             => sedtra%mudfrac
    sandfrac            => sedtra%sandfrac
    hidexp              => sedtra%hidexp
    rsdqlc              => sedtra%rsdqlc
    sbcx                => sedtra%sbcx
    sbcy                => sedtra%sbcy
    e_sbcn              => sedtra%e_sbcn
    e_sbct              => sedtra%e_sbct
    sbwx                => sedtra%sbwx
    sbwy                => sedtra%sbwy
    e_sbwn              => sedtra%e_sbwn
    e_sbwt              => sedtra%e_sbwt
    sddflc              => sedtra%sddflc
    sswx                => sedtra%sswx
    sswy                => sedtra%sswy
    e_sswn              => sedtra%e_sswn
    e_sswt              => sedtra%e_sswt
    sxtot               => sedtra%sxtot
    sytot               => sedtra%sytot
    sinkse              => sedtra%sinkse
    sourse              => sedtra%sourse
    sour_im             => sedtra%sour_im
    srcmax              => sedtra%srcmax
    taurat              => sedtra%taurat
    ust2                => sedtra%ust2
    umod                => sedtra%umod
    uuu                 => sedtra%uuu
    vvv                 => sedtra%vvv
    wslc                => sedtra%wslc
    zumod               => sedtra%zumod
    !
    if (varyingmorfac) then
       call updmorfac(stmpar%morpar, time1/3600.0_fp, julrefdat)
    endif
!!    !
!!    nm_pos =  1
    if (scour) then
       !
       ! Second parameter is zero: save taubmx(*) in gdp%gdscour
       !
!!       call shearx(taubmx, 0, gdp)
    endif
    !
    ! Determine total thickness of the mud layers
    ! to be used in computation of skin friction (Soulsby 2004)
    !
    if (bsskin) then
       call detthcmud(stmpar%morlyr, thcmud)
    endif
    !
    ! Initialisation:
    ! reset sediment sources and sinks
    !     set default kmxsed layer
    !     set kfsed
    !
    lstart = ised1 - 1 ! = max(isalt, itemp)
    !
    ! Reset Sourse and Sinkse arrays for all (l,nm)
    !
    sinkse  = 0.0_fp
    sourse  = 0.0_fp
    sour_im = 0.0_fp
    ! source and sink terms fluff layer
    if (iflufflyr>0) then
        sinkf = 0.0_fp
        sourf = 0.0_fp
    endif
    !
    ! Reset Sediment diffusion arrays for (l,nmk)
    !
    seddif  = 0.0_fp
    caksrho = 0.0_fp
    !
    ! Reset Bed Shear Ratio for all nm and l = 1:lsedtot
    !                        
    taurat = 0.0_fp
    !
    ! Set zero bedload transport for all nm and l = 1:lsedtot
    !
!!    sbuu   = 0.0_fp
!!    sbvv   = 0.
    
    sbcx   = 0.0_fp
    sbcy   = 0.0_fp
    e_sbcn = 0.0_fp
    e_sbct = 0.0_fp
    sbwx   = 0.0_fp
    sbwy   = 0.0_fp
    e_sbwn = 0.0_fp
    e_sbwt = 0.0_fp
    sswx   = 0.0_fp
    sswy   = 0.0_fp
    e_sswn = 0.0_fp
    e_sswt = 0.0_fp
    sxtot  = 0.0_fp
    sytot  = 0.0_fp
!!    !
!!    call dfexchg( dps,1, 1, dfloat, nm_pos, gdp)
!!    !
    do nm = 1, ndxi
       if ((s1(nm) - bl(nm))*kfs(nm) > sedthr) then
          kfsed(nm) = 1
       else
          kfsed(nm) = 0
       endif
    enddo
!!    !
!!    call dfexchg( kfsed,1, 1, dfint, nm_pos, gdp)
    !
    ! Determine fractions of all sediments the top layer and
    ! compute the mud fraction.
    !
    if (lsedtot > 1) then
       call getfrac(stmpar%morlyr,frac      ,anymud    ,mudcnt    , &
                  & mudfrac      ,1         ,ndxi)
    endif
!   BEGIN DEBUG
    frac = 1d0
!   END DEBUG
!!    !
!!    ! Calculate velocity components and magnitude at the zeta points
!!    ! based on velocity in the bottom computational layer
!!    !
!!    call z_dwnvel(nmmax     ,kmax      ,icx       ,kcs       ,kfu       , &
!!                & kfv       ,kcu       ,kcv       ,s1        ,dps       , &
!!                & u0eul     ,v0eul     ,uuu       ,vvv       ,umod      , &
!!                & zumod     ,dzs1      ,hu        ,hv        ,kfsed     , &
!!                & kfsmin    ,kfsmax    ,kfumin    ,kfumax    ,kfvmin    , &
!!                & kfvmax    ,dzu1      ,dzv1      ,z0ucur    ,z0vcur    , &
!!                & vonkar    ,gdp     )
! Quick hack for 2D ...

!   compute ucx, ucy
    call setucxucyucxuucyu()

    uuu   = ucx
    vvv   = ucy
    umod  = sqrt(uuu*uuu + vvv*vvv)
    zumod = (s1 - bl)/ee
!!    call dfexchg( uuu,  1, 1, dfloat, nm_pos, gdp)
!!    call dfexchg( vvv,  1, 1, dfloat, nm_pos, gdp)
!!    call dfexchg( umod, 1, 1, dfloat, nm_pos, gdp)
!!    call dfexchg( zumod,1, 1, dfloat, nm_pos, gdp)
    !
    ! Get the reduction factor if thickness of sediment at bed is less than
    ! user specified threshold. Also get maximum erosion source SRCMAX
    ! (used for cohesive sediments).
    !
    dtmor = dts * morfac
    !
    call getfixfac(stmpar%morlyr, 1        , ndxi     , lsedtot, &
                 & ndxi         , fixfac    , ffthresh  )
    !
    ! Set fixfac to 1.0 for tracer sediments and adjust frac
    !
    istat = bedcomp_getpointer_integer(stmpar%morlyr, 'IUnderLyr', iunderlyr)
    if (ffthresh>0.0_hp .or. iunderlyr/=1) then
       srcmax = 1.0e+10_fp
    elseif (iunderlyr==1) then
       istat = bedcomp_getpointer_realprec(stmpar%morlyr,'bodsed',bodsed)
       do l = 1, lsed
          if (ffthresh<1.0e-10_fp) then
             !
             ! Compute SRCMAX (only used for cohesive sediments)
             !
             do nm = 1, ndxi
                !
                ! If user-specified THRESH is <= 0.0, the erosion flux is effectively not limited by FIXFAC since ffthresh is 1e-10
                ! but by the amount of sediment that is available
                !
                srcmax(nm, l) = bodsed(l, nm)*cdryb(l)/dtmor
             enddo
          endif
          !
          if (sedtrcfac(l)>0.0_fp) then
             grkg = 1.0_fp / (rhosol(l)*pi*sedd50(l)**3/6.0_fp) ! Number of grains per kg
             grm2 = 0.5_fp / (pi*sedd50(l)**2) ! Number of grains per m^2 -- Not quite correct: maximum area of grain is pi*r^2 not pi*d^2, using porosity factor of 0.5
             do nm = 1, ndxi
                fixfac(nm, l) = 1.0_fp
                grlyrs = bodsed(l, nm) * grkg / grm2 ! Number of grain layers
                frac(nm, l) = min(max(0.0_fp, grlyrs), 1.0_fp)*sedtrcfac(l)
             enddo
          endif
       enddo
    endif
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
                   & logsedsig ,nseddia   ,logseddia ,ndxi      ,1         , &
                   & ndxi      ,xx        ,nxx       ,sedd50fld ,dm        , &
                   & dg        ,dxx       ,dgsd      )
       !
       ! determine hiding & exposure factors
       !
       call comphidexp(frac      ,dm        ,ndxi      ,lsedtot   , &
                     & sedd50    ,hidexp    ,ihidexp   ,asklhe    , &
                     & mwwjhe    ,1         ,ndxi      )
       !
       ! compute sand fraction
       !
       call compsandfrac(frac   ,sedd50       ,ndxi      ,lsedtot   , &
                    & sedtyp    ,sandfrac     ,sedd50fld , &
                    & 1         ,ndxi         )
    endif
    !
    do iln = 1, lnxi
       !
       ! compute normal component of bed slopes at edges      
! SPvdP: Ask Bert about orientation of db/dn
       !
       e_dzdn(iln) = (bl(ln(1,iln)) - bl(ln(2,iln)))*dxi(iln)
    enddo
!!    !
!!    call dfexchg( dzduu,1, 1, dfloat, nm_pos, gdp)
!!    call dfexchg( dzdvv,1, 1, dfloat, nm_pos, gdp)
    !
    ! Start of main loop over sediment fractions for suspended sediment
    ! sources, sinks, equilibrium concentrations and vertical diffusion
    ! coefficients, and bed-load transport vector components at water
    ! level points
    !
!!    call dfexchg( z0ucur,1, 1, dfloat, nm_pos, gdp)
!!    call dfexchg( z0vcur,1, 1, dfloat, nm_pos, gdp)
!!    call dfexchg( z0urou,1, 1, dfloat, nm_pos, gdp)
!!    call dfexchg( z0vrou,1, 1, dfloat, nm_pos, gdp)
!!    do l = 1, lsedtot
!!       call dfexchg( ws(:,:,l),0, kmax, dfloat, nm_pos, gdp)
!!    enddo
    !
    do nm = 1, ndxi
       if (kfs(nm)/=1) cycle
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
          if (kmx>1) then
             ! at layer interfaces, but not at bed and surface
             do k = kbot(nm), ktop(nm)-1
             do l = 1, lsed
                   seddif(l, k) = 0.7_fp * vicwws(k) ! sigdifi * vicwws(k) + difsed(l)
             enddo
             enddo
             ! in layers
             do k = kbot(nm), ktop(nm)
                do l = 1, lsed
!!                   rsedeq(l, k) = 0.0_fp
                enddo
             enddo
          endif
          cycle
       endif
       !
       ! kfsed(nm) == 1
       !
       h0   = max(0.01_fp, s0(nm) - bl(nm))
       h1   = max(0.01_fp, s1(nm) - bl(nm))
!       nmd  = nm - icx
!!       ndm  = nm - icy
!!       call nm_to_n_and_m(nm, n, m, gdp)
       ! In z-layer use kmaxlc, siglc and thicklc for morphology (like sigma-layer).
       kmaxlc = kmx
       if (kmx>0) then  
!!          !
!!          ! 3D CASE
!!          !
!!          kbed    = kbot(nm)
!!          thicklc = 0.0_fp
!!          klc     = 1
!!          do k = kfsmax(nm),kfsmin(nm),-1
!!             thicklc(klc)   = dzs1(nm, k)/h1
!!             klc=klc+1
!!          enddo
!!          siglc   = 0.0_fp
!!          kmaxlc  =klc-1
!!          siglc(1) = -0.5_fp*thicklc(1)
!!          do klc = 2, kmaxlc
!!             siglc(klc) = siglc(klc - 1) - 0.5_fp*(thicklc(klc) + thicklc(klc - 1))
!!          enddo
       else
           kbed    = nm
           kmaxlc  = 1
           thicklc = 1.0_fp
       endif
       
!      BEGIN DEBUG
       kbed = nm
!      END DEBUG
       
       !
       ! Compute depth-averaged velocity components at cell centre
       !
       umean = ucx(nm)
       vmean = ucy(nm)
       velm = sqrt(umean**2+vmean**2)
       !
       ubed = ucx(kbed)
       vbed = ucy(kbed)
       velb = sqrt(ubed**2 + vbed**2)
       if (kmaxlc>1) then
          zvelb = 0.5_fp*thicklc(kmaxlc)*h1
       else
          zvelb = h1/ee
       endif
       !
       ! Calculate current related roughness
       !
!!       kn    = max(1, kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm))
!!       z0cur = (  kfu(nmd)*z0ucur(nmd) + kfu(nm)*z0ucur(nm) &
!!             &  + kfv(ndm)*z0vcur(ndm) + kfv(nm)*z0vcur(nm)  )/kn
       z0cur = 0.01_fp
       
!      SPvdP: use z0 from unstruc
       call getczz0 (h1, frcuni, ifrctypuni, cz_dum, z0cur)


       !
       ! Calculate total (possibly wave enhanced) roughness
       !
!!       z0rou = (  kfu(nmd)*z0urou(nmd) + kfu(nm)*z0urou(nm) &
!!             &  + kfv(ndm)*z0vrou(ndm) + kfv(nm)*z0vrou(nm)  )/kn
       z0rou = z0cur
       chezy = sag * log( 1.0_fp + h1/max(1.0e-8_fp,ee*z0rou) ) / vonkar
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
                           & kssand  , thcmud(nm), taub    , rhowat(kbed), &
                           & vicmol  )
       else
          !
          ! use max bed shear stress, rather than mean
          !
!!          taub = taubmx(nm)
          ustarc = umod(nm)*vonkar/log(1.0_fp + zumod(nm)/z0rou)
          taub = ustarc*ustarc*rhowat(kbed)
       endif
       !
       ustarc = umod(nm)*vonkar/log(1.0_fp + zumod(nm)/z0rou)
       if (scour) then
          !
          ! Calculate extra stress (tauadd) for point = nm, if so required by
          ! user input. Increment TAUB(MX) and USTARC.
          !
!!          call shearx(tauadd, nm, gdp)
          taub = sqrt(taub**2 + tauadd**2)
          !
          tauc = rhowat(kbed)*ustarc**2
          tauc = sqrt(tauc**2 + tauadd**2)
          ustarc = sqrt(tauc/rhowat(kbed))
       else
          tauadd = 0.0_fp
       endif
       !
       ! Compute effective depth averaged velocity
       !
       utot  = ustarc * chezy / sag
       u     = utot * uuu(nm) / (umod(nm)+eps)
       v     = utot * vvv(nm) / (umod(nm)+eps)
       !
!!       if (lsal > 0) then
!!          salinity = r0(nm, kbed, lsal)
!!       else
!!          salinity = saleqs
          salinity = 0.0_fp
!!       endif
!!       if (ltem > 0) then
!!          temperature = r0(nm, kbed, ltem)
!!       else
!!          temperature = temeqs
          temperature = 15.0_fp
!!       endif
!!       !
       taks0 = 0.0_fp
       !
       ! Calculate Van Rijn's reference height
       !
       if (iopkcw==1) then
          rc = 30.0_fp*z0cur
       else
          rc = rdc
       endif
       taks0 = max(aksfac*rc, 0.01_fp*h1)
       !
       if (wave .and. tp(nm)>0.0_fp) then
          delr  = 0.025_fp
          taks0 = max(0.5_fp*delr, taks0)
       endif
       !
       ! Limit maximum aks to 20% of water depth
       ! (may be used when water depth becomes very small)
       !
       taks0 = min(taks0, 0.2_fp*h1)
       !
       ! Input parameters are passed via dll_reals/integers/strings-arrays
       !
       if (max_reals < MAX_RP) then
          write(errmsg,'(a)') 'Insufficient space to pass real values to transport routine.'
          call write_error(errmsg, unit=mdia)
          error = .true.
          return
       endif
       dll_reals(RP_TIME ) = real(time1     ,hp)
       dll_reals(RP_EFUMN) = real(u         ,hp)
       dll_reals(RP_EFVMN) = real(v         ,hp)
       dll_reals(RP_EFVLM) = real(utot      ,hp)
       dll_reals(RP_UCHAR) = real(uuu(nm)   ,hp)
       dll_reals(RP_VCHAR) = real(vvv(nm)   ,hp)
       dll_reals(RP_VELCH) = real(umod(nm)  ,hp)
       dll_reals(RP_ZVLCH) = real(zumod(nm) ,hp)
       dll_reals(RP_DEPTH) = real(h1        ,hp)
       dll_reals(RP_CHEZY) = real(chezy     ,hp)
       if (wave) then
          dll_reals(RP_HRMS ) = real(min(gammax*h1, hrms(nm)) ,hp)
          dll_reals(RP_TPEAK) = real(tp(nm)                   ,hp)
          dll_reals(RP_TETA ) = real(teta(nm)                 ,hp)
          dll_reals(RP_RLAMB) = real(rlabda(nm)               ,hp)
          dll_reals(RP_UORB ) = real(uorb(nm)                 ,hp)
       else
          dll_reals(RP_HRMS ) = 0.0_hp
          dll_reals(RP_TPEAK) = 0.0_hp
          dll_reals(RP_TETA ) = 0.0_hp
          dll_reals(RP_RLAMB) = 0.0_hp
          dll_reals(RP_UORB ) = 0.0_hp
       endif
       dll_reals(RP_D10MX) = real(dxx(nm,i10),hp)
       dll_reals(RP_D90MX) = real(dxx(nm,i90),hp)
       dll_reals(RP_MUDFR) = real(mudfrac(nm),hp)
       dll_reals(RP_RHOWT) = real(rhowat(kbed)   ,hp) ! Density of water
       dll_reals(RP_SALIN) = real(salinity       ,hp)
       dll_reals(RP_TEMP ) = real(temperature    ,hp)
       dll_reals(RP_GRAV ) = real(ag             ,hp)
       dll_reals(RP_VICML) = real(vicmol         ,hp)
       dll_reals(RP_TAUB ) = real(taub           ,hp) !taubmx incremented with tauadd
       dll_reals(RP_UBED ) = real(ubed           ,hp)
       dll_reals(RP_VBED ) = real(vbed           ,hp)
       dll_reals(RP_VELBD) = real(velb           ,hp)
       dll_reals(RP_ZVLBD) = real(zvelb          ,hp)
       dll_reals(RP_VNKAR) = real(vonkar         ,hp)
       dll_reals(RP_Z0CUR) = real(z0cur          ,hp)
       dll_reals(RP_Z0ROU) = real(z0rou          ,hp)
       dll_reals(RP_DG   ) = real(dg(nm)         ,hp)
       dll_reals(RP_SNDFR) = real(sandfrac(nm)   ,hp)
       dll_reals(RP_DGSD ) = real(dgsd(nm)       ,hp)
       if (ltur >= 1) then
!!          dll_reals(RP_KTUR ) = real(rtur0(nm,kbed,1),hp)
       endif
       dll_reals(RP_UMEAN) = real(umean     ,hp)
       dll_reals(RP_VMEAN) = real(vmean     ,hp)
       dll_reals(RP_VELMN) = real(velm      ,hp)
       dll_reals(RP_USTAR) = real(ustarc         ,hp)
       !
       if (max_integers < MAX_IP) then
          write(errmsg,'(a)') 'Insufficient space to pass integer values to transport routine.'
          call write_error(errmsg, unit=mdia)
          error = .true.
          return
       endif
       dll_integers(IP_NM   ) = nm
!!       dll_integers(IP_N    ) = n
!!       dll_integers(IP_M    ) = m
       !
       if (max_strings < MAX_SP) then
          write(errmsg,'(a)') 'Insufficient space to pass strings to transport routine.'
          call write_error(errmsg, unit=mdia)
          error = .true.
          return
       endif
!!       dll_strings(SP_RUNID) = gdp%runid
       !
       ! total mass in fluff layer
       !
       mfltot = 0.0_fp
       if (iflufflyr>0) then
            do l = 1, lsedtot
                mfltot = mfltot + max(0.0_fp,mfluff(l,nm))
            enddo
       endif
       !
       do l = 1, lsedtot
          !
          ! fraction specific quantities
          !
          dll_reals(RP_HIDEX)    = real(hidexp(nm,l) ,hp)
          dll_reals(RP_RHOSL)    = real(rhosol(l) ,hp)
          dll_integers(IP_ISED ) = l
          dll_strings(SP_USRFL)  = dll_usrfil(l)
          !
          do i = 1,stmpar%trapar%npar
             j = stmpar%trapar%iparfld(i,l)
             if (j>0) then
                 par(i,l) = stmpar%trapar%parfld(nm,j)
             endif
          enddo
          !
          if (sedtyp(l) == SEDTYP_COHESIVE) then
             !
             ! sediment type COHESIVE
             !
             dll_reals(RP_D50  ) = 0.0_hp
             dll_reals(RP_DSS  ) = 0.0_hp
             dll_reals(RP_DSTAR) = 0.0_hp
!!             dll_reals(RP_SETVL) = real(ws(nm, kbed, l)  ,hp) ! Vertical velocity near bedlevel
!!             if (flmd2l) then
!!                 par(11,l) = entr(nm)
!!             endif
             !
             klc = 0
             dcwwlc = 0.0_fp
             wslc   = 0.0_fp
!!             do k = kfsmax(nm),kfsmin(nm)-1,-1
!!                dcwwlc(klc) = dicww(nm, k)
!!                wslc(klc)   = ws(nm, k, l)
!!                klc=klc+1
!!             enddo
             !
             ! Fluff layer parameters
             !
             fracf   = 0.0_fp
             if (iflufflyr>0) then
                if (mfltot>0.0_fp) fracf   = max(0.0_fp,mfluff(l,nm))/mfltot
             endif
             !
             kmaxsd        = kmaxlc ! for mud fractions kmaxsd points to the grid cell at the bottom of the water column
             thick0        = thicklc(kmaxsd) * h0
             thick1        = thicklc(kmaxsd) * h1
             ! following lines to be replaced by call ...
             sinktot       = 0.0_fp
             sourfluff     = 0.0_fp
!!             call erosilt(thicklc     ,kmaxlc      ,wslc        ,mdia        , &
!!                        & thick0      ,thick1      ,fixfac(nm,l), srcmax(nm, l),&
!!                        & frac(nm,l)  ,oldmudfrac  ,flmd2l      ,iform(l)    , &
!!                        & par         ,max_integers,max_reals   ,max_strings , &
!!                        & dll_function(l),dll_handle(l),dll_integers,dll_reals, &
!!                        & dll_strings  ,iflufflyr ,mfltot ,fracf    , &
!!                        & error ,wstau(nm) ,sinktot ,sourse(nm,l), sourfluff)
             if (error) return
             !
             if (iflufflyr>0) then
                if (iflufflyr==2) then
                   sinkf(l,nm)  = sinktot*(1.0_fp - depfac(l,nm))
                   sinkse(nm,l) = sinktot*depfac(l,nm)
                else
                   sinkf(l,nm)  = sinktot
                   sinkse(nm,l) = 0.0_fp
                endif
                !
                sourf(l,nm)  = sourfluff
             else
                sinkse(nm,l) = sinktot
                sourse(nm,l) = sourse(nm,l) + sourfluff ! sourfluff should actually always be 0 already
             endif
             !
             if (kmx>1) then 
                !
                ! For 3D model set sediment diffusion coefficient
                ! NOTE THAT IF ALGEBRAIC OR K-L TURBULENCE MODEL IS USED THEN WAVES
                ! ONLY AFFECT THE VERTICAL TURBULENT MIXING VIA THE ENHANCED BED
                ! ROUGHNESS
                !
                klc    = 0
!!                do k = kfsmax(nm),kfsmin(nm)-1,-1
!!                   seddif(nm, k, l) = dcwwlc(klc)
!!                   klc=klc+1
!!                enddo
             endif
             !
             ! l runs from 1 to lsedtot, kmxsed is defined for 1:lsed
             ! The first lsed fractions are the suspended fractions (including cohesive ones),
             ! so this goes right
             !
!!             kmxsed(nm, l) = kfsmin(nm)+kmaxlc-kmaxsd
             cycle
          endif
          !
          ! sediment type NONCOHESIVE_SUSPENDED or NONCOHESIVE_TOTALLOAD
          !
!!          ll = lstart + l
          suspfrac = sedtyp(l)/=SEDTYP_NONCOHESIVE_TOTALLOAD
          !
          ! Calculation for sand or bedload
          !
          ! Reset Prandtl-Schmidt number for sand fractions
          !
          if (suspfrac) then
!!             sigdif(ll) = 1.0_fp
          endif
          tsd  = -999.0_fp
          di50 = sedd50(l)
          !DEBUG if (di50 < 0.0_fp) then
             !
             ! Space varying sedd50 specified in array sedd50fld:
             ! Recalculate dstar, tetacr and taucr for each nm,l - point
             ! This code is copied from inised (uniform sedd50)
             !
             !DEBUG di50     = sedd50fld(nm)
             drho     = (rhosol(l)-rhowat(kbed)) / rhowat(kbed)
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
             taucr(l) = factcr * (rhosol(l)-rhowat(kbed)) * ag * di50 * tetacr(l)
          !DEBUG endif
          !
          if (suspfrac) then
!!             tsigmol = sigmol(ll)
!!             tdss    = dss(nm, l)
!!             twsk    = ws(nm, kbed, l)

!            BEGIN DEBUG
             tsigmol =  1.0_fp
             tdss    = di50
             twsk    = 1d-4
!            END DEBUG

          else
             !
             ! use dummy values for bedload fractions
             !
             tsigmol =  1.0_fp
             tdss    = di50
             twsk    =  0.0_fp
          endif
          !
          ! NONCOHESIVE fraction specific quantities
          !
          dll_reals(RP_D50  ) = real(di50    ,hp)
          dll_reals(RP_DSS  ) = real(tdss    ,hp)
          dll_reals(RP_DSTAR) = real(dstar(l),hp)

          dll_reals(RP_SETVL) = real(twsk    ,hp) ! Vertical velocity near bedlevel
          par(1,l) = ag
          par(2,l) = rhowat(kbed) ! rhow
          par(3,l) = rhosol(l)
          par(4,l) = (rhosol(l)-rhowat(kbed)) / rhowat(kbed)
          par(5,l) = 1.0E-6     ! rnu    from md-tran.*
          par(6,l) = di50
          !
          ! TODO: compute bed slope vector in cell centre from normal components of bed slope at edges
          !
          dzdx = 0.0_fp
          dzdy = 0.0_fp
          !
          ! SWITCH 2DH/3D SIMULATIONS
          !
          if (kmaxlc > 1) then
!!             !
!!             ! 3D CASE
!!             !
!!             if (suspfrac) then
!!                !
!!                ! Fill local 1dv arrays with fall velocity and diffusivity.
!!                !
!!                klc    = 0
!!                dcwwlc = 0.0_fp
!!                wslc   = 0.0_fp
!!                do k = kfsmax(nm),kfsmin(nm)-1,-1
!!                   dcwwlc(klc) = dicww(nm, k)
!!                   wslc(klc)   = ws(nm, k, l)
!!                   klc=klc+1
!!                enddo
!!             endif
!!             taks = 0.0_fp
!!             !
!!             klc    = 1
!!             do k = kfsmax(nm),kfsmin(nm),-1
!!                concin3d(klc) = max(0.0_fp , r0(nm,k,ll))
!!                klc=klc+1
!!             enddo
!!             !
!!             ! Solve equilibrium concentration vertical and
!!             ! integrate over vertical
!!             !
!!             call eqtran(siglc     ,thicklc   ,kmaxlc    ,wslc      ,ltur      , &
!!                       & frac(nm,l),tsigmol   ,dcwwlc    ,lundia    ,taucr(l)  , &
!!                       & rksr(nm)  ,3         ,lsecfl    ,spirint   ,suspfrac  , &
!!                       & tetacr(l) ,concin3d  , &
!!                       & dzduu(nm) ,dzdvv(nm) ,ubot(nm)  ,tauadd    ,sus       , &
!!                       & bed       ,susw      ,bedw      ,espir     ,wave      , &
!!                       & scour     ,ubot_from_com        ,camax     ,eps       , &
!!                       & iform(l)  ,par(1,l)  ,max_integers,max_reals,max_strings, &
!!                       & dll_function(l),dll_handle(l),dll_integers,dll_reals,dll_strings, &
!!                       & taks      ,caks      ,taurat(nm,l),sddflc  ,rsdqlc    , &
!!                       & kmaxsd    ,conc2d    ,sbcu(nm,l ),sbcv(nm,l),sbwu(nm,l), &
!!                       & sbwv(nm,l),sswu(nm,l),sswv(nm,l),tdss      ,caks_ss3d , &
!!                       & aks_ss3d  ,ust2(nm)  ,tsd       ,error     )
!!             if (error) call d3stop(1, gdp)
!!             if (suspfrac) then
!!                aks(nm, l) = taks
!!                dss(nm, l) = tdss
!!                !
!!                ! Copy results into arrays
!!                !
!!                kmxsed(nm, l) = kfsmin(nm)+kmaxlc-kmaxsd
!!                !
!!                klc=0
!!                do k = kfsmax(nm),kfsmin(nm)-1,-1
!!                   seddif(nm,k,l) = sddflc(klc)
!!                   klc            = klc + 1
!!                enddo
!!                klc = 1
!!                do k = kfsmax(nm),kfsmin(nm),-1
!!                   rsedeq(nm,k,l) = rsdqlc(klc)
!!                   klc            = klc + 1
!!                enddo
!!                !
!!                ! Source and sink terms for main 3d computation
!!                ! note: terms are part explicit, part implicit, see
!!                ! thesis of Giles Lesser, May 2000
!!                !
!!                thick0 = thicklc(kmaxsd) * h0
!!                thick1 = thicklc(kmaxsd) * h1
!!                call soursin_3d  (h1                ,thick0         ,thick1             , &
!!                               &  siglc(kmaxsd)     ,thicklc(kmaxsd),r0(nm,kmxsed(nm,l) ,ll)    , &
!!                               &  vicmol            ,sigmol(ll)     ,seddif(nm,kmxsed(nm,l)-1,l), &
!!                               &  rhosol(l)         ,caks_ss3d      ,ws(nm,kmxsed(nm,l),l)      , &
!!                               &  aks_ss3d          ,sourse(nm,l)   ,sour_im(nm,l)      , &
!!                               &  sinkse(nm,l) )
!!                ! Impose relatively large vertical diffusion
!!                ! coefficients for sediment in layer interfaces from
!!                ! bottom of reference cell downwards, to ensure little
!!                ! gradient in sed. conc. exists in this area.
!!                !
!!                difbot = 10.0_fp * ws(nm,kmxsed(nm,l)-1,l) * thick1
!!                do k = kfsmin(nm)-1, kmxsed(nm,1)-1
!!                   seddif(nm, k, l) = difbot
!!                enddo
!!             endif ! suspfrac
          else
             !
             ! kmaxlc = 1
             ! 2D CASE (Numerical approximation)
             !
             if (suspfrac) then
                !
                ! Fill local 1dv arrays with fall velocity and
                ! diffusivity
                !
                do k2d = 0, kmax2d
!!                   ws2d(k2d)   = ws(nm, kbed, l)
!                  BEGIN DEBUG
                   ws2d(k2d) = 1d-4
!                  END DEBUG
                   dcww2d(k2d) = 0.0_fp
                enddo
!!                trsedeq = rsedeq(nm, kbed, l)
             else
                trsedeq =  0.0_fp
             endif
             taks = taks0
             !
             if (lsecfl > 0) then
!!                spirint = r0(nm, kbed, lsecfl)
             else
                spirint = 0.0_fp
             endif
             !
             ! Solve equilibrium concentration vertical and
             ! integrate over vertical; compute bedload
             ! transport excluding slope effects.
             !
             
!            BEGIN DEBUG
             if ( time1.gt.1320 .and. abs(nm-0.5*ndxi).lt.1d0 ) then
                continue
             end if
!            END DEBUG
             
             call eqtran(sig2d     ,thck2d    ,kmax2d    ,ws2d      ,ltur      , &
                       & frac(nm,l),tsigmol   ,dcww2d    ,mdia      ,taucr(l)  , &
                       & rksr(nm)  ,2         ,lsecfl    ,spirint   ,suspfrac  , &
                       & tetacr(l) ,concin2d  , &
                       & dzdx      ,dzdy      ,ubot(nm)  ,tauadd    ,sus       , &
                       & bed       ,susw      ,bedw      ,espir     ,wave      , &
                       & scour     ,ubot_from_com        ,camax     ,eps       , &
                       & iform(l)  ,par(:,l)  ,max_integers,max_reals,max_strings, &
                       & dll_function(l),dll_handle(l),dll_integers,dll_reals,dll_strings, &
                       & taks      ,caks      ,taurat(nm,l),sddf2d  ,rsdq2d    , &
                       & kmaxsd    ,trsedeq   ,sbcx(nm,l) ,sbcy(nm,l),sbwx(nm,l) , &
                       & sbwy(nm,l),sswx(nm,l),sswy(nm,l),tdss      ,caks_ss3d , &
                       & aks_ss3d  ,ust2(nm)  ,tsd       ,error     )
             if (error) return
             if (suspfrac) then
!!                aks   (nm, l)    = taks
!!                dss   (nm, l)    = tdss
!!                rsedeq(nm, kbed, l) = trsedeq
                kmxsed(nm, l)    = kbed
                !
                ! Galappatti time scale and source and sink terms
                !
                call soursin_2d(umod(nm)      ,ustarc        ,h0            ,h1        , &
!DEBUG                              & ws(nm,kbed,l) ,tsd           ,rsedeq(nm,kbed,l),factsd,         &
                              & twsk ,tsd           ,trsedeq, 1d0,        &
                              & sourse(nm,l)  ,sour_im(nm,l) ,sinkse(nm,l)  )
             endif ! suspfrac
          endif ! kmaxlc = 1
          if (suspfrac) then
!               caksrho(l, nm) = caks * rhosol(l)
          endif
       enddo ! next sediment fraction
    enddo ! next nm point
    !
    ! Reduce the source and sink terms to avoid large bed level changes
    ! Note: previous implementation forgot to multiply source/
    !       sink terms with the thickness for the 2Dh case
    !
!!    call   z_red_soursin(nmmax     ,kmax      ,thick     ,kmxsed    , &
!!                       & lsal      ,ltem      ,lsed      ,lsedtot   , &
!!                       & dps       ,s0        ,s1        ,r0        , &
!!                       & rsedeq    ,nst       ,dzs1      ,kfsmax    , &
!!                       & kfsmin    ,kfs       ,gdp       )
    !
    ! Fill sutot and svtot
    !
    do l = 1,lsedtot
!!       call dfexchg( sbcu(:,l) ,1, 1, dfloat, nm_pos, gdp)
!!       call dfexchg( sbwu(:,l) ,1, 1, dfloat, nm_pos, gdp)
!!       call dfexchg( sswu(:,l) ,1, 1, dfloat, nm_pos, gdp)
!!       call dfexchg( sbcv(:,l) ,1, 1, dfloat, nm_pos, gdp)
!!       call dfexchg( sbwv(:,l) ,1, 1, dfloat, nm_pos, gdp)
!!       call dfexchg( sswv(:,l) ,1, 1, dfloat, nm_pos, gdp)
       if (sedtyp(l)/=SEDTYP_COHESIVE) then
          do nm = 1, ndxi
             sxtot(nm, l) = sbcx(nm, l) + sbwx(nm, l) + sswx(nm, l)
             sytot(nm, l) = sbcy(nm, l) + sbwy(nm, l) + sswy(nm, l)
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
!!       call upwbed(sbcu      ,sbcv      ,sbcuu     ,sbcvv     ,kfu       , &
!!                 & kfv       ,kcs       ,kfsed     ,lsedtot   , &
!!                 & nmmax     ,icx       ,icy       ,sutot     ,svtot     , &
!!                 & gdp       )
       call fm_upwbed(lsedtot, sbcx, sbcy, sxtot, sytot, e_sbcn, e_sbct)
       
       

    endif
    !
    if (bedw>0.0_fp .and. wave) then
       !
       ! Upwind wave-related bed load load transports
       !
!!       call upwbed(sbwu      ,sbwv      ,sbwuu     ,sbwvv     ,kfu       , &
!!                 & kfv       ,kcs       ,kfsed     ,lsedtot   , &
!!                 & nmmax     ,icx       ,icy       ,sutot     ,svtot     , &
!!                 & gdp       )
    endif
    !
    if (susw>0.0_fp .and. wave) then
       !
       ! Upwind wave-related suspended load transports
       !
!!       call upwbed(sswu      ,sswv      ,sswuu     ,sswvv     ,kfu       , &
!!                 & kfv       ,kcs       ,kfsed     ,lsedtot   , &
!!                 & nmmax     ,icx       ,icy       ,sutot     ,svtot     , &
!!                 & gdp       )
    endif
    !
    ! Bed-slope and sediment availability effects for
    ! current-related bed load transport
    !
    if (bed > 0.0_fp) then
!!       call adjust_bedload(nmmax     ,icx       ,icy       ,kcs       , &
!!              & kcu       ,kcv       ,kfu       ,kfv       ,lsedtot   , &
!!              & sbcuu     ,sbcvv     ,sbuut     ,sbvvt     ,dzduu     , &
!!              & dzdvv     ,taurat    ,frac      ,fixfac    ,ust2      , &
!!              & hu        ,hv        ,dm        ,hidexp    ,.true.    , &
!!              & .true.    ,rhowat    ,kmax      ,dps       ,gsqs      , &
!!              & guu       ,gvv       ,guv       ,gvu       ,kbed      , &
!!              & gdp       )
    endif
    !
    ! Bed-slope and sediment availability effects for
    ! wave-related bed load transport
    !
    if (bedw>0.0_fp .and. wave) then
!!       call adjust_bedload(nmmax     ,icx       ,icy       ,kcs       , &
!!              & kcu       ,kcv       ,kfu       ,kfv       ,lsedtot   , &
!!              & sbwuu     ,sbwvv     ,sbuut     ,sbvvt     ,dzduu     , &
!!              & dzdvv     ,taurat    ,frac      ,fixfac    ,ust2      , &
!!              & hu        ,hv        ,dm        ,hidexp    ,.true.    , &
!!              & .false.   ,rhowat    ,kmax      ,dps       ,gsqs      , &
!!              & guu       ,gvv       ,guv       ,gvu       ,kbed      , &
!!              & gdp       )
    endif
    !
    ! Sediment availability effects for
    ! wave-related suspended load transport
    !
    if (susw>0.0_fp .and. wave) then
!!       call adjust_bedload(nmmax     ,icx       ,icy       ,kcs       , &
!!              & kcu       ,kcv       ,kfu       ,kfv       ,lsedtot   , &
!!              & sswuu     ,sswvv     ,sbuut     ,sbvvt     ,dzduu     , &
!!              & dzdvv     ,taurat    ,frac      ,fixfac    ,ust2      , &
!!              & hu        ,hv        ,dm        ,hidexp    ,.false.   , &
!!              & .false.   ,rhowat    ,kmax      ,dps       ,gsqs      , &
!!              & guu       ,gvv       ,guv       ,gvu       ,kbed      , &
!!              & gdp       )
    endif
    !
    ! Summation of current-related and wave-related transports
    !
    !do l = 1,lsedtot
    !   if (sedtyp(l)/=SEDTYP_COHESIVE) then
    !      do nm = 1, lnx
    !         sbuu(l,nm) = e_sbcn(nm, l) + e_sbwn(nm, l) + e_sswn(nm, l)
    !      enddo
    !   endif
    !enddo
    !
    ! Update sourse fluxes due to sand-mud interaction
    !
    allocate(E(lsedtot), STAT=istat)
    do nm = 1, ndxi
        if (pmcrit(nm)<0.0_fp) cycle
        if (mudfrac(nm)<=0.0_fp .or. mudfrac(nm)>=1.0_fp) cycle
        !
        ! Compute erosion velocities
        !
        E = 0.0_fp        
        do l = 1, lsed
!!            ll = lstart + l
            kmaxsd = kmxsed (nm,l)
!!            if (frac(nm,l)>0.0_fp)  E(l) = (sourse(nm,l) - sour_im(nm,l)*r0(nm,kmaxsd,ll))/(cdryb(l)*frac(nm,l))
        enddo
        !
        ! Recompute erosion velocities
        !
        call sand_mud(lsed, E, frac(nm,:), mudfrac(nm), sedtyp, pmcrit(nm))
        !
        ! Recompute erosion fluxes
        ! only explicit part of erosion flux is changed 
        ! (is effectively the same as changing the equilibrium concentration)
        !
        do l = 1, lsed
!!            ll = lstart + l
            kmaxsd = kmxsed (nm,l)
!!            sourse(nm,l) = frac(nm,l)*cdryb(l)*E(l) + sour_im(nm,l)*r0(nm,kmaxsd,ll)
        enddo
    enddo
    deallocate(E, STAT=istat)
    !
    ! Add implicit part of source term to sinkse
    !
    
! SPvdP: source_im > 0 acts as sink
    do l = 1, lsed
        do nm = 1, ndxi
            sinkse(nm, l) = sinkse(nm, l) + sour_im(nm, l)
        enddo
    enddo
    
!   BEGIN DEBUG
!    call realloc(plotlin,Ndx)
!    plotlin = DMISS
!    do nm=1,Ndxi
!       plotlin(nm) = sinkse(nm,1)
!       plotlin(nm) = sour_im(nm,1)
!       plotlin(nm) = sourse(nm,1)
!    end do
!   END DEBUG

    !
    ! Finally fill sour and sink arrays for both sand and silt
    ! note that sourse/sinkse and sourf/sinkf arrays are required for BOTT3D
    !
    do l = 1, lsed
!!       ll = ISED1 + l - 1
       do nm = 1, ndxi
!          k = kmxsed(nm,l)
          !
          !  sourse/sinkse/sourf/sinkf are defined per unit volume
          !  in z-model the sour/sink terms are defined per content of cell 
          !                                       sedtra%sourse(nm,l)
!!          constsour(ll, k) = constsour(ll, k) + sourse(nm,l)
!!          constsink(ll, k) = constsink(ll, k) + sinkse(nm,l)
          if (iflufflyr>0) then
!!             sour(nm, k, ll) = sour(nm, k, ll) + sourf(l, nm) * gsqs(nm) * dzs0(nm,kmxsed(nm,l))
!!             sink(nm, k, ll) = sink(nm, k, ll) + sinkf(l, nm) * gsqs(nm) * dzs1(nm,kmxsed(nm,l))
          endif
       enddo
!!       call dfexchg( sour(:,:,l),1, kmax, dfloat, nm_pos, gdp)
!!       call dfexchg( sink(:,:,l),1, kmax, dfloat, nm_pos, gdp)
    enddo
!!    !
!!    ! DD-Mapper: copy sbuu and sbvv
!!    !
!!    nhystp = nxtstp(d3dflow_sediment, gdp)

   return
end subroutine fm_erosed


! Interpolate flownode-based vector (sx,sy) to edge-based vector (sxx, syy)
subroutine fm_upwbed(lsedtot, sx, sy, sxtot, sytot, e_sn, e_st)
   use m_flowgeom
   use m_flow
   use unstruc_messages
   implicit none
   
   integer,                                  intent(in)  :: lsedtot        !< number of sediment fractions
   double precision, dimension(Ndx,lsedtot), intent(in)  :: sx, sy         !< cell (flownode)-based quantity
   double precision, dimension(Ndx,lsedtot), intent(in)  :: sxtot, sytot   !< cell (flownode)-based fluxes
   double precision, dimension(Lnx,lsedtot), intent(out) :: e_sn, e_st     !< edge (flowlink)-based quantity, normal and tangential components

   double precision                                      :: sutot1, sutot2

   integer                                               :: k1, k2, Lf, l

   !if ( laterallyaveragedbedload ) then
   !   call mess(LEVEL_ERROR, 'upwbed: laterally averaged bedload not supported')
   !end if

!  internal flowlinks   
   do Lf=1,Lnxi
!     check if flowlink is active and if it connects two active sediment flownodes
      if ( hu(Lf).gt.epshu ) then
!        find left and right neighboring flownodes
         k1 = ln(1,Lf)
         k2 = ln(2,Lf)
         
         do l=1,lsedtot
!           project the fluxes in flowlink direction
            sutot1 =  csu(Lf)*sxtot(k1,l) + snu(Lf)*sytot(k1,l)
            sutot2 =  csu(Lf)*sxtot(k2,l) + snu(Lf)*sytot(k2,l)

!           upwind approximation
            if ( sutot1.gt.0d0 .and. sutot2.gt.0d0 ) then
               e_sn(Lf,l) =  csu(Lf)*sx(k1,l) + snu(Lf)*sy(k1,l)
               e_st(Lf,l) = -snu(Lf)*sx(k1,l) + csu(Lf)*sy(k1,l)
            else if ( sutot1.lt.0d0 .and. sutot2.lt.0d0 ) then
               e_sn(Lf,l) =  csu(Lf)*sx(k2,l) + snu(Lf)*sy(k2,l)
               e_st(Lf,l) = -snu(Lf)*sx(k2,l) + csu(Lf)*sy(k2,l)
            else
               e_sn(Lf,l) =  csu(Lf)*0.5d0*(sx(k1,l)+sx(k2,l)) + snu(Lf)*0.5d0*(sy(k1,l)+sy(k2,l))
               e_st(Lf,l) = -snu(Lf)*0.5d0*(sx(k1,l)+sx(k2,l)) + csu(Lf)*0.5d0*(sy(k1,l)+sy(k2,l))
            end if
         end do
      else
         do l=1,lsedtot
            e_sn(Lf,l) = 0d0
            e_st(Lf,l) = 0d0
         end do
      end if
   end do
   
!  boundary flowlinks
   do Lf=Lnxi+1,Lnx
      if ( hu(Lf).gt.epshu ) then
!        find left and right neighboring flownodes
         k1 = ln(1,Lf)  ! boundary node
         k2 = ln(2,Lf)  ! internal node
         
         do l=1,lsedtot
            e_sn(Lf,l) =  csu(Lf)*sx(k2,l) + snu(Lf)*sy(k2,l)
            e_st(Lf,l) = -snu(Lf)*sx(k2,l) + csu(Lf)*sy(k2,l)
         end do
      else
         do l=1,lsedtot
            e_sn(Lf,l) = 0d0
            e_st(Lf,l) = 0d0
         end do
      end if
   end do

   return
end subroutine fm_upwbed


subroutine fm_bott3d()
!                  nmmax     ,kmax      ,lsed      ,lsedtot  , &
!                & lsal      ,ltem      ,kfs       ,kfu       ,kfv       , &
!                & r1        ,s0        ,kcs       , &
!                & dps       ,gsqs      ,guu       , &
!                & gvv       ,s1        ,thick     ,dp        , &
!                & umean     ,vmean     ,sbuu      ,sbvv      , &
!                & depchg    ,ssuu      ,ssvv      ,nst       ,hu        , &
!                & hv        ,aks       ,sig       ,u1        ,v1        , &
!                & sscomp    ,kcsbot    , &
!                & guv       ,gvu       ,rca       ,kcu       , &
!                & kcv       ,icx       ,icy       ,timhr     , &
!                & nto       ,volum0    ,volum1    ,dt        ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2018.                                
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
!  $Id: fm_erosed.f90 52266 2017-09-02 11:24:11Z klecz_ml $
!  $HeadURL: https://repos.deltares.nl/repos/ds/branches/dflowfm/20161017_dflowfm_codecleanup/engines_gpl/dflowfm/packages/dflowfm_kernel/src/fm_erosed.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes suspended sediment transport correction
!              vector for sand sediment fractions
!              Computes depth integrated suspended sediment
!              transport vector for output to map file
!              Computes change in BODSED based on source and sink
!              terms calculated in EROSED, and new concentrations.
!              Calculates new mixing layer thickness based on
!              change in BODSED values
!              Calculates new depth values based on changes
!              in bottom sediment.
!              Includes erosion of dry points and associated
!              bathymetry changes
! Method used: Attention: pointer ll for 'standard' FLOW
!              arrays is shifted with lstart
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!!--declarations----------------------------------------------------------------
    use precision
!    use sp_buffer
!    use flow_tables
!    use bedcomposition_module
!    use globaldata
!    use dfparall
    use sediment_basics_module
    use m_flowgeom
    use m_sediment,  only: stmpar, sedtra, mtd
    use m_flowtimes, only: dts, tstart_user, time1
!    !
    implicit none
!    !
!    type(globdat),target :: gdp
!    !
!    ! The following list of pointer parameters is used to point inside the gdp structure
!    !
!    include 'flow_steps_f.inc'
!    integer                              , pointer :: lundia
    real(hp)                             , pointer :: morft
    real(fp)                             , pointer :: morfac
!    real(fp)                             , pointer :: sus
    real(fp)                             , pointer :: bed
    real(fp)                             , pointer :: tmor
!    real(fp)                             , pointer :: thetsd
!    real(fp)                             , pointer :: sedthr
!    real(fp)                             , pointer :: hmaxth
!    integer                              , pointer :: mergehandle
!    integer                              , pointer :: itmor
!    type (handletype)                    , pointer :: bcmfile
!    type (bedbndtype)     , dimension(:) , pointer :: morbnd
!    real(hp)              , dimension(:) , pointer :: mergebuf
    logical                              , pointer :: bedupd
!    logical                              , pointer :: cmpupd
!    logical                              , pointer :: neglectentrainment
!    logical                              , pointer :: multi
!    logical                              , pointer :: wind
!    logical                              , pointer :: temp
!    logical                              , pointer :: const
!    logical                              , pointer :: dredge
!    logical                              , pointer :: struct
!    logical                              , pointer :: sedim
!    logical                              , pointer :: scour
!    logical                              , pointer :: snelli
!    real(fp), dimension(:)               , pointer :: factor
!    real(fp)                             , pointer :: slope
!    real(fp), dimension(:)               , pointer :: bc_mor_array
    real(fp), dimension(:,:)             , pointer :: dbodsd
!    real(fp), dimension(:)               , pointer :: dm
!    real(fp), dimension(:)               , pointer :: dg
!    real(fp), dimension(:,:)             , pointer :: fixfac
!    real(fp), dimension(:,:)             , pointer :: frac
!    integer , dimension(:)               , pointer :: kfsed
!    integer , dimension(:,:)             , pointer :: kmxsed
!    real(fp), dimension(:)               , pointer :: mudfrac
!    real(fp), dimension(:,:)             , pointer :: sbuuc
!    real(fp), dimension(:,:)             , pointer :: sbvvc
!    real(fp), dimension(:,:)             , pointer :: ssuuc
!    real(fp), dimension(:,:)             , pointer :: ssvvc
!    real(fp), dimension(:,:)             , pointer :: sucor
!    real(fp), dimension(:,:)             , pointer :: svcor
    real(fp), dimension(:,:)             , pointer :: sinkse
    real(fp), dimension(:,:)             , pointer :: sourse
!    integer                              , pointer :: nmudfrac
!    real(fp)      , dimension(:)         , pointer :: rhosol
    real(fp)      , dimension(:)         , pointer :: cdryb
    integer       , dimension(:)         , pointer :: sedtyp
!    integer                              , pointer :: julday
!    integer                              , pointer :: ntstep
!    real(fp), dimension(:,:,:)           , pointer :: fluxu
!    real(fp), dimension(:,:,:)           , pointer :: fluxv
!    real(fp), dimension(:)               , pointer :: duneheight
!    integer                              , pointer :: iflufflyr
!    real(fp), dimension(:,:)             , pointer :: mfluff
!    real(fp), dimension(:,:)             , pointer :: sinkf
!    real(fp), dimension(:,:)             , pointer :: sourf
!!
!! Local parameters
!!
!    integer, parameter :: bedchangemessmax = 50
!!
!! Global variables
!!
!    integer                                            , intent(in)  :: icx
!    integer                                            , intent(in)  :: icy
!    integer                                            , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
!    integer                                            , intent(in)  :: lsal   !  Description and declaration in dimens.igs
!    integer                                            , intent(in)  :: lsed   !  Description and declaration in esm_alloc_int.f90
!    integer                                            , intent(in)  :: lsedtot!  Description and declaration in esm_alloc_int.f90
    integer, pointer :: lsedtot
    integer, pointer :: lsed
!    integer                                            , intent(in)  :: ltem   !  Description and declaration in dimens.igs
!    integer                                            , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
!    integer                                            , intent(in)  :: nto    !  Number of open boundaries (esm_alloc_int.igs)
!    integer                                            , intent(in)  :: nst
!    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
!    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kcsbot
!    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
!    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kcv    !  Description and declaration in esm_alloc_int.f90
!    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
!    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
!    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
!    logical                                            , intent(in)  :: sscomp
!    real(fp)                                           , intent(in)  :: dt
!    real(fp)                                                         :: timhr
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsed)   , intent(in)  :: aks    !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                       :: depchg !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(:), pointer :: depchg
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                       :: dp     !  Description and declaration in esm_alloc_real.f90
!    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                     :: dps    !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: gsqs   !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: guv    !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: hu     !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: hv     !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                       :: s0     !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                       :: s1     !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: umean  !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: vmean  !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in)  :: u1     !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in)  :: v1     !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in)  :: volum0 !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in)  :: volum1 !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, *), intent(in)  :: r1     !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsed)   , intent(in)  :: rca    !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot)              :: sbuu   !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot)              :: sbvv   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(:,:), pointer :: e_sbcn
    real(fp), dimension(:,:), pointer :: e_sbct
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsed)                 :: ssuu   !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsed)                 :: ssvv   !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(kmax)                          , intent(in)  :: sig    !  Description and declaration in esm_alloc_real.f90
!    real(fp), dimension(kmax)                          , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
!!
!! Local variables
!!
!    integer  :: i
!    integer  :: ib
!    integer  :: icond
!    integer  :: idir_scalar
!    integer  :: jb
!    integer  :: k
!    integer  :: kvalue
    integer  :: l
!    integer  :: li
!    integer  :: ll
!    integer  :: lsedbed
!    integer  :: lstart
!    integer  :: m
!    integer  :: n
!    integer  :: ndm
!    integer  :: nhystp
    integer  :: nm
!    integer  :: nmd
!    integer  :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!    integer  :: nmu
!    integer  :: num
!    integer  :: numu
!    integer  :: nxmx
!    integer  :: bedchangemesscount
    logical  :: bedload
!    logical  :: from_ndm
!    logical  :: from_nmd
!    logical  :: from_nmu
!    logical  :: from_num
!    real(fp) :: aksu
!    real(fp) :: apower
!    real(fp) :: cavg
!    real(fp) :: cavg1
!    real(fp) :: cavg2
!    real(fp) :: ceavg
!    real(fp) :: cumflux
!    real(fp) :: dhmax
!    real(fp) :: alfa_dist
!    real(fp) :: alfa_mag
    real(fp) :: dsdnm
!    real(fp) :: dv
!    real(fp) :: dz
    real(fp) :: eroflx
!    real(fp) :: fact
!    real(fp) :: gsqsmin
!    real(fp) :: gsqsinv
!    real(fp) :: h1
    real(fp) :: dtmor
!    real(fp) :: htdif
!    real(fp) :: rate
!    real(fp) :: r1avg
    real(fp) :: sedflx
!    real(fp) :: thet
!    real(fp) :: thick0
!    real(fp) :: thick1
!    real(fp) :: totdbodsd
!    real(fp) :: totfixfrac
    real(fp) :: trndiv
!    real(fp) :: z

    real(fp) :: flux, sumflux
    
    integer  :: ii, LL, Lf


!!
!!! executable statements -------------------------------------------------------
!!
!    lundia              => gdp%gdinout%lundia
    morft               => stmpar%morpar%morft
    morfac              => stmpar%morpar%morfac
!    sus                 => gdp%gdmorpar%sus
!    bed                 => gdp%gdmorpar%bed
    bed                 => stmpar%morpar%bed
    tmor                => stmpar%morpar%tmor
!    thetsd              => gdp%gdmorpar%thetsd
!    sedthr              => gdp%gdmorpar%sedthr
!    hmaxth              => gdp%gdmorpar%hmaxth
!    mergehandle         => gdp%gdmorpar%mergehandle
!    itmor               => gdp%gdmorpar%itmor
!    bcmfile             => gdp%gdmorpar%bcmfile
!    morbnd              => gdp%gdmorpar%morbnd
!    mergebuf            => gdp%gdmorpar%mergebuf
    bedupd              => stmpar%morpar%bedupd
!    cmpupd              => gdp%gdmorpar%cmpupd
!    neglectentrainment  => gdp%gdmorpar%neglectentrainment
!    multi               => gdp%gdmorpar%multi
!    wind                => gdp%gdprocs%wind
!    temp                => gdp%gdprocs%temp
!    const               => gdp%gdprocs%const
!    dredge              => gdp%gdprocs%dredge
!    struct              => gdp%gdprocs%struct
!    sedim               => gdp%gdprocs%sedim
!    snelli              => gdp%gdprocs%snelli
!    scour               => gdp%gdscour%scour
!    factor              => gdp%gdscour%factor
!    slope               => gdp%gdscour%slope
!    bc_mor_array        => gdp%gderosed%bc_mor_array
    dbodsd              => sedtra%dbodsd
!    dm                  => gdp%gderosed%dm
!    dg                  => gdp%gderosed%dg
!    fixfac              => gdp%gderosed%fixfac
!    frac                => gdp%gderosed%frac
!    kfsed               => gdp%gderosed%kfsed
!    kmxsed              => gdp%gderosed%kmxsed
!    mudfrac             => gdp%gderosed%mudfrac
!    sbuuc               => gdp%gderosed%e_sbnc
!    sbvvc               => gdp%gderosed%e_sbtc
!    ssuuc               => gdp%gderosed%e_ssnc
!    ssvvc               => gdp%gderosed%e_sstc
!    sucor               => gdp%gderosed%e_scrn
!    svcor               => gdp%gderosed%e_scrt
    sinkse              => sedtra%sinkse
    sourse              => sedtra%sourse
!    nmudfrac            => gdp%gdsedpar%nmudfrac
!    rhosol              => gdp%gdsedpar%rhosol
    cdryb               => stmpar%sedpar%cdryb
!    sedtyp              => gdp%gdsedpar%sedtyp
    sedtyp              => stmpar%sedpar%sedtyp
!    julday              => gdp%gdinttim%julday
!    ntstep              => gdp%gdinttim%ntstep
!    fluxu               => gdp%gdflwpar%fluxu
!    fluxv               => gdp%gdflwpar%fluxv
!    duneheight          => gdp%gdbedformpar%duneheight
!    iflufflyr           => gdp%gdmorpar%flufflyr%iflufflyr
!    mfluff              => gdp%gdmorpar%flufflyr%mfluff
!    sinkf               => gdp%gdmorpar%flufflyr%sinkf
!    sourf               => gdp%gdmorpar%flufflyr%sourf


    lsed                => stmpar%lsedsus
    lsedtot             => stmpar%lsedtot
    e_sbcn              => sedtra%e_sbcn
    e_sbct              => sedtra%e_sbct
    depchg              => mtd%depchg
    
!    !
!    lstart  = max(lsal, ltem)
    bedload = .false.
    dtmor   = dts*morfac
!    nm_pos  = 1
!    !
!    ! parallel case: exchange arrays in the overlapping regions
!    ! other arrays are their overlap data exchanged in erosed.f90
!    !
!    call dfexchg( fixfac,1, lsedtot, dfloat, nm_pos, gdp)
!    call dfexchg( frac,  1, lsedtot, dfloat, nm_pos, gdp)
!    if (lsed > 0) then
!       call dfexchg( aks,   1, lsed,    dfloat, nm_pos, gdp)
!       call dfexchg( rca,   1, lsed,    dfloat, nm_pos, gdp)
!    endif
!    !
!    !   Calculate suspended sediment transport correction vector (for SAND)
!    !   Note: uses GLM velocites, consistent with DIFU
!    !
!    !   Correct suspended sediment transport rates by estimating the
!    !   quantity of suspended sediment transported in the grid cells below
!    !   Van Rijn's reference height (aks) and making a vector of this in the
!    !   opposite direction to the suspended sediment transport.
!    !
!    !   ensure suspended sediment correction arrays and suspended sediment
!    !   vector arrays are blank
!    !
!    !
!    sucor = 0.0_fp
!    svcor = 0.0_fp
!    ssuu  = 0.0_fp
!    ssvv  = 0.0_fp
!    !
!    ! calculate corrections
!    !
!    if (sus /= 0.0_fp) then
!       !
!       ! suspension transport correction vector only for 3D
!       !
!       if (kmax > 1) then
!          do l = 1, lsed
!             ll = lstart + l
!             if (sedtyp(l) == SEDTYP_NONCOHESIVE_SUSPENDED) then
!                !
!                call dfexchg( fluxu(:,:,ll) ,1, kmax, dfloat, nm_pos, gdp)
!                call dfexchg( fluxv(:,:,ll) ,1, kmax, dfloat, nm_pos, gdp)
!                do nm = 1, nmmax
!                   nmu = nm + icx
!                   num = nm + icy
!                   !
!                   ! try new approach - should be smoother
!                   ! don't worry about direction of the flow
!                   ! use concentration at velocity point=average of the
!                   ! two adjacent concentrations
!                   ! use aks height at velocity point = average of the
!                   ! two adjacent aks values
!                   !
!                   ! note correction vector only computed for velocity
!                   ! points with active sediment cells on both sides
!                   !
!                   ! u direction
!                   !
!                   if ((kfu(nm)*kfsed(nm)*kfsed(nmu)) /= 0) then
!                      cumflux = 0.0_fp
!                      if (kcs(nmu) == 3 .or. kcs(nmu) == -1) then
!                         aksu = aks(nm, l)
!                      elseif (kcs(nm) == 3 .or. kcs(nm) == -1) then
!                         aksu = aks(nmu, l)
!                      else
!                         aksu = (aks(nm, l) + aks(nmu, l))/2.0
!                      endif
!                      !
!                      ! work up through layers integrating transport
!                      ! below aksu
!                      !
!                      do k = kmax, 1, -1
!                         kvalue = k
!                         htdif  = aksu/hu(nm) - (1.0 + sig(k) - thick(k)/2.0)
!                         !
!                         ! if layer containing aksu
!                         !
!                         if (htdif <= thick(k)) then
!                            cumflux = cumflux + fluxu(nm, k, ll)*htdif/thick(k)
!                            exit
!                         else
!                            cumflux = cumflux + fluxu(nm, k, ll)
!                         endif
!                      enddo
!                      cumflux = cumflux / guu(nm)
!                      !
!                      ! integration finished
!                      ! suspended transport correction = opposite of the
!                      ! transport below aksu
!                      !
!                      ! Approximation of the additional transport between bottom of
!                      ! kmaxsd layer and za has been included in the correction vector
!                      !
!                      k = kvalue
!                      if (k > 1) then
!                         if (kcs(nmu) == 3 .or. kcs(nmu) == -1) then
!                            !
!                            ! correction for domain decomposition:
!                            !
!                            ceavg = rca(nm, l)
!                         elseif (kcs(nm) == 3 .or. kcs(nm) == -1) then
!                            ceavg = rca(nmu, l)
!                         else
!                            ceavg = (rca(nm, l) + rca(nmu, l))/2.0_fp
!                         endif
!                         r1avg = (r1(nm, k - 1, ll) + r1(nmu, k - 1, ll))/2.0
!                       if (ceavg>r1avg*1.1 .and. ceavg>0.05) then
!                            z      = (1.0 + sig(k - 1))*hu(nm)
!                            apower = log(max(r1avg/ceavg,1.0e-5_fp))/log(z/aksu)
!                            z      = (1.0 + sig(k - 1) - 0.5*thick(k - 1))*hu(nm)
!                            dz     = (thick(k)-htdif)*hu(nm)
!                            if (apower>-1.05 .and. apower<=-1.0) then
!                               apower = -1.05
!                            elseif (apower>=-1.0 .and. apower<-0.95) then
!                               apower = -0.95
!                            else
!                            endif
!                            apower = max(-10.0_fp , apower)
!                            cavg1  = (ceavg/(apower+1.0)) * (1.0/aksu)**apower
!                            cavg2  = z**(apower+1.0) - aksu**(apower+1.0)
!                            cavg   = cavg1 * cavg2 / dz
!                            cumflux= cumflux - u1(nm,k)*(cavg-r1avg)*dz
!                         endif
!                      endif
!                      sucor(nm,l) = -cumflux
!                      !
!                      ! bedload will be reduced in case of sediment transport
!                      ! over a non-erodible layer (no sediment in bed) in such
!                      ! a case, the suspended sediment transport vector must
!                      ! also be reduced.
!                      !
!                      if ((sucor(nm,l)>0.0_fp .and. kcs(nm)==1) .or. kcs(nmu)/=1) then
!                         sucor(nm,l) = sucor(nm,l) * fixfac(nm,l)
!                      else
!                         sucor(nm,l) = sucor(nm,l) * fixfac(nmu,l)
!                      endif
!                   endif
!                   !
!                   ! v direction
!                   !
!                   if ((kfv(nm)*kfsed(nm)*kfsed(num)) /= 0) then
!                      cumflux = 0.0_fp
!                      if (kcs(num) == 3 .or. kcs(num) == -1) then
!                         aksu = aks(nm, l)
!                      elseif (kcs(nm) == 3 .or. kcs(nm) == -1) then
!                         aksu = aks(num, l)
!                      else
!                         aksu = (aks(nm, l)+aks(num, l)) / 2.0_fp
!                      endif
!                      !
!                      ! work up through layers integrating transport
!                      ! below aksu
!                      !
!                      do k = kmax, 1, -1
!                         kvalue = k
!                         htdif  = aksu/hv(nm) - (1.0 + sig(k) - thick(k)/2.0)
!                         !
!                         ! if layer containing aksu
!                         !
!                         if (htdif <= thick(k)) then
!                            cumflux = cumflux + fluxv(nm,k,ll)*htdif/thick(k)
!                            exit
!                         else
!                            cumflux = cumflux + fluxv(nm,k,ll)
!                         endif
!                      enddo
!                      cumflux = cumflux / gvv(nm)
!                      !
!                      ! integration finished
!                      ! suspended transport correction = opposite of the
!                      ! transport below aksu
!                      !
!                      ! Approximation of the additional transport between bottom of
!                      ! kmaxsd layer and za has been included in the correction vector
!                      !
!                      k = kvalue
!                      if (k > 1) then
!                         if (kcs(num) == 3 .or. kcs(num) == -1) then
!                            !
!                            ! correction for domain decomposition:
!                            !
!                            ceavg = rca(nm,l)
!                         elseif (kcs(nm) == 3 .or. kcs(nm) == -1) then
!                            ceavg = rca(num,l)
!                         else
!                            ceavg = (rca(nm,l)+rca(num,l)) / 2.0
!                         endif
!                         r1avg = (r1(nm,k-1,ll)+r1(num,k-1,ll)) / 2.0
!                         if (ceavg>r1avg*1.1 .and. ceavg>0.05) then
!                            z      = (1.0+sig(k-1)) * hv(nm)
!                            apower = log(max(r1avg/ceavg,1.0e-5_fp))/log(z/aksu)
!                            z      = (1.0+sig(k-1)-0.5*thick(k-1)) * hv(nm)
!                            dz     = (thick(k)-htdif) * hv(nm)
!                            if (apower>-1.05 .and. apower<=-1.0) then
!                               apower = -1.05
!                            elseif (apower>=-1.0 .and. apower<-0.95) then
!                               apower = -0.95
!                            else
!                            endif
!                            apower = max(-10.0_fp , apower)
!                            cavg1  = (ceavg/(apower+1.0)) * (1/aksu)**apower
!                            cavg2  = z**(apower+1.0) - aksu**(apower+1.0)
!                            cavg   = cavg1 * cavg2 / dz
!                            cumflux= cumflux - v1(nm,k)*(cavg-r1avg)*dz
!                         endif
!                      endif
!                      svcor(nm, l) = -cumflux
!                      !
!                      ! bedload will be reduced in case of sediment transport
!                      ! over a non-erodible layer (no sediment in bed) in such
!                      ! a case, the suspended sediment transport vector must
!                      ! also be reduced.
!                      !
!                      if ((svcor(nm,l) > 0.0_fp .and. kcs(nm)==1) .or. kcs(num)/=1) then
!                         svcor(nm, l) = svcor(nm, l)*fixfac(nm, l)
!                      else
!                         svcor(nm, l) = svcor(nm, l)*fixfac(num, l)
!                      endif
!                   endif
!                enddo ! nm
!             endif    ! sedtyp = SEDTYP_NONCOHESIVE_SUSPENDED
!          enddo       ! l
!       endif          ! kmax>1
!       !
!       if (lsed > 0) then
!          call dfexchg( sucor,   1, lsed, dfloat, nm_pos, gdp)
!          call dfexchg( svcor,   1, lsed, dfloat, nm_pos, gdp)
!       endif
!       !
!       ! Calculate suspended sediment transport vector components for
!       ! output
!       ! Note: uses DIFU fluxes
!       ! if suspended sediment vector is required this half timestep
!       ! note, will be required if nst.ge.itmor for cumulative
!       ! transports
!       !
!       if (sscomp .or. nst>=itmor) then
!          do l = 1, lsed
!             ll = lstart + l
!             do nm = 1, nmmax
!                nmu = nm + icx
!                num = nm + icy
!                !
!                ! u component
!                !
!                if (kfu(nm) == 1 .and. kcu(nm)/=-1) then
!                   cumflux = 0.0_fp
!                   do k = 1, kmax
!                      cumflux = cumflux + fluxu(nm, k, ll)
!                   enddo
!                   !
!                   ! total suspended transport
!                   !
!                   ssuu(nm, l) = cumflux/guu(nm) + sucor(nm, l)
!                endif
!                !
!                ! v component
!                !
!                if (kfv(nm) == 1 .and. kcv(nm)/=-1) then
!                   cumflux = 0.0_fp
!                   do k = 1, kmax
!                      cumflux = cumflux + fluxv(nm, k, ll)
!                   enddo
!                   !
!                   ! total suspended transport
!                   !
!                   ssvv(nm, l) = cumflux/gvv(nm) + svcor(nm, l)
!                endif
!             enddo  ! nm
!          enddo     ! l
!       endif        ! sscomp .or. nst>=itmor
!    endif           ! sus /= 0.0
!    !
!    if (lsed > 0) then
!       call dfexchg( ssuu,   1, lsed, dfloat, nm_pos, gdp)
!       call dfexchg( ssvv,   1, lsed, dfloat, nm_pos, gdp)
!    endif
    !
    ! if morphological computations have started
    !
!    if (nst >= itmor) 
    if (time1 > tstart_user + tmor * 60._fp) then   ! tmor in minutes since start of computations, time1 in seconds since reference date
!       !
!       ! Increment morphological time
!       ! Note: dtmor in seconds, morft in days!
!       !
       morft = morft + dtmor/86400.0_fp
!       !
!       ! Bed boundary conditions: transport condition
!       !
!       do jb = 1, nto
!          icond = morbnd(jb)%icond
!          if (icond == 4 .or. icond == 5) then
!             !
!             ! Open boundary with transport boundary condition:
!             ! Get data from table file
!             !
!             call flw_gettabledata(bcmfile  , morbnd(jb)%ibcmt(1) , &
!                      & morbnd(jb)%ibcmt(2) , morbnd(jb)%ibcmt(3) , &
!                      & morbnd(jb)%ibcmt(4) , bc_mor_array        , &
!                      & timhr      ,julday  , gdp        )
!             !
!             ! Prepare loop over boundary points
!             !
!             do ib = 1, morbnd(jb)%npnt
!                alfa_dist   = morbnd(jb)%alfa_dist(ib)
!                alfa_mag    = morbnd(jb)%alfa_mag(ib)
!                idir_scalar = morbnd(jb)%idir(ib)
!                nm          = morbnd(jb)%nm(ib)
!                nxmx        = morbnd(jb)%nxmx(ib)
!                !
!                nmu = nm + icx
!                num = nm + icy
!                nmd = nm - icx
!                ndm = nm - icy
!                !
!                ! If the computed transport is directed outward, do not
!                ! impose the transport rate (at outflow boundaries the
!                ! "free bed level boundary" condition is imposed. This
!                ! check is carried out for each individual boundary point.
!                !
!                ! Detect the case based on the value of nxmx.
!                !
!                if (nxmx == nmu) then
!                   if (umean(nm)<0.0_fp) cycle
!                elseif (nxmx == nmd) then
!                   if (umean(nmd)>0.0_fp) cycle
!                elseif (nxmx == num) then
!                   if (vmean(nm)<0.0_fp) cycle
!                elseif (nxmx == ndm) then
!                   if (vmean(ndm)>0.0_fp) cycle
!                endif
!                !
!                ! The velocity/transport points to the left and top are part
!                ! of this cell. nxmx contains by default the index of the
!                ! neighbouring grid cell, so that has to be corrected. This
!                ! correction is only carried out locally since we need the
!                ! unchanged nxmx value further down for the bed level updating
!                !
!                if (nxmx == nmu .or. nxmx == num) nxmx = nm
!                !
!                li      = 0
!                lsedbed = lsedtot - nmudfrac
!                do l = 1, lsedtot
!                   !
!                   ! bed load transport always zero for mud fractions
!                   !
!                   if (sedtyp(l) == SEDTYP_COHESIVE) cycle
!                   li = li + 1
!                   !
!                   if (morbnd(jb)%ibcmt(3) == lsedbed) then
!                      rate = bc_mor_array(li)
!                   elseif (morbnd(jb)%ibcmt(3) == 2*lsedbed) then
!                      rate = bc_mor_array(li) + &
!                           & alfa_dist * (bc_mor_array(li+lsedbed)-bc_mor_array(li))
!                   endif
!                   rate = alfa_mag * rate
!                   !
!                   if (icond == 4) then
!                      !
!                      ! transport including pores
!                      !
!                      rate = rate*cdryb(l)
!                   else
!                      !
!                      ! transport excluding pores
!                      !
!                      rate = rate*rhosol(l)
!                   endif
!                   !
!                   ! impose boundary condition
!                   !
!                   if (idir_scalar == 1) then
!                      sbuu(nxmx, l) = rate
!                   else
!                      sbvv(nxmx, l) = rate
!                   endif
!                enddo ! l (sediment fraction)
!             enddo    ! ib (boundary point)
!          endif       ! icond = 4 or 5 (boundary with transport condition)
!       enddo          ! jb (open boundary) 
!       !
!       call dfexchg(sbuu, 1, lsedtot, dfloat, nm_pos, gdp)
!       call dfexchg(sbvv, 1, lsedtot, dfloat, nm_pos, gdp)
       !
       ! Update quantity of bottom sediment
       !
       dbodsd = 0.0_fp
       !
       ! compute change in bodsed (dbodsd)
       !
!       bedchangemesscount = 0
       do l = 1, lsedtot
          bedload = sedtyp(l)==SEDTYP_NONCOHESIVE_TOTALLOAD
!          ll = lstart + l
!          do nm = 1, nmmax
          do nm=1,Ndxi
!             !
!             ! note: do not update bottom sediment at open boundary pnts
!             !
!             if (kcs(nm)*kfs(nm) /= 1) cycle
!             !
!             nmu     = nm + icx
!             num     = nm + icy
!             nmd     = nm - icx
!             ndm     = nm - icy
             trndiv  = 0.0_fp
             sedflx  = 0.0_fp
             eroflx  = 0.0_fp
!             gsqsinv = 1.0_fp/gsqs(nm)
!             if (sus/=0.0_fp .and. .not. bedload) then
!                if (neglectentrainment) then
!                   !
!                   ! mass balance based on fluxes: entrainment and deposition
!                   ! does not lead to erosion/sedimentation.
!                   !
!                   if (snelli) then
!                     !
!                     ! Only cross-shore component
!                     !
!                     trndiv = trndiv + gsqsinv                                   &
!                            & *(  ssuu(nmd, l)*guu(nmd) - ssuu(nm, l)*guu(nm))
!                   else
!                     trndiv = trndiv + gsqsinv                                   &
!                            & *(  ssuu(nmd, l)*guu(nmd) - ssuu(nm, l)*guu(nm)    &
!                            &   + ssvv(ndm, l)*gvv(ndm) - ssvv(nm, l)*gvv(nm))
!                   endif
!                else
!                   !
!                   ! mass balance includes entrainment and deposition
!                   !
!                   if (sedtyp(l) == SEDTYP_NONCOHESIVE_SUSPENDED) then
!                      !
!                      ! l runs from 1 to lsedtot, kmxsed is defined for 1:lsed
!                      ! The first lsed fractions are the suspended fractions,
!                      ! so this goes right
!                      !
!                      k = kmxsed(nm, l)
!                   else
!                      k = kmax
!                   endif
!                   thick0 = volum0(nm,k) * gsqsinv
!                   thick1 = volum1(nm,k) * gsqsinv
!                   sedflx = sinkse(nm,l) * r1(nm,k,ll) * thick1
!                   eroflx = sourse(nm,l)               * thick0
!                   !
!                   ! Update fluff layer
!                   !
!                   if (iflufflyr>0) then
!                      mfluff(l, nm) = mfluff(l, nm) + &
!                                    & dt*(  sinkf(l, nm)*r1(nm, k, ll)*thick1   &
!                                    &     - sourf(l, nm)              *thick0  )
!                   endif
!                   !
!                   ! add suspended transport correction vector
!                   !
!                   if (snelli) then
!                      !
!                      ! Only cross-shore component
!                      !
!                      trndiv = trndiv + gsqsinv                                     &
!                             &         *( sucor(nmd,l)*guu(nmd)-sucor(nm,l)*guu(nm))
!                   else
!                      trndiv = trndiv + gsqsinv                                     &
!                             &         *( sucor(nmd,l)*guu(nmd)-sucor(nm,l)*guu(nm) &
!                             &           +svcor(ndm,l)*gvv(ndm)-svcor(nm,l)*gvv(nm))
!                   endif
!                endif
!             endif
             if (bed /= 0.0_fp) then
!                if (snelli) then
!                  !
!                  ! Only cross-shore component
!                  !
!                  trndiv = trndiv + gsqsinv                                     &
!                         &         *( sbuu(nmd,l)*guu(nmd)-sbuu(nm,l)*guu(nm))
!                else
!                  trndiv = trndiv + gsqsinv                                     &
!                         &         *( sbuu(nmd,l)*guu(nmd)-sbuu(nm,l)*guu(nm)   &
!                         &           +sbvv(ndm,l)*gvv(ndm)-sbvv(nm,l)*gvv(nm))
                   sumflux = 0d0 
                   do ii=1,nd(nm)%lnx
                      LL = nd(nm)%ln(ii)
                      Lf = iabs(LL)
                      flux = e_sbcn(Lf,l)*wu(Lf)
                     
                      if ( LL.gt.0 ) then  ! inward
                         sumflux = sumflux + flux
                      else                 ! outward
                         sumflux = sumflux - flux
                      end if
                   end do
                   trndiv = trndiv + sumflux * bai(nm)
!                endif
             endif
!             !
             dsdnm = (trndiv+sedflx-eroflx) * dtmor
!             !
!             ! Warn if bottom changes are very large,
!             ! depth change NOT LIMITED
!             !
!             dhmax = 0.05_fp
!             h1 = max(0.01_fp, s1(nm) + real(dps(nm),fp))
!             if (abs(dsdnm) > dhmax*h1*cdryb(1) .and. bedupd) then
!                !
!                ! Only write bed change warning when bed updating is true
!                ! (otherwise no problem)
!                ! Limit the number of messages with bedchangemessmax
!                ! (otherwise tri-diag will grow very fast)
!                !
!                bedchangemesscount = bedchangemesscount + 1
!                if (bedchangemesscount <= bedchangemessmax) then
!                   call nm_to_n_and_m(nm, n, m, gdp)
!                   write (lundia, '(a,f5.1,a,i0,a,i0,a,i0,a)') &
!                       & '*** WARNING Bed change exceeds ' , dhmax*100.0_fp, ' % of waterdepth after ', ntstep,  &
!                       & ' timesteps, location (m,n) = (', m,',',n,')'
!                endif
!             endif
!             !
!             ! Update dbodsd value at nm
!             !
             dbodsd(l, nm) = dbodsd(l, nm) + dsdnm
!             !
!             call updwaqflxsed(nst, nm, l, trndiv, sedflx, eroflx, gdp)
          enddo    ! nm
       enddo       ! l
       
!       if (bedchangemesscount > bedchangemessmax) then
!          write (lundia,'(12x,a,i0,a)') 'Bed change messages skipped (more than ',bedchangemessmax,')'
!          write (lundia,'(12x,2(a,i0))') 'Total number of Bed change messages for timestep ',ntstep,' : ',bedchangemesscount
!       endif
!       !
!       call fluff_burial(gdp%gdmorpar%flufflyr, dbodsd, lsed, lsedtot, gdp%d%nmlb, gdp%d%nmub, dt, morfac)
!       !
!       nm_pos = 2
!       call dfexchg(dbodsd, 1, lsedtot, dfloat, nm_pos, gdp)
!       nm_pos = 1
!       !
!       ! Re-distribute erosion near dry and shallow points to allow erosion
!       ! of dry banks
!       !
!       do nm = 1, nmmax
!          !
!          ! If this is a cell in which sediment processes are active then ...
!          !
!          if (kcs(nm)*kfs(nm)*kfsed(nm) /= 1) cycle
!          !
!          nmu = nm + icx
!          num = nm + icy
!          nmd = nm - icx
!          ndm = nm - icy
!          totdbodsd = 0.0_fp
!          do l = 1, lsedtot
!             totdbodsd = totdbodsd + dbodsd(l, nm)
!          enddo
!          !
!          ! If this is a cell in erosion is occuring (accretion is not
!          ! distributed to dry points) then...
!          !
!          if (totdbodsd < 0.0_fp) then
!             !
!             ! Note: contrary to the previous implementation, this new
!             ! implementation erodes the sediment from nm and
!             ! re-distributes the eroded volume based on the composition
!             ! of the neighbouring cells, replenishing the sediment volume
!             ! at grid point nm with sediment of a different composition
!             ! than that what was eroded. This new implementation is mass
!             ! conserving per fraction. Furthermore, re-distribution takes
!             ! place only in case of net TOTAL erosion, i.e. not of
!             ! individual fractions.
!             !
!             gsqsmin    = gsqs(nm)
!             totfixfrac = 0.0_fp
!             !
!             from_ndm = kfsed(ndm)==0 .and. kcs(ndm) /= 0 .and. kcs(ndm)<3 .and. kcv(ndm)==1 .and. dps(ndm)<dps(nm)
!             if (from_ndm) then
!                gsqsmin = min(gsqsmin,gsqs(ndm))
!                do l = 1, lsedtot
!                   totfixfrac = totfixfrac + fixfac(ndm, l)*frac(ndm, l)
!                enddo
!             endif
!             !
!             from_nmd = kfsed(nmd)==0 .and. kcs(nmd) /= 0 .and. kcs(nmd)<3 .and. kcu(nmd)==1 .and. dps(nmd)<dps(nm)
!             if (from_nmd) then
!                gsqsmin = min(gsqsmin,gsqs(nmd))
!                do l = 1, lsedtot
!                   totfixfrac = totfixfrac + fixfac(nmd, l)*frac(nmd, l)
!                enddo
!             endif
!             !
!             from_nmu = kfsed(nmu)==0 .and. kcs(nmu) /= 0 .and. kcs(nmu)<3 .and. kcu(nm)==1 .and. dps(nmu)<dps(nm)
!             if (from_nmu) then
!                gsqsmin = min(gsqsmin,gsqs(nmu))
!                do l = 1, lsedtot
!                   totfixfrac = totfixfrac + fixfac(nmu, l)*frac(nmu, l)
!                enddo
!             endif
!             !
!             from_num = kfsed(num)==0 .and. kcs(num) /= 0 .and. kcs(num)<3 .and. kcv(nm)==1 .and. dps(num)<dps(nm)
!             if (from_num) then
!                gsqsmin = min(gsqsmin,gsqs(num))
!                do l = 1, lsedtot
!                   totfixfrac = totfixfrac + fixfac(num, l)*frac(num, l)
!                enddo
!             endif
!             !
!             ! Re-distribute THET % of erosion in nm to surrounding cells
!             ! THETSD is a user-specified maximum value, range 0-1
!             !
!             if (totfixfrac > 1.0e-7_fp) then
!                !
!                ! Compute local re-distribution factor THET
!                !
!                if (hmaxth > sedthr) then
!                   h1   = real(dps(nm),fp) + s1(nm)
!                   thet = (h1 - sedthr)/(hmaxth - sedthr)*thetsd
!                   thet = min(thet, thetsd)
!                else
!                   thet = thetsd
!                endif
!                !
!                ! Combine some constant factors in variable THET
!                ! Note: TOTDBODSD<0.0 and thus THET>0.0 !
!                !
!                thet = -gsqsmin * totdbodsd * thet / totfixfrac
!                !
!                do l = 1, lsedtot
!                   !
!                   ! update dbodsd values in this cell and surrounding cells
!                   ! adjust bedload transport rates to include this erosion
!                   ! process.
!                   !
!                   if (from_ndm) then
!                      dv             = thet*fixfac(ndm, l)*frac(ndm, l)
!                      dbodsd(l, ndm) = dbodsd(l, ndm) - dv/gsqs(ndm)
!                      dbodsd(l, nm ) = dbodsd(l, nm ) + dv/gsqs(nm)
!                      sbvv(ndm, l)   = sbvv(ndm, l) + dv/(dtmor*gvv(ndm))
!                   endif
!                   !
!                   if (from_nmd) then
!                      dv             = thet*fixfac(nmd, l)*frac(nmd, l)
!                      dbodsd(l, nmd) = dbodsd(l, nmd) - dv/gsqs(nmd)
!                      dbodsd(l, nm ) = dbodsd(l, nm ) + dv/gsqs(nm)
!                      sbuu(nmd, l)   = sbuu(nmd, l) + dv/(dtmor*guu(nmd))
!                   endif
!                   !
!                   if (from_nmu) then
!                      dv             = thet*fixfac(nmu, l)*frac(nmu, l)
!                      dbodsd(l, nmu) = dbodsd(l, nmu) - dv/gsqs(nmu)
!                      dbodsd(l, nm ) = dbodsd(l, nm ) + dv/gsqs(nm)
!                      sbuu(nm, l)    = sbuu(nm, l) - dv/(dtmor*guu(nm))
!                   endif
!                   !
!                   if (from_num) then
!                      dv = thet*fixfac(num, l)*frac(num, l)
!                      dbodsd(l, num) = dbodsd(l, num) - dv/gsqs(num)
!                      dbodsd(l, nm ) = dbodsd(l, nm ) + dv/gsqs(nm)
!                      sbvv(nm, l)    = sbvv(nm, l) - dv/(dtmor*gvv(nm))
!                   endif
!                enddo ! l
!             endif    ! totfixfrac > 1.0e-7
!          endif       ! totdbodsd < 0.0
!       enddo          ! nm
!       !
!       nm_pos = 2
!       call dfexchg(dbodsd, 1, lsedtot, dfloat, nm_pos, gdp)
!       nm_pos = 1
!       !
!       ! Modifications for running parallel conditions
!       !
!       !
!       if (multi) then
!          i = 0
!          do l = 1, lsedtot
!             do nm = 1, nmmax
!                i = i + 1
!                mergebuf(i) = real(dbodsd(l, nm),hp)
!             enddo
!          enddo
!          call putarray (mergehandle,mergebuf(1:nmmax*lsedtot),nmmax*lsedtot)
!          call getarray (mergehandle,mergebuf(1:nmmax*lsedtot),nmmax*lsedtot)
!          i = 0
!          do l = 1, lsedtot
!             do nm = 1, nmmax
!                i = i + 1
!                dbodsd(l, nm) = real(mergebuf(i),fp)
!             enddo
!          enddo
!       endif
!       !
!       ! Add transports to cumulative transports
!       !
!       do l = 1, lsedtot
!          do nm = 1, nmmax
!             sbuuc(nm, l) = sbuuc(nm, l) + sbuu(nm, l) * dtmor
!             sbvvc(nm, l) = sbvvc(nm, l) + sbvv(nm, l) * dtmor
!          enddo
!       enddo
!       do l = 1, lsed
!          do nm = 1, nmmax
!             ssuuc(nm, l) = ssuuc(nm, l) + ssuu(nm, l) * dtmor
!             ssvvc(nm, l) = ssvvc(nm, l) + ssvv(nm, l) * dtmor
!          enddo
!       enddo
!       !
!       call dfexchg(sbuuc, 1, lsedtot, dfloat, nm_pos, gdp)
!       call dfexchg(sbvvc, 1, lsedtot, dfloat, nm_pos, gdp)
!       if (lsed > 0) then
!          call dfexchg(ssuuc, 1, lsed, dfloat, nm_pos, gdp)
!          call dfexchg(ssvvc, 1, lsed, dfloat, nm_pos, gdp)
!       endif
!       !
!       ! Apply erosion and sedimentation to bookkeeping system
!       !
!       if (cmpupd) then
!          !
!          ! Determine new thickness of transport layer
!          !
!          call compthick(dps       ,s1        , &
!                       & nmmax     ,gdp       )
!          !
!          ! Update layers and obtain the depth change
!          !
!          if (updmorlyr(gdp%gdmorlyr, dbodsd, depchg, gdp%messages) /= 0) then
!             call writemessages(gdp%messages, lundia)
!             call d3stop(1, gdp)
!          else
!             call writemessages(gdp%messages, lundia)
!          endif
!          call lyrdiffusion(gdp%gdmorlyr, dtmor)
!          !
!          ! Apply composition boundary conditions
!          !
!          call bndmorlyr(lsedtot   ,timhr        , &
!                       & nto       ,bc_mor_array , &
!                       & gdp       )
!       else
!          !
!          ! Compute bed level changes without actually updating the bed composition
!          !
          depchg = 0.0_fp
!          do nm = 1, nmmax
          do nm = 1, Ndxi
!             if (kcs(nm)/=0 .and. kcs(nm)<=2) then
                do l = 1, lsedtot
                   depchg(nm) = depchg(nm) + dbodsd(l, nm)/cdryb(l)
                enddo
!             endif
          enddo
!       endif
!       !
!       call dfexchg(depchg, 1, 1, dfloat, nm_pos, gdp)
!       !
!       ! Bed boundary conditions
!       !
!       do jb = 1, nto
!          icond = morbnd(jb)%icond
!          !
!          ! In case of an open boundary with bed level condition
!          ! described by time series: get data from table file
!          !
!          if (icond == 2 .or. icond == 3) then
!             call flw_gettabledata(bcmfile  , morbnd(jb)%ibcmt(1)    , &
!                      & morbnd(jb)%ibcmt(2) , morbnd(jb)%ibcmt(3)    , &
!                      & morbnd(jb)%ibcmt(4) , bc_mor_array           , &
!                      & timhr      ,julday  , gdp        )
!          endif
!          !
!          ! Prepare loop over boundary points
!          !
!          do ib = 1, morbnd(jb)%npnt
!             alfa_dist   = morbnd(jb)%alfa_dist(ib)
!             alfa_mag    = morbnd(jb)%alfa_mag(ib)**2
!             idir_scalar = morbnd(jb)%idir(ib)
!             nm          = morbnd(jb)%nm(ib)
!             nxmx        = morbnd(jb)%nxmx(ib)
!             !
!             nmu = nm + icx
!             num = nm + icy
!             nmd = nm - icx
!             ndm = nm - icy
!             !
!             ! Bed change in open boundary point
!             ! Any boundary condition is changed into a "free bed level
!             ! boundary" if the computed transport is directed outward.
!             !
!             ! Detect the case based on the value of nxmx. In case of a
!             ! diagonal water level boundary, there will be two separate
!             ! entries in the morbnd structure. The sum of alfa_mag(ib)**2
!             ! will be equal to 1.
!             !
!             if (nxmx == nmu) then
!                if (umean(nm)<0.0) icond = 0
!             elseif (nxmx == nmd) then
!                if (umean(nmd)>0.0) icond = 0
!             elseif (nxmx == num) then
!                if (vmean(nm)<0.0) icond = 0
!             elseif (nxmx == ndm) then
!                if (vmean(ndm)>0.0) icond = 0
!             endif
!             !
!             select case(icond)
!             case (0,4,5)
!                !
!                ! outflow or free boundary (0)
!                ! or prescribed transport with pores (4)
!                ! or prescribed transport without pores (5)
!                !
!                depchg(nm) = depchg(nm) + depchg(nxmx) * alfa_mag
!             case (1)
!                !
!                ! fixed bed level: no update
!                !
!                ! depchg(nm) = depchg(nm) + 0.0 * alfa_mag
!             case (2)
!                !
!                ! prescribed depth
!                ! temporarily store "bed levels" in variable "rate"
!                !
!                if (morbnd(jb)%ibcmt(3) == 1) then
!                   rate = bc_mor_array(1)
!                elseif (morbnd(jb)%ibcmt(3) == 2) then
!                   rate = bc_mor_array(1) + &
!                        & alfa_dist * (bc_mor_array(2)-bc_mor_array(1))
!                endif
!                !
!                depchg(nm) = depchg(nm) + (real(dps(nm),fp)-rate) * alfa_mag
!             case (3)
!                !
!                ! prescribed depth change rate
!                !
!                if (morbnd(jb)%ibcmt(3) == 1) then
!                   rate = bc_mor_array(1)
!                elseif (morbnd(jb)%ibcmt(3) == 2) then
!                   rate = bc_mor_array(1) + &
!                        & alfa_dist * (bc_mor_array(2)-bc_mor_array(1))
!                endif
!                !
!                depchg(nm) = depchg(nm) - rate * alfa_mag * dtmor
!             end select
!          enddo ! ib (boundary point)
!       enddo    ! jb (open boundary)
!    else
!       !
!       ! if morphological computations haven't started yet
!       !
!       do nm = 1, nmmax
!          depchg(nm) = 0.0_fp
!       enddo
    endif ! nst >= itmor
!    !
!    call dfexchg(depchg, 1, 1, dfloat, nm_pos, gdp)
!    !
!    ! Update bottom elevations
!    !
    if (bedupd) then
!       !
!       ! note: dps and dp are positive downwards.
!       !
!       do nm = 1, nmmax
       do nm = 1, Ndxi
!          !
!          ! note: if kcs(nm)=0 then depchg(nm)=0.0
!          ! should change to following test because depchg may be small
!          ! due to truncation errors
!          !
          if (abs(depchg(nm)) > 0.0_fp) then
!             dps(nm) = dps(nm) - real(depchg(nm),prec)
              bl(nm) = bl(nm) + depchg(nm)
          endif
       enddo
!       if (scour) then
!          !
!          ! -Check bottom slopes and apply an avalance effect if needed
!          ! -Depths at waterlevel points (dps) will be updated,
!          !  to be used for dpu and dpv
!          ! -Depth changes will be added to depchg,to be used for dp
!          !
!          call avalan(dps       ,depchg    ,gvu       ,guv       , &
!                    & icx       ,icy       ,gsqs      ,kcs       ,gdp       )
!       endif
!       do nm = 1, nmmax
!          !
!          ! note: if kcs(nm)=0 then depchg(nm)=0.0
!          ! should change to following test because depchg may be small
!          ! due to truncation errors
!          !
!          if (abs(depchg(nm)) >= 0.0) then
!             s1(nm) = max(s1(nm), -real(dps(nm),fp))
!             s0(nm) = max(s0(nm), -real(dps(nm),fp))
!             !
!             ! if dry cells are eroded then bring water level down to
!             ! bed or maximum water level in surrounding wet cells
!             ! (whichever is higher)
!             !
!             if (kfs(nm) == 0) then
!                s1(nm) = s1(nm) + depchg(nm)
!                s0(nm) = s0(nm) + depchg(nm)
!             endif
!          endif
!          !
!          ! set flag for updating dp points below (note does not = 2 at
!          ! open boundaries)
!          !
!          if (kcs(nm) == 0) then
!             kcsbot(nm) = 0
!          else
!             kcsbot(nm) = 1
!          endif
!       enddo
!       !
!       call dfexchg( kcsbot, 1, 1, dfint, nm_pos, gdp)
!       !
!       ! Dredging and Dumping
!       !
!       if (dredge) then
!          call dredgedump(dbodsd    ,cdryb     ,nst       ,timhr     ,morft     , &
!                        & gdp       )
!       endif
    endif
!    ! -----------------------------------------------------------
!    ! DD_mapper: copy dps and depchg at zeta points
!    ! -----------------------------------------------------------
!    nhystp = nxtstp(d3dflow_bottom3d, gdp)
!    if (bedupd) then
!       !
!       ! CALDPU is called after BOTT3D in TRISOL when BEDUPD = TRUE
!       ! instead of updating dpu/dpv here
!       !
!       ! Update dp points
!       !
!       do nm = 1, nmmax
!          nmu  = nm  + icx
!          num  = nm  + icy
!          numu = num + icx
!          fact =   kcsbot(nm) *gsqs(nm)  + kcsbot(num) *gsqs(num)  &
!               & + kcsbot(nmu)*gsqs(nmu) + kcsbot(numu)*gsqs(numu)
!          if (fact > 0.0_fp) then
!             dp(nm) = dp(nm) - (  depchg(nm) *gsqs(nm)  + depchg(num) *gsqs(num)     &
!                    &           + depchg(nmu)*gsqs(nmu) + depchg(numu)*gsqs(numu))/fact
!          endif
!       enddo
!       call dfexchg(dp, 1, 1, dfloat, nm_pos, gdp)
!    endif
end subroutine fm_bott3d
