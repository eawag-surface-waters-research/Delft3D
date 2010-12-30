subroutine z_uzd(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
               & icy       ,nsrc      ,kcs       ,kcs45     ,kcscut    , &
               & kcu       ,kfu       ,kfuz1     ,kfumin    ,kfumax    , &
               & kfv       ,kfvz1     ,kfs       ,kfsz1     ,kfsmin    , kfsmax    , &
               & u0        ,v1        ,w1        ,hu        ,dzu1      , &
               & guu       ,gvv       ,gvu       ,guv       ,gsqs      , &
               & gud       ,gvd       ,guz       ,gvz       ,gsqiu     , &
               & disch     ,umdis     ,kspu      ,mnksrc    ,dismmt    , &
               & aak       ,bbk       ,cck       ,ddk       ,bdx       , &
               & bux       ,bdy       ,buy       , &
               & vicuv     ,vnu2d     ,vicww     ,tgfsep    , &
               & drhodx    ,wsu       ,taubpu    ,taubsu    ,rxx       , &
               & rxy       ,windu     ,patm      ,fcorio    ,p0        , &
               & ubrlsu    ,pship     ,diapl     ,rnpl      ,nst       , &
               & u1        ,s0        ,cfurou    ,uvdwk     ,vvdwk     , &
               & norow     ,irocol    ,gdp       )
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
! This subroutine evaluates/solves at each half time
! step the momentum equation with implicit advection
! approximation, and implicit diffusion in the ver-
! tical direction.
! Reference : A.O.I. - scheme (G.S. Stelling and
! J.J. Leendertse, Approximation of Convective
! Processes by Cyclic AOI Methods, Proceedings,
! ASCE Conference, Tampa, 1991).
! - Horizontal Advection: depending on the flag MOMSOL. Options: 
!    implicit, upwind scheme (IUPW)
!    explicit, multi-directional upwind (MDUE)
!
! - Horizontal Diffusion:                explicit, along Z-planes
! - Vertical Advection :                 implicit, central scheme
! - Vertical Diffusion :                 implicit
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use timers
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'flow_steps_f.inc'
    !    
    real(fp)               , pointer :: hdt
    real(fp)               , pointer :: ag
    integer                , pointer :: iro
    integer                , pointer :: irov
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: vicmol
    character(8)           , pointer :: dpsopt
    real(fp)               , pointer :: eps
    integer                , pointer :: lundia
    real(fp)               , pointer :: dryflc
    real(fp)               , pointer :: gammax
    integer                , pointer :: ibaroc
    logical                , pointer :: cstbnd
    character(6)           , pointer :: momsol
    real(fp)               , pointer :: rhofrac
    logical                , pointer :: wind
    logical                , pointer :: wave
    logical                , pointer :: roller
    logical                , pointer :: dpmveg
!
! Global variables
!
    integer                                                              :: icx    !!  Increment in the X-dir., 
                                                                                   !!  if icx=NMAX then computation proceeds in the X-dir., 
                                                                                   !!  if icx=1    then computation proceeds in the Y-dir.
    integer                                                              :: icy    !!  Increment in the Y-dir. (see icx)
    integer                                                              :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays.
                                                                                   !!  Due to the shift in the 2nd (M-)index, J = -2*NMAX + 1
    integer                                                              :: kmax   !  Description and declaration in iidim.f90
    integer                                                              :: nmmax  !  Description and declaration in dimens.igs
    integer                                                              :: nmmaxj !  Description and declaration in dimens.igs
    integer                                                 , intent(in) :: nsrc   !  Description and declaration in iidim.f90
    integer                                                 , intent(in) :: nst    !!  Time step number
    integer     , dimension(7, nsrc)                        , intent(in) :: mnksrc !  Description and declaration in iidim.f90
    integer                                                              :: norow  !  Description and declaration in iidim.f90
    integer     , dimension(5, norow)                                    :: irocol !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kcs    !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfs    !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: kfsmax !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: kfsmin !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfu    !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfumax !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfumin !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfv    !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)  , intent(in) :: kspu   !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: kcs45
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)    , intent(in) :: kcscut !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: kfsz1  !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kcu    !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: kfuz1  !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: kfvz1  !  Description and declaration in iidim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: fcorio !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: gsqiu  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: gsqs   !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: gud    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: guu    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: guv    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: guz    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: gvd    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: gvu    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: gvv    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: gvz    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: hu     !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: patm   !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: pship  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, 3)                    :: cfurou !  Description and declaration in rjdim.f90    
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: taubpu !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: taubsu !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: tgfsep !!  Water elevation induced by tide generating forces
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: vnu2d  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: windu  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: wsu    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)  , intent(in) :: vicww  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)  , intent(in) :: w1     !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax + 2)             :: vicuv  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: aak    !!  Internal work array, lower diagonal tridiagonal matrix, implicit coupling
                                                                                   !!  of layer velocity in (N,M,K) with layer velocity in (N,M,K-1)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: bbk    !!  Internal work array, coefficient layer velocity in (N,M,K) implicit part
                                                                                   !!  with layer velocity in (N+1,M,K)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: cck    !!  Internal work array, upper diagonal tridiagonal matrix, implicit coupling
                                                                                   !!  of layer velocity in (N,M,K) with layer velocity in (N,M,K+1)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: ddk    !!  Internal work array, diagonal space at (N,M,K)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: bdx    !!  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                                   !!  with layer velocity in (N,M-1,K)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: bdy    !!  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                                   !!  with layer velocity in (N-1,M,K)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: bux    !!  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                                   !!  with layer velocity in (N,M+1,K)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: buy    !!  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                                   !!  with layer velocity in (N+1,M,K)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: diapl  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)    , intent(in) :: dzu1   !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)    , intent(in) :: p0     !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: rnpl   !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: rxx    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: rxy    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: s0     !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)    , intent(in) :: drhodx !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: u0     !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)    , intent(in) :: ubrlsu !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: v1     !  Description and declaration in rjdim.f90
    real(fp)    , dimension(nsrc)                           , intent(in) :: disch  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: u1     !  Description and declaration in rjdim.f90
    real(fp)    , dimension(nsrc)                           , intent(in) :: umdis  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: uvdwk  !!  Internal work array for Jac.iteration
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: vvdwk  !!  Internal work array for Jac.iteration
    character(1), dimension(nsrc)                           , intent(in) :: dismmt !  Description and declaration in ckdim.f90
!
! Local variables
!
    integer            :: ddb
    integer            :: iada
    integer            :: iadc
    integer            :: ibf
    integer            :: ibl
    integer            :: ic
    integer            :: icxy    ! MAX value of ICX and ICY 
    integer            :: idifc
    integer            :: idifd
    integer            :: idifu
    integer            :: isrc
    integer            :: iter
    integer            :: itr
    integer            :: k
    integer            :: kdo
    integer            :: kenm
    integer            :: kfad
    integer            :: kk
    integer            :: kkmax
    integer            :: kmaxtl
    integer            :: kmin
    integer            :: kup
    integer            :: maskval
    integer            :: maxk
    integer            :: mf
    integer            :: ml
    integer            :: n
    integer            :: nmf
    integer            :: nml
    integer            :: ndm
    integer            :: ndmd
    integer            :: ndmu
    integer            :: nm
    integer            :: nmd
    integer            :: nmdis
    integer            :: nmu
    integer            :: num
    integer            :: numd
    integer            :: numu
    integer            :: nhystp
    real(fp)           :: advecx
    real(fp)           :: advecy
    real(fp)           :: adza
    real(fp)           :: adzb
    real(fp)           :: adzc
    real(fp)           :: bdmwrp
    real(fp)           :: bdmwrs
    real(fp)           :: bi
    real(fp)           :: cbot
    real(fp)           :: cuu
    real(fp)           :: cvv
    real(fp)           :: dbk
    real(fp)           :: ddx
    real(fp)           :: ddy
    real(fp)           :: ddza
    real(fp)           :: ddzb
    real(fp)           :: ddzc
    real(fp)           :: dgeta
    real(fp)           :: dgvnm
    real(fp)           :: dux
    real(fp)           :: duy
    real(fp)           :: dz
    real(fp)           :: dzdo
    real(fp)           :: dzup
    real(fp)           :: eps1
    real(fp)           :: ff
    real(fp)           :: geta
    real(fp)           :: getad
    real(fp)           :: getau
    real(fp)           :: gksi
    real(fp)           :: gksid
    real(fp)           :: gksiu
    real(fp)           :: gsqi
    real(fp)           :: hugsqs  ! HU(NM/NMD) * GSQS(NM) Depending on UMDIS the HU of point NM or NMD will be used 
    real(fp)           :: qwind
    real(fp), external :: redvic
    real(fp)           :: smax
    real(fp)           :: timest
    real(fp)           :: uuu
    real(fp)           :: uweir
    real(fp)           :: vih
    real(fp)           :: viznm
    real(fp)           :: viznmd
    real(fp)           :: vvv
    real(fp)           :: www
    real(fp)           :: zz
    character(20)      :: errtxt
!
!! executable statements -------------------------------------------------------
!
    !
    eps        => gdp%gdconst%eps
    lundia     => gdp%gdinout%lundia
    dryflc     => gdp%gdnumeco%dryflc
    gammax     => gdp%gdnumeco%gammax
    hdt        => gdp%gdnumeco%hdt
    ibaroc     => gdp%gdnumeco%ibaroc
    cstbnd     => gdp%gdnumeco%cstbnd
    momsol     => gdp%gdnumeco%momsol
    ag         => gdp%gdphysco%ag
    iro        => gdp%gdphysco%iro
    irov       => gdp%gdphysco%irov
    rhofrac    => gdp%gdphysco%rhofrac
    rhow       => gdp%gdphysco%rhow
    vicmol     => gdp%gdphysco%vicmol
    wind       => gdp%gdprocs%wind
    wave       => gdp%gdprocs%wave
    roller     => gdp%gdprocs%roller
    dpmveg     => gdp%gdprocs%dpmveg
    dpsopt     => gdp%gdnumeco%dpsopt
    !
    if (momsol == 'iupw  ') then
       !
       ! Implicit upwind method (first order) for horizontal advection
       !
       call timer_start(timer_uzd_ini, gdp)
       ddb      = gdp%d%ddbound
       icxy     = max(icx, icy)
       timest   = hdt
       !
       if (icx==1) then
          ff = -1.0_fp
       else
          ff = 1.0_fp
       endif
       kmaxtl = 0
       !
       ! Initialise arrays aak, bbk, cck and ddk for all (nm, k)
       !
       aak  = 0.0_fp
       bbk  = 1.0_fp/timest
       cck  = 0.0_fp
       ddk  = 0.0_fp
       bdx  = 0.0_fp
       bdy  = 0.0_fp
       bux  = 0.0_fp
       buy  = 0.0_fp
       !
       call timer_stop(timer_uzd_ini, gdp)
       !
       ! Fill the array elements and the right hand side
       !
       call timer_start(timer_uzd_rhs, gdp)
       do nm = 1, nmmax
          ndm        = nm - icy
          nmu        = nm + icx
          ndmu       = nm + icx - icy
          gksi       = gvu(nm)
          if (kfu(nm)==1) then
             do k = kfumin(nm), kfumax(nm)
                if (kfuz1(nm, k)==1 .and. kcs(nm)*kcs(nmu)>0) then
                   !
                   ! BAROCLINIC PRESSURE, CORIOLIS, ATMOSPHERIC PRES. and TIDE GEN. FORCE
                   !
                   vvv        = 0.25_fp*(v1(ndm, k) + v1(ndmu, k) + v1(nm, k) + v1(nmu, k))
                   ddk(nm, k) = u0(nm, k)/timest                                   &
                              & - ag*(s0(nmu) - s0(nm))/gksi                       &
                              & + ff*fcorio(nm)*vvv                                &
                              & - ag/rhow*drhodx(nm, k)*kfsz1(nm, k)*kfsz1(nmu, k) &
                              & - (patm(nmu) - patm(nm))/(gksi*rhow)               &
                              & - (pship(nmu) - pship(nm))/(gksi*rhow)             &
                              & + ag*(tgfsep(nmu) - tgfsep(nm))/gksi               &
                              & - (p0(nmu, k) - p0(nm, k))/(gksi*rhow)
                endif
             enddo
          endif
       enddo
       call timer_stop(timer_uzd_rhs, gdp)
       !
       ! Horizontal advection (first order upwind implicit)
       !
       call timer_start(timer_uzd_momsol, gdp)
       call z_impl_upw(nmmax     ,kmax      ,icx       ,icy       ,kcs     , &
                     & kcscut    ,kfu       ,kfuz1     ,kfumin    ,kfumax  , &
                     & kfvz1     ,u0        ,v1        ,guu       ,gvu     , &
                     & gvd       ,guz       ,gsqiu     ,bdx       ,bux     , &
                     & bbk       ,bdy       ,ddk       ,buy       ,gdp     )
       call timer_stop(timer_uzd_momsol, gdp)
       !
       ! 2D Weir not included
       ! energy loss for CDW (remainder structure untested yet)
       !
       call timer_start(timer_uzd_eloss, gdp)
       call usrbrl(icx       ,icy       ,nmmax     ,kmax      ,kfu       , &
                  & kspu      ,gvu       ,u0        ,v1        ,bbk       , &
                  & ubrlsu    ,diapl     ,rnpl      ,gdp       )
       call timer_stop(timer_uzd_eloss, gdp)
       !
       call timer_start(timer_uzd_stress, gdp)
       do nm = 1, nmmax
          nmu  = nm + icx
          kmin = kfumin(nm)
          if (kfu(nm) == 1 .and. kcs(nm)*kcs(nmu) > 0) then
             kkmax = max(1, kfumax(nm))
             !
             ! WIND AND BOTTOM FRICTION
             !
             qwind  = 0.0_fp
             bdmwrp = 0.0_fp
             bdmwrs = 0.0_fp
             !
             ! BOTTOM STRESS DUE TO FLOW AND WAVES
             !
             ! For inundation
             !
             ! Estimate velocity on the basis of local equilibrium by solving u(nm)
             ! with an explicit procedure. So that high velocities can be avoided
             ! during flooding (kfu=1 and u=0.) in regions with steep topography.
             ! Velocity is estimated assuming critical flow over a short-crested
             ! weir.
             ! Gates are excluded
             !
             if (dpsopt == 'DP  ') then
                if (kfu(nm) == 1 .and. abs(u0(nm,kmin)) <= 1.0e-15 .and. kspu(nm, 0) /= 4 .and. kspu(nm, 0) /= 10) then
                   !
                   ! cfurou(nm,1) contains u/u*
                   !
                   uweir      = sqrt(2.0 / 3.0 * ag * max(hu(nm), 0.01_fp))
                   taubpu(nm) = uweir / (cfurou(nm,1)**2)
                endif
             endif
             cbot           = taubpu(nm)
             qwind          = windu(nm)/dzu1(nm, kkmax)
             bdmwrp         = cbot/dzu1(nm, kmin)
             bdmwrs         = taubsu(nm)/dzu1(nm, kmin)
             bbk(nm, kmin)  = bbk(nm, kmin) + bdmwrp
             ddk(nm, kkmax) = ddk(nm, kkmax) - qwind/rhow
             ddk(nm, kmin)  = ddk(nm, kmin) + bdmwrs
             !
             ! WAVE FORCE AT SURFACE
             !
             ddk(nm, kkmax) = ddk(nm, kkmax) + wsu(nm)/(rhow*dzu1(nm, kkmax))
          endif
       enddo
       call timer_stop(timer_uzd_stress, gdp)
       !
       ! DISCHARGE ADDITION OF MOMENTUM
       !
       call timer_start(timer_uzd_dismmt, gdp)
       do isrc = 1, nsrc
          nm = (mnksrc(2, isrc) + ddb) + ((mnksrc(1, isrc) - 1) + ddb)*icxy
          nmd = nm - icx
          if (dismmt(isrc) == 'Y' .and. disch(isrc) > 0.0_fp) then
             if (umdis(isrc) >= 0.0_fp) then
                nmdis  = nm
                hugsqs = hu(nm)*gsqs(nm)
             else
                nmdis  = nmd
                hugsqs = hu(nmd)*gsqs(nm)
             endif
             kk = mnksrc(3, isrc)
             if (kfu(nmdis) == 1) then
                if (kk == 0) then
                   do k = 1, kmax
                      bbk(nmdis, k) = bbk(nmdis, k) + disch(isrc)/hugsqs
                      ddk(nmdis, k) = ddk(nmdis, k) + umdis(isrc)*disch(isrc)      &
                                    & /hugsqs
                   enddo
                else
                   bbk(nmdis, kk) = bbk(nmdis, kk) + disch(isrc)                   &
                                  & /(dzu1(nmdis, kk)*gsqs(nm))
                   ddk(nmdis, kk) = ddk(nmdis, kk) + umdis(isrc)*disch(isrc)       &
                                  & /(dzu1(nmdis, kk)*gsqs(nm))
                endif
             endif
          endif
       enddo
       call timer_stop(timer_uzd_dismmt, gdp)
       !
       ! VERTICAL ADVECTION AND VISCOSITY, IMPLICIT
       !
       call timer_start(timer_uzd_advdiffv, gdp)
       do nm = 1, nmmax
          if (kfumax(nm) > kfumin(nm)) then
             kmaxtl = 1
             nmu    = nm + icx
             do k = kfumin(nm), kfumax(nm)
                if (kfuz1(nm, k) == 1) then
                   kfad = 0
                   kdo  = k - 1
                   kup  = k + 1
                   if (k == kfumin(nm)) then
                      kfad = 1
                      kdo  = k
                   endif
                   if (k == kfumax(nm)) then
                      kfad = -1
                      kup  = k
                   endif
                   !
                   ! Free slip between open and closed layers of a gate
                   !
                   iada = 1
                   iadc = 1
                   if (kspu(nm, 0) == 4 .or. kspu(nm, 0) == 10) then
                      iada = max(1 - (kspu(nm, kdo) + kspu(nm, k)), 0)
                      iadc = max(1 - (kspu(nm, k) + kspu(nm, kup)), 0)
                   endif
                   !
                   dzup = dzu1(nm, k) + dzu1(nm, kup)
                   dzdo = dzu1(nm, k) + dzu1(nm, kdo)
                   !
                   ! ADVECTION IN VERTICAL DIRECTION; W*DU/DZ
                   !
                   !
                   ! Is this correct at the surface when there are no neighbours?
                   !
                   maskval = min(kcs(nm), 2)*min(kcs(nmu), 2)
                   www     = 0.25_fp*maskval*(w1(nm, k - 1) + w1(nm, k) + w1(nmu, k - 1) + w1(nmu, k))
                   if (www < 0.0_fp) then
                      adza = -2.0_fp*www/(dzup + dzdo)*(1 - abs(kfad))
                      adzc =  2.0_fp*www/(dzup + dzdo)*(1 - abs(kfad)) + kfad*(1 + kfad)*www/dzup
                   else
                      adza = -2.0_fp*www/(dzup + dzdo)*(1 - abs(kfad)) + abs(kfad)*( - 1 + kfad)*www/dzdo
                      adzc =  2.0_fp*www/(dzup + dzdo)*(1 - abs(kfad))
                   endif
                   adza = iada*adza
                   adzc = iadc*adzc
                   adzb = -adza - adzc
                   !
                   ! Subtitution in coefficients
                   !
                   aak (nm, k) = adza
                   bbk (nm, k) = bbk(nm, k) + adzb
                   cck (nm, k) = adzc
                   !
                   ! VERTICAL VISCOSITY
                   !
                   !
                   ! viznmd calculation 
                   ! restriction is moved from Z_TURCLO to here
                   !
                   viznmd = 0.25_fp * (2 - kfad*(1 + kfad))                 &
                          & * (2.0_fp*vicmol + redvic(vicww(nm , kdo), gdp) &
                          &                  + redvic(vicww(nmu, kdo), gdp))
                   !
                   ! viznm calculation 
                   ! restriction is moved from Z_TURCLO to here
                   !
                   viznm  = 0.25_fp * (2 + kfad*(1 - kfad))               &
                          & * (2.0_fp*vicmol + redvic(vicww(nm , k), gdp) &
                          &                  + redvic(vicww(nmu, k), gdp))
                   dz    = dzu1(nm, k)
                   !
                   ddza = iada * 2.0_fp * viznmd / (dzdo*dz)
                   ddzc = iadc * 2.0_fp * viznm  / (dzup*dz)
                   ddzb = -ddza - ddzc
                   !
                   ! subtitution in coefficients
                   !
                   aak(nm, k) = aak(nm, k) - ddza
                   bbk(nm, k) = bbk(nm, k) - ddzb
                   cck(nm, k) = cck(nm, k) - ddzc
                endif
             enddo
          endif
       enddo
       call timer_stop(timer_uzd_advdiffv, gdp)
       !
       ! HORIZONTAL VISCOSITY
       !
       call timer_start(timer_uzd_vih, gdp)
       if (irov>0) then
          !
          ! Stresses due to rigid walls
          !     implemented fully explicit
          !
          call z_vihrov(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                      & icy       ,kcs45     ,kcs       ,kfu       ,kfv       , &
                      & kfs       ,u0        ,v1        ,vicuv     ,vnu2d     , &
                      & gud       ,guu       ,gvd       ,gvu       ,gvz       , &
                      & ddk       ,rxx       ,rxy       ,kfuz1     ,kfvz1     , &
                      & kfsz1     ,kfumin    ,kfumax    ,gdp       )
       !
       ! for Crank Nicolson method: is computed here (implicitly for the whole time step)
       ! for fractional step method: is computed in z_cucnp (explicitly for the whole time step)
       !
       else
          do nm = 1, nmmax
             nmd  = nm - icx
             ndm  = nm - icy
             ndmd = nm - icx - icy
             nmu  = nm + icx
             num  = nm + icy
             numu = nm + icx + icy
             ndmu = nm + icx - icy
             numd = nm - icx + icy
             do k = kfumin(nm), kfumax(nm)
                if (kfu(nm) == 1 .and. kcs(nm)*kcs(nmu) > 0) then
                   if (kfuz1(nm, k)==1) then
                      gksid = gvz(nm)
                      gksiu = gvz(nmu)
                      gksi  = gvu(nm)
                      getad = gud(ndm)
                      getau = gud(nm)
                      geta  = guu(nm)
                      idifd = kfvz1(ndm, k)*kfvz1(ndmu, k)*kfuz1(ndm, k)
                      idifu = kfvz1(nm, k)*kfvz1(nmu, k)*kfuz1(num, k)
                      idifc = abs(2 - kcs(nm))*abs(2 - kcs(nmu))
                      !
                      ! EDDY VISCOSITY FOR KMAX = 1, USING LAPLACE OPERATOR
                      ! (2*VIH*(D2U/DX2 + D2U/DY2)
                      ! Vih multiplied by 1.0 (instead of 0.5) because it is included here for the whole time step
                      !
                      vih        = vicuv(nm, k) + vicuv(nmu, k) + vnu2d(nm) + vnu2d(ndm)
                      bbk(nm, k) = bbk(nm, k)                    &
                                 & + 2.0_fp*vih/(gksid*gksiu)*idifc  &
                                 & +        vih/(getau*geta) *idifu  &
                                 & +        vih/(getad*geta) *idifd
                      bux(nm, k) = bux(nm, k) - vih/(gksiu*gksi)*idifc
                      bdx(nm, k) = bdx(nm, k) - vih/(gksid*gksi)*idifc
                      buy(nm, k) = buy(nm, k) - vih/(getau*geta)*idifu
                      bdy(nm, k) = bdy(nm, k) - vih/(getad*geta)*idifd
                   endif
                endif
             enddo
          enddo
       !
       endif
       call timer_stop(timer_uzd_vih, gdp)
       !
       ! BOUNDARY CONDITIONS
       !
       call timer_start(timer_uzd_bouncond, gdp)
       do ic = 1, norow
          !
          n   = irocol(1, ic)
          mf  = irocol(2, ic) - 1
          ml  = irocol(3, ic)
          ibf = irocol(4, ic)
          ibl = irocol(5, ic)
          nmf = (n + ddb)*icy + (mf + ddb)*icx - icxy
          nml = (n + ddb)*icy + (ml + ddb)*icx - icxy
          !
          ! IMPLEMENTATION OF BOUNDARY CONDITIONS
          !
          if (kcu(nmf)*kfu(nmf) == 1) then
             if (ibf==3 .or. ibf==5 .or. ibf==6 .or. ibf==7) then
                do k = kfumin(nmf), kfumax(nmf)
                   aak (nmf, k) = 0.0_fp
                   bbk (nmf, k) = 1.0_fp
                   bux (nmf, k) = 0.0_fp
                   bdx (nmf, k) = 0.0_fp
                   buy (nmf, k) = 0.0_fp
                   bdy (nmf, k) = 0.0_fp
                   cck (nmf, k) = 0.0_fp
                   ddk (nmf, k) = u0(nmf, k)
                enddo
             endif
          endif
          if (kcu(nml)*kfu(nml) == 1) then
             if (ibl==3 .or. ibl==5 .or. ibl==6 .or. ibl==7) then
                do k = kfumin(nml), kfumax(nml)
                   aak (nml, k) = 0.0_fp
                   bbk (nml, k) = 1.0_fp
                   bux (nml, k) = 0.0_fp
                   bdx (nml, k) = 0.0_fp
                   buy (nml, k) = 0.0_fp
                   bdy (nml, k) = 0.0_fp
                   cck (nml, k) = 0.0_fp
                   ddk (nml, k) = u0(nml, k)
                enddo
             endif
          endif
       enddo
       call timer_stop(timer_uzd_bouncond, gdp)
       !
       ! left hand-side is now set by Delft3D-FLOW instead of the mapper
       !
       call timer_start(timer_uzd_lhs, gdp)
       do nm = 1, nmmax
          if (kcu(nm) == 3) then
             do k = kfumin(nm), kfumax(nm)
                aak(nm,k) = 0.0_fp
                bbk(nm,k) = 1.0_fp
                cck(nm,k) = 0.0_fp
                ddk(nm,k) = u0(nm,k)
             enddo
          endif
       enddo
       call timer_stop(timer_uzd_lhs, gdp)
       !
       ! Domain decomposition:
       !        end of "build_system_for_velocity",
       !        mapper can build the coupling equations
       !
       call timer_start(timer_uzd_rest, gdp) 
       if (icx == 1) then
          !
          ! D3dFlowMap_Build_V: poke the coupling equations into system
          !
          nhystp = nxtstp(d3dflow_build_v, gdp)
       else
          !
          ! D3dFlowMap_Build_U: poke the coupling equations into system
          !
          nhystp = nxtstp(d3dflow_build_u, gdp)
       endif
       call timer_stop(timer_uzd_rest, gdp) 
       !
       uvdwk = 0.0_fp
       vvdwk = 0.0_fp
       !
       !***SCALE ROWS OF MATRIX/RIGHT HAND SIDE VECTOR
       !
       call timer_start(timer_uzd_rowsc, gdp)
       do nm = 1, nmmax
          do k = kfumin(nm), kfumax(nm)
             bi          = 1.0_fp / bbk(nm, k)
             aak (nm, k) = aak (nm, k) * bi
             bbk (nm, k) = 1.0_fp
             bux (nm, k) = bux (nm, k) * bi
             bdx (nm, k) = bdx (nm, k) * bi
             buy (nm, k) = buy (nm, k) * bi
             bdy (nm, k) = bdy (nm, k) * bi
             cck (nm, k) = cck (nm, k) * bi
             ddk (nm, k) = ddk (nm, k) * bi
          enddo
       enddo
       call timer_stop(timer_uzd_rowsc, gdp)
       !
       !***SOLUTION PROCEDURE SYSTEM OF EQUATIONS
       !
       call timer_start(timer_uzd_solve1, gdp)
       !
       ! System of equations is reduced for all points
       ! right hand side is reduced within iterative loops
       !
       ! Division by the pivot for k=1 is not needed anymore
       ! because of row scaling
       !
       do nm = 1, nmmax
          do k = kfumin(nm)+1, kfumax(nm)
             bi         = 1.0_fp/(bbk(nm, k) - aak(nm, k)*cck(nm, k - 1))
             bbk(nm, k) = bi
             cck(nm, k) = cck(nm, k)*bi
          enddo
       enddo
       call timer_stop(timer_uzd_solve1, gdp)
       !
       ! ITERATION LOOP
       !
       call timer_start(timer_uzd_solve2, gdp)
       iter = 0
       do nm = 1, nmmax
          do k = kfumin(nm), kfumax(nm)
             u1   (nm, k) = u0(nm, k)
             uvdwk(nm, k) = u0(nm, k)
          enddo
       enddo
       call timer_stop(timer_uzd_solve2, gdp)
       !
       ! Domain decomposition:
       !     resume point for next solve
       !
  222 continue
       gdp%dd%uzditer = gdp%dd%uzditer + 1
       !
       ! End Domain decomposition addition
       !
 1100 continue
       iter = iter + 1
       !
       ! ITERATIVE SOLUTION METHOD USING CHECKERBOARD JACOBI
       ! IN HORIZONTAL DIRECTION
       ! ATTENTION : AN ODD NUMBER OF GRIDPOINTS IN V-DIRECTION
       !             ( NMAX ) IS ASSUMED!!!!!
       !
       itr = 0
       if(icx == 1) then
          call timer_start(timer_uzd_solve3u, gdp)
       else
          call timer_start(timer_uzd_solve5v, gdp)
       end if
       do nm = 1, nmmax, 2
          do k = kfumin(nm), kfumax(nm)
             !
             ! COMPUTE RIGHT HAND SIDE
             !
             uvdwk(nm, k) = ddk(nm,k)                     &
                          & - bdx (nm,k)*u1(nm-icx    ,k) &
                          & - bdy (nm,k)*u1(nm-icy    ,k) &
                          & - buy (nm,k)*u1(nm+icy    ,k) &
                          & - bux (nm,k)*u1(nm+icx    ,k) 
          enddo
       enddo
       if(icx == 1) then
         call timer_stop(timer_uzd_solve3u, gdp)
         call timer_start(timer_uzd_solve4u, gdp)
       else
         call timer_stop(timer_uzd_solve5v, gdp)
         call timer_start(timer_uzd_solve6v, gdp)
       end if
!
       do nm = 1, nmmax, 2
          kmin = kfumin(nm)
          vvdwk(nm, kmin) = uvdwk(nm, kmin)*bbk(nm, kmin)
       enddo
       do nm = 1, nmmax, 2
          do k = kfumin(nm)+1, kfumax(nm)
             vvdwk(nm, k) = (uvdwk(nm, k) - aak(nm, k)*vvdwk(nm, k - 1))*bbk(nm, k)
          enddo
       enddo
       
       do nm = 1, nmmax, 2
          do k = kfumax(nm)-1, kfumin(nm), -1
             vvdwk(nm, k) = vvdwk(nm, k) - cck(nm, k)*vvdwk(nm, k + 1)
          enddo
       enddo
       !
       ! CHECK FOR CONVERGENCE
       !
       smax = 0.0
       do nm = 1, nmmax, 2
          if (kfu(nm) == 1) then
             do k = kfumin(nm), kfumax(nm)
                if (abs(vvdwk(nm,k)-u1(nm,k)) > eps) itr = 1
                zz = abs(vvdwk(nm, k) - u1(nm, k))
                if (zz > smax) then
                   smax = zz
                endif
                u1(nm, k) = vvdwk(nm, k)
             enddo
          endif
       enddo
       !
       if(icx == 1) then
         call timer_stop(timer_uzd_solve4u, gdp)
         call timer_start(timer_uzd_solve3u, gdp)
       else
         call timer_stop(timer_uzd_solve6v, gdp)
         call timer_start(timer_uzd_solve5v, gdp)
       end if
       !
       do nm = 2, nmmax, 2
          do k = kfumin(nm), kfumax(nm)
             !
             ! COMPUTE RIGHT HAND SIDE
             !
             uvdwk(nm, k) = ddk(nm,k)                     &
                          & - bdx (nm,k)*u1(nm-icx    ,k) &
                          & - bdy (nm,k)*u1(nm-icy    ,k) &
                          & - buy (nm,k)*u1(nm+icy    ,k) &
                          & - bux (nm,k)*u1(nm+icx    ,k) 
          enddo
       enddo
       !
       if(icx == 1) then
         call timer_stop(timer_uzd_solve3u, gdp)
         call timer_start(timer_uzd_solve4u, gdp)
       else
         call timer_stop(timer_uzd_solve5v, gdp)
         call timer_start(timer_uzd_solve6v, gdp)
       end if
       !
       do nm = 2, nmmax, 2
          kmin=kfumin(nm)
          vvdwk(nm,kmin) = uvdwk(nm,kmin) * bbk(nm, kmin)
       enddo
       do nm = 2, nmmax, 2
          do k = kfumin(nm)+1, kfumax(nm)
             vvdwk(nm,k) = (uvdwk(nm,k) - aak(nm,k)*vvdwk(nm,k-1)) * bbk(nm,k)
          enddo
       enddo
       do nm = 2, nmmax, 2
          do k = kfumax(nm)-1, kfumin(nm), -1
             vvdwk(nm,k) = vvdwk(nm,k) - cck(nm,k)*vvdwk(nm,k+1)
          enddo
       enddo
       !
       ! CHECK FOR CONVERGENCE
       !
       do nm = 2, nmmax, 2
          if (kfu(nm) == 1) then
             do k = kfumin(nm), kfumax(nm)
                if (abs(vvdwk(nm,k)-u1(nm,k)) > eps) itr = 1
                zz = abs(vvdwk(nm, k) - u1(nm, k))
                if (zz > smax) then
                   smax = zz
                endif
                u1(nm, k) = vvdwk(nm, k)
             enddo
          endif
       enddo
       if(icx == 1) then
         call timer_stop(timer_uzd_solve4u, gdp)
       else
         call timer_stop(timer_uzd_solve6v, gdp)
       end if
       !
       if (itr>0 .and. iter<50) goto 1100
       if (iter >= 50) then
          write (errtxt, '(i0)') nst
          call prterr(lundia    ,'S205'    ,trim(errtxt)    )
       endif
       !
       ! Domain decomposition:
       !        end "solve_system_for_velocity"
       !
       if (icx == 1) then
          !
          ! D3dFlowMap_Check_V: Check for convergence
          !
          nhystp = nxtstp(d3dflow_solve_v, gdp)
          if (nhystp == d3dflow_solve_v) goto 222
       else
          !
          ! D3dFlowMap_Check_U: Check for convergence
          !
          nhystp = nxtstp(d3dflow_solve_u, gdp)
          if (nhystp == d3dflow_solve_u) goto 222
       endif
       !
       ! End Domain decomposition addition
       !
    elseif (momsol == 'mdue  ') then
       call timer_start(timer_uzd_ini, gdp)
       !
       ! Initialise aak, bbk and cck for all (nm, k)
       !
       aak = 0.0
       bbk = 1.0
       cck = 0.0
       !
       call timer_stop(timer_uzd_ini, gdp)
       call timer_start(timer_uzd_rhs, gdp)
       !
       do k = 1, kmax
          do nm = 1, nmmax
             ddk(nm, k) = u1(nm, k)
          enddo
       enddo
       call timer_stop(timer_uzd_rhs, gdp)
       !
       ! end of "build_system_for_velocity",
       ! mapper can build the coupling equations
       !
       call timer_start(timer_uzd_rest, gdp) 
       if (icx==1) then
          !
          ! D3dFlowMap_Build_V: poke the coupling equations into system
          !
          nhystp = nxtstp(d3dflow_build_v, gdp)
       !
       else
          !
          ! D3dFlowMap_Build_U: poke the coupling equations into system
          !
          nhystp = nxtstp(d3dflow_build_u, gdp)
       !
       endif
       call timer_stop(timer_uzd_rest, gdp) 
       !
       ! resume point for next solve
       !
     333 continue
       gdp%dd%uzditer = gdp%dd%uzditer + 1
       !
       if (icx==1) then
          !
          ! D3dFlowMap_Check_V: Check for convergence
          !
          nhystp = nxtstp(d3dflow_solve_v, gdp)
          if (nhystp==d3dflow_solve_v) goto 333
       else
          !
          ! D3dFlowMap_Check_U: Check for convergence
          !
          nhystp = nxtstp(d3dflow_solve_u, gdp)
          !
          if (nhystp==d3dflow_solve_u) goto 333
       endif
    endif
end subroutine z_uzd
