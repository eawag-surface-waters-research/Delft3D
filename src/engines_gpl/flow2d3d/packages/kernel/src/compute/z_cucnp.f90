subroutine z_cucnp(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                 & icy       ,nsrc      ,kcs       ,kcs45     ,kcscut    , &
                 & kfu       ,kfuz1     ,kfumin    ,kfumax    ,kfv       , &
                 & kfvz1     ,kfs       ,kfsz1     ,kfsmin    ,kfsmax    , &
                 & u0        ,v1        ,w1        ,hu        ,dzu1      , &
                 & guu       ,gvv       ,gvu       ,guv       ,gsqs      , &
                 & gud       ,gvd       ,guz       ,gvz       ,gsqiu     , &
                 & disch     ,umdis     ,kspu      ,mnksrc    ,dismmt    , &
                 & aak       ,bbk       ,cck       ,ddk       ,bbka      , &
                 & bbkc      ,vicuv     ,vnu2d     ,vicww     ,tgfsep    , &
                 & drhodx    ,wsu       ,taubpu    ,taubsu    ,rxx       , &
                 & rxy       ,windu     ,patm      ,fcorio    ,p0        , &
                 & ubrlsu    ,pship     ,diapl     ,rnpl      ,cfurou    , &
                 & gdp       )
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
!    Function: The coefficient for the momentum equations are
!              computed and the stored in the arrays AAK, BBK,
!              CCK, and DDK (velocity points). For the depth-
!              averaged equations the coefficients are stored in
!              AA, BB, CC, DD (velocity points) and A, B, C, D
!              (water level points). A double sweep is used
!              to eliminate the coupling in the vertical.
! Method used: Reference : On the approximation of horizontal
!              gradients in sigma co-ordinates for bathymetry
!              with steep bottom slopes (G.S. Stelling and J.
!              van Kester - International Journal for Methods
!              in Fluids, Vol. 18 1994)
!              - Horizontal Advection in U-direction :
!                explicit, central scheme.
!              - Horizontal Advection in V-direction :
!                explicit, central scheme
!              - Horizontal Diffusion : explicit, along
!                Z-planes (3D), implicit (2DH)
!              - Vertical Advection : implicit, central scheme
!              - Vertical Diffusion : implicit
!              - roughness (partial slip) of rigid walls
!              - blockage flow by rigid sheets
!              Special approximation pressure term, based
!              on limiter to avoid artificial flow.
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
    character(8)           , pointer :: dpsopt
    character(6)           , pointer :: momsol
    real(fp)               , pointer :: hdt
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: vicmol
    integer                , pointer :: iro
    integer                , pointer :: irov
!
! Global variables
!
    integer                                                         :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                              !!  then computation proceeds in the X-
                                                                              !!  dir. If icx=1 then computation pro-
                                                                              !!  ceeds in the Y-dir.
    integer                                                         :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                         :: j      !!  Begin pointer for arrays which have
                                                                              !!  been transformed into 1D arrays.
                                                                              !!  Due to the shift in the 2nd (M-)
                                                                              !!  index, J = -2*NMAX + 1
    integer                                                         :: kmax   !  Description and declaration in iidim.f90
    integer                                                         :: nmmax  !  Description and declaration in dimens.igs
    integer                                                         :: nmmaxj !  Description and declaration in dimens.igs
    integer                                           , intent(in)  :: nsrc   !  Description and declaration in iidim.f90
    integer , dimension(7, nsrc)                      , intent(in)  :: mnksrc !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kcs    !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfs    !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kfsmax !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kfsmin !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfu    !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfumax !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfumin !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfv    !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: kspu   !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kcs45
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: kcscut !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kfsz1  !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kfuz1  !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kfvz1  !  Description and declaration in iidim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 3)                   :: cfurou !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: fcorio !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gsqiu  !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gsqs   !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gud    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: guu    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: guv    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: guz    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvd    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvu    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gvv    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvz    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: hu     !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: patm   !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: pship  !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: taubpu !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: taubsu !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: tgfsep !!  Water elev. induced by tide gen.force
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: vnu2d  !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: windu  !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: wsu    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: vicww  !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: w1     !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax + 2)            :: vicuv  !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: aak    !!  Internal work array, coupling of la-
                                                                              !!  yer velocity in (N,M,K) with water
                                                                              !!  level point left (down)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bbk    !!  Internal work array, coefficient la-
                                                                              !!  yer velocity in (N,M,K) implicit part
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bbka   !!  Internal work array
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bbkc   !!  Internal work array
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: cck    !!  Internal work array, coupling layer
                                                                              !!  velocity in (N,M,K) with water level
                                                                              !!  point right (upper)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ddk    !!  Internal work array, diagonal space
                                                                              !!  at (N,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: diapl  !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: dzu1   !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: p0     !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rnpl   !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rxx    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rxy    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: drhodx !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: u0     !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: ubrlsu !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: v1     !  Description and declaration in rjdim.f90
    real(fp), dimension(nsrc)                         , intent(in)  :: disch  !  Description and declaration in rjdim.f90
    real(fp), dimension(nsrc)                         , intent(in)  :: umdis  !  Description and declaration in rjdim.f90
    character(1), dimension(nsrc)                     , intent(in)  :: dismmt !  Description and declaration in ckdim.f90
!
! Local variables
!
    integer            :: ddb
    integer            :: iada
    integer            :: iadc
    integer            :: icxy    ! MAX value of ICX and ICY 
    integer            :: idifc
    integer            :: idifd
    integer            :: idifu
    integer            :: isrc
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
    real(fp)           :: ff
    real(fp)           :: fac
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
    real(fp)           :: timest
    real(fp)           :: uuu
    real(fp)           :: uweir
    real(fp)           :: vih
    real(fp)           :: viznm
    real(fp)           :: viznmd
    real(fp)           :: vvv
    real(fp)           :: www
!
!! executable statements -------------------------------------------------------
!
    dpsopt   => gdp%gdnumeco%dpsopt
    momsol   => gdp%gdnumeco%momsol
    rhow     => gdp%gdphysco%rhow
    ag       => gdp%gdphysco%ag
    vicmol   => gdp%gdphysco%vicmol
    iro      => gdp%gdphysco%iro
    irov     => gdp%gdphysco%irov
    hdt      => gdp%gdnumeco%hdt
    !
    call timer_start(timer_cucnp_ini, gdp)
    !
    ddb    = gdp%d%ddbound
    icxy   = max(icx, icy)
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
    bbk  = 1.0_fp
    cck  = 0.0_fp
    ddk  = 0.0_fp
    bbka = 0.0_fp
    bbkc = 0.0_fp
    !
    call timer_stop(timer_cucnp_ini, gdp)
    !
    call timer_start(timer_cucnp_momsol, gdp)
    if (momsol == 'mdue  ') then
       !
       ! Advection determined explicitly using multi-directional upwind method
       ! Using the whole time step
       !
       timest = 2.0_fp*hdt
       !
    elseif (momsol == 'iupw  ') then
       !
       ! Advection determined implicitly using first order upwind method in z_uzd
       !
       timest = hdt
       !
    else
       !
       ! No correct advection option specified: ToDo generate error message or set default
       !
    endif
    call z_expl_dir_upw(nmmax     ,kmax      ,icx       , &
                      & icy       ,kcs       ,kcs45     ,kcscut    , &
                      & kfu       ,kfuz1     ,kfumin    ,kfumax    , &
                      & kfvz1     ,kfsz1     ,kfsmin    ,kfsmax    , &
                      & u0        ,v1        ,hu        , &
                      & guu       ,gvv       ,gvu       ,guv       ,gsqs      , &
                      & gud       ,gvd       ,guz       ,gvz       ,gsqiu     , &
                      & ddk       ,gdp       )
    call timer_stop(timer_cucnp_momsol, gdp)
    !
    ! Fill the array elements and the right hand side
    !
    call timer_start(timer_cucnp_rhs, gdp)
    do nm = 1, nmmax
       if (kfu(nm)==1) then
          ndm        = nm - icy
          nmu        = nm + icx
          ndmu       = nm + icx - icy
          gksi       = gvu(nm)
          do k = kfumin(nm), kfumax(nm)
             if (kfuz1(nm, k)==1 .and. kcs(nm)*kcs(nmu)>0) then
                !
                ! BAROCLINIC PRESSURE, CORIOLIS, ATMOSPHERIC PRES. and TIDE GEN. FORCE
                ! Surface gradient is fully accounted for here and not in z_uzd:
                ! -> aak = -2.0 * ag/gki
                ! -> cck =  2.0 * ag/gki
                !
                vvv        = 0.25_fp*(v1(ndm, k) + v1(ndmu, k) + v1(nm, k) + v1(nmu, k))
                aak(nm, k) = -ag/gksi
                bbk(nm, k) = 1.0_fp/timest
                cck(nm, k) = ag/gksi
                ddk(nm, k) = ddk(nm, k)                                       & 
                         & + u0(nm, k)/timest                                 &
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
    call timer_stop(timer_cucnp_rhs, gdp)
    !
    ! 2D Weir not included
    ! energy loss for CDW (remainder structure untested yet)
    !
    call timer_start(timer_cucnp_eloss, gdp)
    call usrbrl(icx       ,icy       ,nmmax     ,kmax      ,kfu       , &
              & kspu      ,gvu       ,u0        ,v1        ,bbk       , &
              & ubrlsu    ,diapl     ,rnpl      ,gdp       )
    call timer_stop(timer_cucnp_eloss, gdp)
    !
    call timer_start(timer_cucnp_stress, gdp)
    do nm = 1, nmmax
       nmu  = nm + icx
       kmin = kfumin(nm)
       if (kfu(nm)==1 .and. kcs(nm)*kcs(nmu)>0) then
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
             if (kfu(nm)==1 .and. abs(u0(nm,kmin)) <= 1.0e-15 .and. kspu(nm, 0) /= 4 .and. kspu(nm, 0) /= 10) then
                !
                ! cfurou(nm,1) contains u/u*
                !
                uweir      = sqrt(2.0 / 3.0 * ag * max(hu(nm), 0.01_fp))
                taubpu(nm) = uweir / (cfurou(nm,1)**2)
             endif
          endif
          !
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
    call timer_stop(timer_cucnp_stress, gdp)
    !
    ! DISCHARGE ADDITION OF MOMENTUM
    !
    call timer_start(timer_cucnp_dismmt, gdp)
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
             !
             endif
          endif
       endif
    enddo
    call timer_stop(timer_cucnp_dismmt, gdp)
    !
    ! VERTICAL ADVECTION AND VISCOSITY, IMPLICIT
    !
    call timer_start(timer_cucnp_advdiffv, gdp)
    do nm = 1, nmmax
       if (kfumax(nm) > kfumin(nm)) then
          kmaxtl = 1
          nmu    = nm + icx
          do k = kfumin(nm), kfumax(nm)
             if (kfuz1(nm, k) == 1) then
                kfad = 0
                kdo  = k - 1
                kup  = k + 1
                if (k==kfumin(nm)) then
                   kfad = 1
                   kdo  = k
                endif
                if (k==kfumax(nm)) then
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
                !   ADVECTION IN VERTICAL DIRECTION; W*DU/DZ
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
                   adza = -2.0_fp*www/(dzup + dzdo)*(1 - abs(kfad)) + abs(kfad)*(-1 + kfad)*www/dzdo
                   adzc =  2.0_fp*www/(dzup + dzdo)*(1 - abs(kfad))
                endif
                adza = iada*adza
                adzc = iadc*adzc
                adzb = -adza - adzc
                !
                ! Subtitution in coefficients
                !
                bbka(nm, k) = adza
                bbk (nm, k) = bbk(nm, k) + adzb
                bbkc(nm, k) = adzc
                !
                ! Vertical viscosity
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
                bbka(nm, k) = bbka(nm, k) - ddza
                bbk (nm, k) = bbk (nm, k) - ddzb
                bbkc(nm, k) = bbkc(nm, k) - ddzc
             endif
          enddo
       endif
    enddo
    call timer_stop(timer_cucnp_advdiffv, gdp)
    !
    ! HORIZONTAL VISCOSTY
    !
    call timer_start(timer_cucnp_vih, gdp)
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
    ! for Crank Nicolson method: is computed in routine uzd (implicitly)
    ! for fractional step method: is computed here (explicitly)
    !
    else
       if (momsol == 'mdue  ') then 
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
                      !
                      vih        = 0.5_fp*(vicuv(nm, k) + vicuv(nmu, k) + vnu2d(nm) + vnu2d(ndm))
                      dbk        = -2.0_fp*vih/(gksid*gksiu)*idifc - vih/(getau*geta)         &
                                 & *idifu - vih/(getad*geta)*idifd
                      dux        = vih/(gksiu*gksi)*idifc
                      ddx        = vih/(gksid*gksi)*idifc
                      duy        = vih/(getau*geta)*idifu
                      ddy        = vih/(getad*geta)*idifd
                      ddk(nm, k) = ddk(nm, k) + dbk*u0(nm, k) + ddx*u0(nmd, k)     &
                                 & + dux*u0(nmu, k) + ddy*u0(ndm, k)               &
                                 & + duy*u0(num, k)
                   endif
                endif
             enddo
          enddo
       else
          !
          ! Implicit upwind, so horizontal viscosity is included in z_uzd
          !
       endif
    endif
    call timer_stop(timer_cucnp_vih, gdp)
    !
    ! SOLUTION PROCEDURE SYSTEM OF EQUATIONS
    !
    call timer_start(timer_cucnp_lhs, gdp)
    if (kmaxtl > 0) then
       do nm = 1, nmmax
          if (kfumin(nm) /= 0 .and. kfumax(nm) > 1) then
             maxk           = kfumax(nm)
             bi             = 1.0_fp/bbk(nm, maxk)
             bbk (nm, maxk) = 1.0_fp
             bbka(nm, maxk) = bbka(nm, maxk)*bi
             aak (nm, maxk) = aak(nm, maxk)*bi
             cck (nm, maxk) = cck(nm, maxk)*bi
             ddk (nm, maxk) = ddk(nm, maxk)*bi
          endif
       enddo
       do nm = 1, nmmax
          do k = kfumax(nm) - 1, kfumin(nm), -1
             bi          = 1.0_fp/(bbk(nm, k) - bbkc(nm, k)*bbka(nm, k + 1))
             bbk (nm, k) = 1.0_fp
             bbka(nm, k) = bbka(nm, k)*bi
             aak (nm, k) = (aak(nm, k) - bbkc(nm, k)*aak(nm, k + 1))*bi
             cck (nm, k) = (cck(nm, k) - bbkc(nm, k)*cck(nm, k + 1))*bi
             ddk (nm, k) = (ddk(nm, k) - bbkc(nm, k)*ddk(nm, k + 1))*bi
          enddo
       enddo
       !
       ! back sweep
       !
       do nm = 1, nmmax
          do k = kfumin(nm) + 1, kfumax(nm)
             aak(nm, k) = aak(nm, k) - bbka(nm, k)*aak(nm, k - 1)
             cck(nm, k) = cck(nm, k) - bbka(nm, k)*cck(nm, k - 1)
             ddk(nm, k) = ddk(nm, k) - bbka(nm, k)*ddk(nm, k - 1)
          enddo
       enddo
    endif
    call timer_stop(timer_cucnp_lhs, gdp)
end subroutine z_cucnp
