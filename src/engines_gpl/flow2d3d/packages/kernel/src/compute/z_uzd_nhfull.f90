subroutine z_uzd_nhfull(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                      & icy       ,nsrc      ,kcs       ,kcs45     ,kcscut    , &
                      & kfu       ,kfuz1     ,kfumin    ,kfumax    ,kfv       , &
                      & kfvz1     ,kfs       ,kfsz1     ,kfsmin    ,kfsmax    , &
                      & u0        ,v0        ,w0        ,hu        ,dzu1      , &
                      & u1        ,s0        , &
                      & guu       ,gvv       ,gvu       ,guv       ,gsqs      , &
                      & gud       ,gvd       ,guz       ,gvz       ,gsqiu     , &
                      & disch     ,umdis     ,kspu      ,mnksrc    ,dismmt    , &
                      & aak       ,bbk       ,cck       ,ddk       ,            &
                      &            vicuv     ,vnu2d     ,vicww     ,tgfsep    , &
                      & drhodx    ,wsu       ,taubpu    ,taubsu    ,rxx       , &
                      & rxy       ,windu     ,patm      ,fcorio    ,p0        , &
                      & crbc      ,norow     ,circ2d    ,circ3d    ,irocol    , &
                      & dpu       ,tetau     ,umean     ,thick     ,zk        , &
                      & ubrlsu    ,pship     ,diapl     ,rnpl      ,gdp       )
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
!    Function: The coefficient for the momentum equations are
!              computed and the stored in the arrays AAK, BBK,
!              CCK, and DDK (velocity points).
!              This is identical to the Z_CUCNP. However, now
!              an explicit time integration is applied.
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
! Dummy subroutine for Z-model / domaindecomposition
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
    ! They replace the  include igd / include igp lines
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
    real(fp)               , pointer :: pi
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
    integer                                                             :: kmax   !  Description and declaration in iidim.f90
    integer                                               , intent(in)  :: norow  !  Description and declaration in iidim.f90
    real(fp)    , dimension(12, norow)                                  :: crbc   !  Description and declaration in rjdim.f90
    real(fp)    , dimension(4, norow)                                   :: circ2d !  Description and declaration in rjdim.f90
    real(fp)    , dimension(kmax, 2, norow)                             :: circ3d !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: dpu    !  Description and declaration in rjdim.f90
    integer                                                             :: icx    !!  Increment in the X-dir., if ICX= NMAX
    integer     , dimension(5, norow)                                   :: irocol !  Description and declaration in iidim.f90 then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                             :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                             :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                                             :: nmmax  !  Description and declaration in dimens.igs
    integer                                                             :: nmmaxj !  Description and declaration in dimens.igs
    integer                                               , intent(in)  :: nsrc   !  Description and declaration in iidim.f90
    integer     , dimension(7, nsrc)                      , intent(in)  :: mnksrc !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kcs    !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfs    !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kfsmax !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kfsmin !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfu    !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfumax !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfumin !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfv    !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: kspu   !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kcs45
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: kcscut !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kfsz1  !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kfuz1  !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kfvz1  !  Description and declaration in iidim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: fcorio !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gsqiu  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gsqs   !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gud    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: guu    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: guv    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: guz    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvd    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvu    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gvv    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvz    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: hu     !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: patm   !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: pship  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: s0     !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: taubpu !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: taubsu !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: tetau  !!  Factor for upwind approach S0 can be 0.0, 0.5 or 1.0 depending on value of HU, DCO, KSPU and UMEAN
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: tgfsep !!  Water elev. induced by tide gen.force
    real(fp)    , dimension(kmax)                                       :: thick  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: umean  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: vnu2d  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: windu  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: wsu    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: vicww  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: w0     !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax + 2)            :: vicuv  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: aak    !!  Internal work array, coupling of layer velocity in (N,M,K) with water level point left (down)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bbk    !!  Internal work array, coefficient layer velocity in (N,M,K) implicit part
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: cck    !!  Internal work array, coupling layer velocity in (N,M,K) with water level point right (upper)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ddk    !!  Internal work array, diagonal space at (N,M,K)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: diapl  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: dzu1   !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: p0     !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rnpl   !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rxx    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rxy    !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: drhodx !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: u0     !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: u1     !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: ubrlsu !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: v0     !  Description and declaration in rjdim.f90
    real(fp)    , dimension(nsrc)                         , intent(in)  :: disch  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(nsrc)                         , intent(in)  :: umdis  !  Description and declaration in rjdim.f90
    character(1), dimension(nsrc)                         , intent(in)  :: dismmt !  Description and declaration in ckdim.f90
    real(fp)    , dimension(0:kmax)                       , intent(in)  :: zk     !  Description and declaration in rjdim.f90
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
    integer            :: nhystp
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
    real(fp)           :: vih
    real(fp)           :: viznm
    real(fp)           :: viznmd
    real(fp)           :: vvv
    real(fp)           :: www
!
!! executable statements -------------------------------------------------------
!
    pi         => gdp%gdconst%pi
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
    ddb    = gdp%d%ddbound
    icxy   = max(icx, icy)
    timest = 2 * hdt
    if (icx==1) then
       ff = -1.0_fp
    else
       ff = 1.0_fp
    endif
    kmaxtl = 0
    !
    ! Initialise arrays aak, bbk, cck and ddk and u1 for all (nm, k)
    !
    aak  = 0.0_fp
    bbk  = 1.0_fp
    cck  = 0.0_fp
    ddk  = 0.0_fp
    u1   = 0.0_fp
    !
    do nm = 1, nmmax
       if (kfu(nm) == 1) then
          nmd   = nm - icx
          ndm   = nm - icy
          ndmd  = nm - icx - icy
          nmu   = nm + icx
          num   = nm + icy
          numu  = nm + icx + icy
          ndmu  = nm + icx - icy
          numd  = nm - icx + icy
          gksi  = gvu(nm)
          geta  = guu(nm)
          dgeta = guz(nmu) - guz(nm)
          dgvnm = gvd(nm)  - gvd(ndm)
          gsqi  = gsqiu(nm)
          !
          do k = kfumin(nm), kfumax(nm)
             if (kfuz1(nm, k)==1 .and. kcs(nm)*kcs(nmu)>0) then
                vvv   = 0.25_fp * (v0(ndm,k)+v0(ndmu,k)+v0(nm,k)+v0(nmu,k))
                uuu   = u0(nm,k)
                cvv   = vvv / geta
                cuu   = uuu / gksi
                idifd = kfvz1(ndm,k) * kfvz1(ndmu,k) * kfuz1(ndm,k)
                idifu = kfvz1(nm ,k) * kfvz1(nmu ,k) * kfuz1(num,k)
                !
                ! For 1:n stair case (cut-cell) boundary:
                ! - check dgvnm
                ! - reset geta
                ! - reset vvv
                !
                if (kcscut(nm,k)==1 .or. kcscut(nmu,k)==1) then
                   kenm = max(1 , kfvz1(nm,k)+kfvz1(ndm,k)+kfvz1(ndmu,k)+kfvz1(nmu,k))
                   vvv  =   v0(nm  ,k)*kfvz1(nm  ,k) + v0(ndm,k)*kfvz1(ndm,k) &
                        & + v0(ndmu,k)*kfvz1(ndmu,k) + v0(nmu,k)*kfvz1(nmu,k)
                   vvv  = vvv / kenm
                endif
                !
                ! ADVECTION IN U-DIRECTION; DU/DX AND CENTRIFUGAL ACCELERATION
                !
                ! method is not identical to (Koren & Vreugdenhil, pp. 73).
                ! uuu/vvv are not taken at correct point
                ! dx and dy according to zeta-definition (for energy conservation)
                ! tests for cuu and cvv are not based on local courant-number but
                ! based on an average velocity and gksi/geta
                !
                if (        kcs45(nm ,k)/=3 .and. kcs45(nm ,k)/= 9 &
                    & .and. kcs45(nmu,k)/=6 .and. kcs45(nmu,k)/=12  ) then
                   !
                   ! All cases except 45 degrees staircase
                   !
                   if (uuu>=0.0_fp .and. vvv>=0.0_fp) then
                      if (cuu > cvv) then
                         advecx = kfuz1(nmd, k)                                 &
                                & *(uuu*((u0(nm, k) - u0(nmd, k))/gksi +        &
                                & vvv*gsqi*dgvnm))
                         if (kfuz1(nmd, k)*kfuz1(ndmd, k) == 0) then
                            advecy = idifd*(cvv*(u0(nm, k) - u0(ndm, k))        &
                                   & - 0.5_fp*vvv*vvv*gsqi*dgeta)
                         else
                            advecy = kfvz1(ndm, k)                              &
                                   & *(cvv*(u0(nmd, k) - u0(ndmd, k))           &
                                   & - 0.5_fp*vvv*vvv*gsqi*dgeta)
                         endif
                      else
                         advecy = idifd*(vvv*(u0(nm, k) - u0(ndm, k))           &
                                & /geta - 0.5_fp*vvv*vvv*gsqi*dgeta)
                         if (kfuz1(ndmd, k)*kfuz1(ndm, k)*kfvz1(ndm, k) == 0) then
                            advecx = kfuz1(nmd, k)                              &
                                   & *(uuu*((u0(nm, k) - u0(nmd, k))            &
                                   & /gksi + vvv*gsqi*dgvnm))
                         else
                            advecx = uuu*((u0(ndm, k) - u0(ndmd, k))            &
                                   & /gksi + vvv*gsqi*dgvnm)
                         endif
                      endif
                   elseif (uuu<0.0_fp .and. vvv>=0.0_fp) then
                      if (-cuu > cvv) then
                         advecx = kfuz1(nmu, k)                                 &
                                & *(uuu*((u0(nmu, k) - u0(nm, k))/gksi +        &
                                & vvv*gsqi*dgvnm))
                         if (kfuz1(nmu, k)*kfuz1(ndmu, k) == 0) then
                            advecy = idifd*(cvv*(u0(nm, k) - u0(ndm, k))        &
                                   & - 0.5_fp*vvv*vvv*gsqi*dgeta)
                         else
                            advecy = kfvz1(ndmu, k)                             &
                                   & *(cvv*(u0(nmu, k) - u0(ndmu, k))           &
                                   & - 0.5_fp*vvv*vvv*gsqi*dgeta)
                         endif
                      else
                         advecy = idifd*(vvv*(u0(nm, k) - u0(ndm, k))           &
                                & /geta - 0.5_fp*vvv*vvv*gsqi*dgeta)
                         if (kfuz1(ndmu, k)*kfuz1(ndm, k)*kfvz1(ndmu, k) == 0) then
                            advecx = kfuz1(nmu, k)                              &
                                   & *(uuu*((u0(nmu, k) - u0(nm, k))            &
                                   & /gksi + vvv*gsqi*dgvnm))
                         else
                            advecx = uuu*((u0(ndmu, k) - u0(ndm, k))            &
                                   & /gksi + vvv*gsqi*dgvnm)
                         endif
                      endif
                   elseif (uuu>=0.0_fp .and. vvv<0.0_fp) then
                      if (cuu > -cvv) then
                         advecx = kfuz1(nmd, k)                                 &
                                & *(uuu*((u0(nm, k) - u0(nmd, k))/gksi +        &
                                & vvv*gsqi*dgvnm))
                         if (kfuz1(numd, k)*kfuz1(nmd, k) == 0) then
                            advecy = idifu*(cvv*(u0(num, k) - u0(nm, k))        &
                                   & - 0.5_fp*vvv*vvv*gsqi*dgeta)
                         else
                            advecy = kfvz1(nm, k)                               &
                                   & *(cvv*(u0(numd, k) - u0(nmd, k))           &
                                   & - 0.5_fp*vvv*vvv*gsqi*dgeta)
                         endif
                      else
                         advecy = idifu*(vvv*(u0(num, k) - u0(nm, k))           &
                                & /geta - 0.5_fp*vvv*vvv*gsqi*dgeta)
                         if (kfuz1(num, k)*kfuz1(numd, k)*kfvz1(nm, k) == 0) then
                            advecx = kfuz1(nmd, k)                              &
                                   & *(uuu*((u0(nm, k) - u0(nmd, k))            &
                                   & /gksi + vvv*gsqi*dgvnm))
                         else
                            advecx = uuu*((u0(num, k) - u0(numd, k))            &
                                   & /gksi + vvv*gsqi*dgvnm)
                         endif
                      endif
                   elseif (uuu<0.0_fp .and. vvv<0.0_fp) then
                      if (-cuu> -cvv) then
                         advecx = kfuz1(nmu, k)                                 &
                                & *(uuu*((u0(nmu, k) - u0(nm, k))/gksi +        &
                                & vvv*gsqi*dgvnm))
                         if (kfuz1(numu, k)*kfuz1(nmu, k) == 0) then
                            advecy = idifu*(cvv*(u0(num, k) - u0(nm, k))        &
                                   & - 0.5_fp*vvv*vvv*gsqi*dgeta)
                         else
                            advecy = kfvz1(nmu, k)                              &
                                   & *(cvv*(u0(numu, k) - u0(nmu, k))           &
                                   & - 0.5_fp*vvv*vvv*gsqi*dgeta)
                         endif
                      else
                         advecy = idifu*(vvv*(u0(num, k) - u0(nm, k))           &
                                & /geta - 0.5_fp*vvv*vvv*gsqi*dgeta)
                         if (kfuz1(numu, k)*kfuz1(num, k)*kfvz1(nmu, k) == 0) then
                            advecx = kfuz1(nmu, k)                              &
                                   & *(uuu*((u0(nmu, k) - u0(nm, k))            &
                                   & /gksi + vvv*gsqi*dgvnm))
                         else
                            advecx = uuu*((u0(numu, k) - u0(num, k))            &
                                   & /gksi + vvv*gsqi*dgvnm)
                         endif
                      endif
                   else
                   endif
                else
                   !
                   ! 45 degree staircase
                   !
                   if (uuu > 0.0_fp) then
                      advecx = uuu*((u0(nm ,k) - u0(nmd,k))/gksi + vvv*gsqi*dgvnm)
                   else
                      advecx = uuu*((u0(nmu,k) - u0(nm ,k))/gksi + vvv*gsqi*dgvnm)
                   endif
                   if (vvv > 0.0_fp) then
                      advecy = vvv*(u0(nm ,k)-u0(ndm,k))/geta - 0.5_fp*vvv*vvv*gsqi*dgeta
                   else
                      advecy = vvv*(u0(num,k)-u0(nm ,k))/geta - 0.5_fp*vvv*vvv*gsqi*dgeta
                   endif
                endif
                !
                ! BAROCLINIC PRESSURE, NO CORIOLIS, ATMOSPHERIC PRES. and TIDE GEN. FORCE
                !
                bbk(nm, k) = 1.0_fp / timest
                ddk(nm, k) = u0(nm, k)/timest - advecx - advecy                 &
                           & + ff*fcorio(nm)*vvv                                &
                           & - ag/rhow*drhodx(nm, k)*kfsz1(nm, k)*kfsz1(nmu, k) &
                           & - (patm(nmu) - patm(nm))/(gksi*rhow)               &
                           & - (pship(nmu) - pship(nm))/(gksi*rhow)             &
                           & + ag*(tgfsep(nmu) - tgfsep(nm))/gksi               &
                           & - (p0(nmu, k) - p0(nm, k))/(gksi*rhow)
                ddk(nm, k) = ddk(nm, k) -ag * (s0(nmu) - s0(nm)) / gksi
             endif
          enddo
       endif
    enddo
    !
    ! 2D Weir not included
    ! energy loss for CDW (remainder structure untested yet)
    !
    !call usrbrl(icx       ,icy       ,nmmax     ,kmax      ,kfu       , &
    !           & kspu      ,gvu       ,u0        ,v0       ,bbk       , &
    !           & ubrlsu    ,diapl     ,rnpl      ,gdp       )
    !
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
          cbot           = taubpu(nm)
          dz             = zk(kmin) - zk(kmin-1)
          bdmwrp         = cbot / dz
          bdmwrs         = taubsu(nm) / dz
          bbk(nm, kmin)  = bbk(nm,kmin) + bdmwrp
          ddk(nm, kmin)  = ddk(nm,kmin) + bdmwrs
          dz             = zk(kkmax) - zk(kkmax-1)
          qwind          = windu(nm) / dz
          ddk(nm, kkmax) = ddk(nm,kkmax) - qwind/rhow
          !
          ! WAVE FORCE AT SURFACE
          !
          ddk(nm, kkmax) = ddk(nm,kkmax) + wsu(nm)/(rhow*dz)
       endif
    enddo
    !
    ! DISCHARGE ADDITION OF MOMENTUM
    !
    do isrc = 1, nsrc
       nm = (mnksrc(2,isrc)+ddb) + ((mnksrc(1,isrc)-1)+ddb)*icxy
       nmd = nm - icx
       if (dismmt(isrc)=='Y' .and. disch(isrc)>0.0_fp) then
          if (umdis(isrc) >= 0.0_fp) then
             nmdis  = nm
             hugsqs = hu(nm) * gsqs(nm)
          else
             nmdis  = nmd
             hugsqs = hu(nmd) * gsqs(nm)
          endif
          kk = mnksrc(3,isrc)
          if (kfu(nmdis) == 1) then
             if (kk == 0) then
                do k = 1, kmax
                   bbk(nmdis,k) = bbk(nmdis,k) +             disch(isrc)/hugsqs
                   ddk(nmdis,k) = ddk(nmdis,k) + umdis(isrc)*disch(isrc)/hugsqs
                enddo
             else
                bbk(nmdis, kk) = bbk(nmdis,kk) +             disch(isrc)/(dzu1(nmdis,kk)*gsqs(nm))
                ddk(nmdis, kk) = ddk(nmdis,kk) + umdis(isrc)*disch(isrc)/(dzu1(nmdis,kk)*gsqs(nm))
             endif
          endif
       endif
    enddo
    !
    ! VERTICAL ADVECTION AND VISCOSITY, IMPLICIT
    !
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
                if (kspu(nm,0)==4 .or. kspu(nm,0)==10) then
                   iada = max( 1-(kspu(nm,kdo)+kspu(nm,k  )) , 0)
                   iadc = max( 1-(kspu(nm,k  )+kspu(nm,kup)) , 0)
                endif
                dzup = zk(k) - zk(k-1) + zk(kup) - zk(kup-1)
                dzdo = zk(k) - zk(k-1) + zk(kdo) - zk(kdo-1)
                !
                !   ADVECTION IN VERTICAL DIRECTION; W*DU/DZ
                !
                ! Special attention is needed at the surface when there are no neighbours
                !
                maskval = min(kcs(nm),2) * min(kcs(nmu),2)
                www     = 0.25_fp * maskval *(w0(nm,k-1)+w0(nm,k)+w0(nmu,k-1)+w0(nmu,k))
                if (www < 0.0_fp) then
                   adza = -2.0_fp*www*dzup/(dzdo*(dzup + dzdo))*(1 - abs(kfad))
                   adzc =  2.0_fp*www*dzdo/(dzup*(dzup + dzdo))*(1 - abs(kfad)) &
                         & + kfad*(1 + kfad)*www/dzup
                else
                   adzc =  2.0_fp*www*dzdo/(dzup*(dzup + dzdo))*(1 - abs(kfad))
                   adza = -2.0_fp*www*dzup/(dzdo*(dzup + dzdo))*(1 - abs(kfad)) &
                         & + abs(kfad)*( - 1 + kfad)*www/dzdo
                endif
                adza = iada * adza
                adzc = iadc * adzc
                adzb = -adza - adzc
                !
                ! Subtitution in coefficients
                !
                aak(nm, k) = adza
                bbk(nm, k) = bbk(nm, k) + adzb
                cck(nm, k) = adzc
                !
                ! Vertical viscosity
                !
                !
                ! viznmd calculation
                ! restriction is moved from Z_TURCLO to here
                !
                viznmd = 0.25_fp * (2 - kfad*(1 + kfad))                  &
                       & * (2.0_fp*vicmol + redvic(vicww(nm , kdo), gdp)  &
                       &                  + redvic(vicww(nmu, kdo), gdp)) &
                       & * maskval
                !
                ! viznm calculation
                ! restriction is moved from Z_TURCLO to here
                !
                viznm  = 0.25_fp * (2 + kfad*(1 - kfad))                &
                       & * (2.0_fp*vicmol + redvic(vicww(nm , k), gdp)  &
                       &                  + redvic(vicww(nmu, k), gdp)) &
                       & * maskval
                dz    = zk(k) - zk(k-1)
                ddza = iada * 2.0_fp * viznmd / (dzdo*dz)
                ddzc = iadc * 2.0_fp * viznm  / (dzup*dz)
                ddzb = -ddza - ddzc
                !
                ! subtitution in coefficients
                !
                aak(nm,k) = aak(nm,k) - ddza
                bbk(nm,k) = bbk(nm,k) - ddzb
                cck(nm,k) = cck(nm,k) - ddzc
             endif
          enddo
       else
          !
          ! no implicit part
          !
          do k = 1, kmax
             aak(nm, k) = 0.0_fp
             cck(nm, k) = 0.0_fp
          enddo
       endif
    enddo
    !
    ! HORIZONTAL VISCOSTY
    !
    if (irov > 0) then
       !
       ! Stresses due to rigid walls
       !     implemented fully explicit
       !
       call z_vihrov(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                   & icy       ,kcs45     ,kcs       ,kfu       ,kfv       , &
                   & kfs       ,u0        ,v0        ,vicuv     ,vnu2d     , &
                   & gud       ,guu       ,gvd       ,gvu       ,gvz       , &
                   & ddk       ,rxx       ,rxy       ,kfuz1     ,kfvz1     , &
                   & kfsz1     ,kfumin    ,kfumax    ,gdp       )
    !
    ! for Crank Nicolson method: is computed in routine uzd (implicitly)
    ! for fractional step method: is computed here (explicitly)
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
             if (kfu(nm)==1 .and. kcs(nm)*kcs(nmu)>0) then
                if (kfuz1(nm, k)==1) then
                   gksid = gvz(nm)
                   gksiu = gvz(nmu)
                   gksi  = gvu(nm)
                   getad = gud(ndm)
                   getau = gud(nm)
                   geta  = guu(nm)
                   idifd = kfvz1(ndm,k) * kfvz1(ndmu,k) * kfuz1(ndm,k)
                   idifu = kfvz1(nm ,k) * kfvz1(nmu ,k) * kfuz1(num,k)
                   idifc = abs(2-kcs(nm)) * abs(2-kcs(nmu))
                   !
                   ! EDDY VISCOSITY FOR KMAX = 1, USING LAPLACE OPERATOR
                   ! (2*VIH*(D2U/DX2 + D2U/DY2)
                   !
                   vih        = 0.5_fp * (vicuv(nm,k)+vicuv(nmu,k)+vnu2d(nm)+vnu2d(ndm))
                   dbk        = -2.0_fp * vih/(gksid*gksiu)*idifc - vih/(getau*geta)*idifu         &
                              &  - vih/(getad*geta)*idifd
                   dux        = vih / (gksiu*gksi) * idifc
                   ddx        = vih / (gksid*gksi) * idifc
                   duy        = vih / (getau*geta) * idifu
                   ddy        = vih / (getad*geta) * idifd
                   ddk(nm, k) = ddk(nm,k) + dbk*u0(nm,k) + ddx*u0(nmd,k)      &
                              & + dux*u0(nmu,k) + ddy*u0(ndm,k)               &
                              & + duy*u0(num,k)
                endif
             endif
          enddo
       enddo
    endif
    !
    ! DETERMINE TETAU (for boundary conditions)
    !
    nmu = +icx
    do nm = 1, nmmax
       nmu       = nmu + 1
       tetau(nm) = 0.5_fp
       if (kfu(nm) == 1) then
          if (umean(nm) >= 0.001_fp) then
             tetau(nm) = 1.0_fp
          elseif (umean(nm) <= -0.001_fp) then
             tetau(nm) = 0.0_fp
          else
             tetau(nm) = 1.0_fp
             if (s0(nmu) > s0(nm)) then
                tetau(nm) = 0.0_fp
             endif
          endif
       endif
    enddo
    !
    ! BOUNDARY CONDITIONS
    !
    call z_cucbp_nhfull(kmax      ,norow     ,icx       , &
                      & icy       ,irocol    ,kcs       ,kfu       , &
                      & kfumin    ,kfumax    ,s0        ,u0        ,dpu       , &
                      & hu        ,umean     ,tetau     ,guu       ,gvu       , &
                      & dzu1      ,thick     ,circ2d    ,circ3d    ,            &
                      & aak       ,bbk       ,cck       ,ddk       ,            &
                      & crbc      ,gdp       )
    !
    ! end of "build_system_for_velocity",
    ! mapper can build the coupling equations
    !
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
    !
    ! resume point for next solve
    !
  222 continue
    gdp%dd%uzditer = gdp%dd%uzditer + 1
    !
    ! SOLUTION PROCEDURE SYSTEM OF EQUATIONS
    !
    if (kmaxtl > 0) then
       do nm = 1, nmmax
          if (kfumin(nm)/=0 .and. kfumax(nm)>1) then
             maxk          = kfumax(nm)
             bi            = 1.0_fp / bbk(nm, maxk)
             bbk (nm,maxk) = 1.0_fp
             aak (nm,maxk) = aak(nm,maxk) * bi
             ddk (nm,maxk) = ddk(nm,maxk) * bi
          endif
       enddo
       do nm = 1, nmmax
          do k = kfumax(nm)-1, max(kfumin(nm),1), -1
             bi          = 1.0_fp / (bbk(nm,k)-cck(nm,k)*aak(nm,k+1))
             bbk (nm, k) = 1.0_fp
             aak (nm, k) = aak(nm,k) * bi
             ddk (nm, k) = (ddk(nm,k)-cck(nm,k)*ddk(nm,k+1)) * bi
          enddo
       enddo
       !
       ! back sweep
       !
       do nm = 1, nmmax
          do k = kfumin(nm)+1, kfumax(nm)
             ddk(nm,k) = ddk(nm,k) - aak(nm,k)*ddk(nm,k-1)
          enddo
       enddo
       do nm = 1, nmmax
          do k = max(kfumin(nm),1), kmax
             if (k <= kfumax(nm)) then         
                u1(nm,k) = ddk(nm,k)
             else
                u1(nm,k) = 0.0_fp
             endif   
          enddo
       enddo
    endif      
    if (icx == 1) then
       !
       ! D3dFlowMap_Check_V: Check for convergence
       !
       nhystp = nxtstp(d3dflow_solve_v, gdp)
       if (nhystp==d3dflow_solve_v) goto 222
    else
       !
       ! D3dFlowMap_Check_U: Check for convergence
       !
       nhystp = nxtstp(d3dflow_solve_u, gdp)
       if (nhystp==d3dflow_solve_u) goto 222
    endif
end subroutine z_uzd_nhfull
