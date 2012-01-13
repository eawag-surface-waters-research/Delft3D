subroutine z_difu(lundia    ,nst       ,icx       ,icy       ,j         , &
                & nmmaxj    ,nmmax     ,kmax      ,lstsci    ,norow     , &
                & irocol    ,kcs       ,kcu       ,kfs       , &
                & kfsmin    ,kfsmax    ,kfsmx0    ,kfumin    ,kfumx0    , &
                & kfvmin    ,kfvmx0    ,kfsz1     ,kfuz1     ,kfvz1     , &
                & qxk       ,qyk       ,qzk       ,u         ,v         , &
                & guv       ,gvu       ,gsqs      ,rbnd      ,sigdif    , &
                & sigmol    ,dicuv     ,vicww     ,r0        ,r1        , &
                & sour      ,sink      ,aak       ,bbk       ,cck       , &
                & bdx       ,bux       ,bdy       ,buy       ,uvdwk     , &
                & vvdwk     ,rscale    , &
                & aakl      ,bbkl      ,cckl      ,ddkl      ,dzs0      ,dzs1      , &
                & dzu0      ,dzv0      ,areau     ,areav     ,volum0    , &
                & volum1    ,guu       ,gvv       ,bruvai    ,rho       , &
                & s1        ,dps       ,ltem      ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
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
! Computes transport in the u, v and w-direction.
! Implicit w-direction, explicit in u- and v-direction.
! Sinks are treated implicitly and sources explicitly
! A special approach is used for the horizontal diffusion to avoid artificial
! creeping.
! - Special approach for top layer.
! - Horizontal Advection in U-direction :
!      Van Leer 2, non linear approach (explicit)
! - Horizontal Advection in V-direction :
!      Van Leer 2, non linear approach (explicit)
! - Vertical Advection, first order upwind (impl)
! - Horizontal Diffusion :
!     2D and 3D: explicit, along Z-planes
! - Option: horizontal diffusion strictly horizontal using special filter
! - Vertical Diffusion : implicitly
! - Sources are integrated explicitly.
! - Sinks are integrated implicitly.
!
! COMMENT:
! For the Thatcher Harlemann boundaries the boundary points for outflow
! are reflected from the inner points; for inflow the boundary conditions are
! used (see also thahbc.for).
!
! NOTE:
! AREAU/V were set using DZU0/V0 in Z_SUD as required by Z_DIFHOR

!
!!--pseudo code and references--------------------------------------------------
!
! "A comparison of two 3D shallow water models using sigma coordinates and
!  z-coordinates in the vertical direction."
! Bijvelds, Van Kester and Stelling.
!
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
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
    real(fp)               , pointer :: hdt
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: vicmol
    real(fp)               , pointer :: dicoww
    integer                , pointer :: iro
    character(13)          , pointer :: trasol
    real(fp)               , pointer :: xlo
    real(fp)               , pointer :: ck
    logical                , pointer :: nonhyd
    integer                , pointer :: nh_level
!
! Global variables
!
    integer                                                               :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                                    !!  then computation proceeds in the X-
                                                                                    !!  dir. If icx=1 then computation pro-
                                                                                    !!  ceeds in the Y-dir.
    integer                                                               :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                               :: j      !!  Begin pointer for arrays which have
                                                                                    !!  been transformed into 1D arrays.
                                                                                    !!  Due to the shift in the 2nd (M-)
                                                                                    !!  index, J = -2*NMAX + 1
    integer                                                               :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                               :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                               :: lundia !  Description and declaration in inout.igs
    integer                                                               :: nmmax  !  Description and declaration in dimens.igs
    integer                                                               :: nmmaxj !  Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: norow  !  Description and declaration in esm_alloc_int.f90
    integer                                                               :: nst
    integer , dimension(5, norow)                           , intent(in)  :: irocol !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfumx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfvmin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfvmx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: kfsz1  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: kfuz1  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: kfvz1  !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: ltem   !  Description and declaration in dimens.igs
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: bruvai !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: vicww  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: qzk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: aak    !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: bbk    !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: cck    !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: bdx    !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: bux    !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: bdy    !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: buy    !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: uvdwk  !!  Internal work array for Jac.iteration
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: vvdwk  !!  Internal work array for Jac.iteration
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: rscale !!  Internal work array for Jac.iteration
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax+2)                    :: dicuv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: areau  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: areav  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzs0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzs1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: rho    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: volum0 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: volum1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: dzu0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: dzv0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: qyk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: u
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: v
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: aakl   !!  Internal work array, lower diagonal
                                                                                    !!  tridiagonal matrix, implicit coupling
                                                                                    !!  of concentration in (N,M,K) with con-
                                                                                    !!  centration in (N,M,K-1)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: bbkl   !!  Internal work array, main diagonal
                                                                                    !!  tridiagonal matrix, implicit coupling
                                                                                    !!  of concentration in (N,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: cckl   !!  Internal work array, upper diagonal
                                                                                    !!  tridiagonal matrix, implicit coupling
                                                                                    !!  of concentration in (N,M,K) with con-
                                                                                    !!  centration in (N,M,K+1)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: ddkl   !!  Internal work array, diagonal space
                                                                                    !!  at (N,M,K,L)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: r0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(in)  :: sink   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(in)  :: sour   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax, max(lstsci, 1), 2, norow)     , intent(in)  :: rbnd   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(lstsci)                                           :: sigdif !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(lstsci)                             , intent(in)  :: sigmol !  Description and declaration in esm_alloc_real.f90

!
! Local variables
!
    integer            :: ddb
    integer            :: ic
    integer            :: iter
    integer            :: itr
    integer            :: icxy
    integer            :: k
    integer            :: kfw
    integer            :: kfsum
    integer            :: kmin
    integer            :: ksm
    integer            :: l
    integer            :: m
    integer            :: mf
    integer            :: mink
    integer            :: mink2
    integer            :: ml
    integer            :: n
    integer            :: ndm
    integer            :: nhystp
    integer            :: nm
    integer            :: nmd
    integer            :: nmf
    integer            :: nml
    integer            :: nmlu
    integer            :: nmu
    integer            :: nmuu
    integer            :: num
    integer            :: nuum
    integer            :: ku
    integer            :: kr
    integer            :: kd
    real(fp)           :: adza
    real(fp)           :: adzc
    real(fp)           :: bi
    real(fp)           :: cfl
    real(fp)           :: ddzc
    real(fp)           :: delz
    real(fp)           :: difiwe
    real(fp)           :: diz1
    real(fp)           :: epsitr
    real(fp)           :: timest
    real(fp)           :: flux
    real(fp)           :: qzw
    real(fp)           :: r00
    real(fp), external :: reddic
    real(fp)           :: rscal
    real(fp)           :: sqrtbv
    real(fp)           :: volu
    real(fp)           :: z1
    character(256)     :: errtxt
!
!! executable statements -------------------------------------------------------
!
    hdt         => gdp%gdnumeco%hdt
    rhow        => gdp%gdphysco%rhow
    ag          => gdp%gdphysco%ag
    vicmol      => gdp%gdphysco%vicmol
    dicoww      => gdp%gdphysco%dicoww
    iro         => gdp%gdphysco%iro
    trasol      => gdp%gdtricom%trasol
    xlo         => gdp%gdturcoe%xlo
    ck          => gdp%gdturcoe%ck
    nonhyd      => gdp%gdprocs%nonhyd
    nh_level    => gdp%gdnonhyd%nh_level
    !
    if (lstsci==0) goto 9999
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    if (nonhyd .and. nh_level==nh_full) then
       timest = 2.0_fp * hdt
    else
       timest = hdt
    endif
    !
    call timer_start(timer_difu_ini, gdp)
    !
    ! Initialise work arrays
    !
    aakl   = 0.0_fp
    bbkl   = 1.0_fp
    cckl   = 0.0_fp
    ddkl   = 0.0_fp
    bdx    = 0.0_fp
    bux    = 0.0_fp
    bdy    = 0.0_fp
    buy    = 0.0_fp
    !
    do l = 1, lstsci
       !
       ! system of difference equations
       !
       do nm = 1, nmmax
          if (kfs(nm) == 1) then
             kmin = min(kfsmax(nm), kfsmx0(nm))
             if (kfsmax(nm) == kfsmx0(nm) .or. kmax == 1) then
                do k = kfsmin(nm), kfsmax(nm)
                   bbkl(nm, k, l) = volum1(nm, k) / timest
                   aakl(nm, k, l) = 0.0
                   cckl(nm, k, l) = 0.0
                   ddkl(nm, k, l) = volum0(nm, k) * r0(nm, k, l) / timest
                enddo
             elseif (kfsmax(nm) > kfsmx0(nm)) then
                do k = kfsmin(nm), kmin
                   bbkl(nm, k, l) = volum1(nm, k) / timest
                   aakl(nm, k, l) = 0.0
                   cckl(nm, k, l) = 0.0
                   ddkl(nm, k, l) = volum0(nm, k) * r0(nm, k, l) / timest
                enddo
                do k = kmin + 1, kfsmax(nm)
                   bbkl(nm, kmin, l) = bbkl(nm, kmin, l) + volum1(nm, k)/timest
                   !
                   ! also volume at old time level should be taken into account 
                   ! Noted that for the concentrations the value at KMIN
                   ! is used, because at the other layers the concentration
                   ! is zero (see routine Z_CHKDRY)
                   !
                   ddkl(nm, kmin, l) = ddkl(nm, kmin, l) &
                                   & + volum0(nm, k)*r0(nm, kmin, l)/timest
                   aakl(nm, k, l) = 0.0
                   bbkl(nm, k, l) = 1.0
                   cckl(nm, k, l) = 0.0
                   ddkl(nm, k, l) = 0.0
                enddo
             else
                do k = kfsmin(nm), kmin
                   bbkl(nm, k, l) = volum1(nm, k) / timest
                   aakl(nm, k, l) = 0.0
                   cckl(nm, k, l) = 0.0
                   ddkl(nm, k, l) = volum0(nm, k) * r0(nm, k, l) / timest
                enddo
                do k = kmin + 1, kfsmx0(nm)
                   bbkl(nm, k, l) = 1.0
                   aakl(nm, k, l) = 0.0
                   cckl(nm, k, l) = 0.0
                   ddkl(nm, k, l) = 0.0
                   ddkl(nm, kmin, l) = ddkl(nm, kmin, l) + volum0(nm, k)*r0(nm, k, l)/timest
                enddo
             endif
          endif
       enddo
    enddo
    call timer_stop(timer_difu_ini, gdp) 
    !
    ! Horizontal advection in U and V direction
    !
    call timer_start(timer_difu_horadv, gdp)
    if     (trasol == 'van leer-2   ') then
       !
       ! Explicit Van Leer-2 method
       !
       call z_difu_horadv_expl()
       !
    elseif (trasol == 'iupw         ') then
       !
       ! Implicit upwind method
       !
       call z_difu_horadv_impl()
       !
    endif
    call timer_stop(timer_difu_horadv, gdp)
    !
    call timer_start(timer_difu_rest, gdp)
    !
    ! Summation of fluxes for cells which do not have a horizontal neighbour cell
    !
    do l = 1, lstsci
       do nm = 1, nmmax
          if (kfs(nm) /= 0) then
             kmin = min(kfsmax(nm), kfsmx0(nm))
             do k = kmin + 1, kmax
                ddkl(nm, kmin, l) = ddkl(nm, kmin, l) + ddkl(nm, k, l)
                ddkl(nm, k   , l) = 0.0_fp
             enddo
          endif
       enddo
    enddo
    call timer_stop(timer_difu_rest, gdp)
    !
    ! Diffusion in horizontal direction
    !
    call timer_start(timer_difu_hordiff, gdp)
    if     (trasol == 'van leer-2   ') then
       !
       ! Explicit method
       !
       call z_difu_difhor_expl()
       !
    elseif (trasol == 'iupw         ') then
       !
       ! Implicit method
       !
       call z_difu_difhor_impl()
       !
    endif
    call timer_stop(timer_difu_hordiff, gdp)    
    !
    do l = 1, lstsci
       !
       uvdwk  = 0.0_fp
       vvdwk  = 0.0_fp
       rscale = 0.0_fp
       !
       ! SOURCES AND SINK TERMS
       !
       call timer_start(timer_difu_sourcesink, gdp) 
       !
       ! SINKS ARE TREATED IMPLICITLY
       !
       do nm = 1, nmmax
          if (kfs(nm)*kcs(nm) == 1) then
             if (kfsmax(nm) == kfsmx0(nm) .or. kmax == 1) then
                do k = kfsmin(nm), kfsmax(nm)
                   bbkl(nm, k, l) = bbkl(nm, k, l) + sink(nm, k, l)
                   ddkl(nm, k, l) = ddkl(nm, k, l) + sour(nm, k, l)
                enddo
             else
                kmin = min(kfsmax(nm), kfsmx0(nm))
                do k = kfsmin(nm), kmin
                   bbkl(nm, k, l) = bbkl(nm, k, l) + sink(nm, k, l)
                   ddkl(nm, k, l) = ddkl(nm, k, l) + sour(nm, k, l)
                enddo
                if (kfsmax(nm) > kfsmx0(nm)) then
                   do k = kmin + 1, kfsmax(nm)
                      bbkl(nm, kmin, l) = bbkl(nm, kmin, l) + sink(nm, k, l)
                      ddkl(nm, kmin, l) = ddkl(nm, kmin, l) + sour(nm, k, l)
                   enddo
                endif
             endif
          endif
       enddo
       call timer_stop(timer_difu_sourcesink, gdp) 
       call timer_start(timer_difu_vertadv, gdp)  
       !
       do nm = 1, nmmax
          ksm = min(kfsmx0(nm), kfsmax(nm))
          if (kfs(nm) == 1) then
             do k = kfsmin(nm), ksm - 1
                if (k == kfsmin(nm) .or. k == ksm - 1) then
                   kfw = 1
                else
                   kfw = 0
                endif
                !
                ! ADVECTION IN VERTICAL DIRECTION; W*DC/DZ
                !
                ! second order central
                !
                if (kfs(nm) == 1) then
                   qzw = qzk(nm, k)
                   if (qzw < 0.0) then
                      adza = 0.5 *qzw * (1 - kfw)
                      adzc = 0.5 *qzw * (1 + kfw)
                   else
                      adza = 0.5 *qzw * (1 + kfw)
                      adzc = 0.5 *qzw * (1 - kfw)
                   endif
                   aakl(nm, k + 1, l) = aakl(nm, k + 1, l) - adza
                   bbkl(nm, k + 1, l) = bbkl(nm, k + 1, l) - adzc
                   bbkl(nm, k    , l) = bbkl(nm, k    , l) + adza
                   cckl(nm, k    , l) = cckl(nm, k    , l) + adzc
                endif
             enddo
          endif
       enddo
       call timer_stop(timer_difu_vertadv, gdp)
       !
       ! DIFFUSION IN VERTICAL DIRECTION
       !
       call timer_start(timer_difu_vertdiff, gdp)
       do nm = 1, nmmax
          ksm = min(kfsmx0(nm), kfsmax(nm))
          if (kfs(nm) == 1 ) then
             do k = kfsmin(nm), ksm - 1
                delz = max(0.1_fp, 0.5*(dzs1(nm, k) + dzs1(nm, k + 1)))
                !
                ! Internal wave contribution
                !
                sqrtbv = max(0.0_fp, bruvai(nm, k))
                sqrtbv = sqrt(sqrtbv)
                difiwe = 0.2 * sqrtbv * xlo**2
                !
                ! dicoww-restriction is moved from TURCLO to here (in reddic)
                ! vicww is used instead of dicww
                !
                diz1 = vicmol/sigmol(l) + reddic(difiwe + vicww(nm,k)/sigdif(l), gdp)
                ddzc = gsqs(nm) * diz1 / delz
                aakl(nm, k+1, l) = aakl(nm, k+1, l) - ddzc
                bbkl(nm, k+1, l) = bbkl(nm, k+1, l) + ddzc
                bbkl(nm, k  , l) = bbkl(nm, k  , l) + ddzc
                cckl(nm, k  , l) = cckl(nm, k  , l) - ddzc
             enddo
          endif
       enddo
       call timer_stop(timer_difu_vertdiff, gdp)  
       !
       ! set values in open boundary points (in part. for y-direction)
       !
       call timer_start(timer_difu_bounopen, gdp)
       do nm = 1, nmmax
          if (kcs(nm) == 2) then
             do k = kfsmin(nm), kfsmax(nm)
                ddkl(nm, k, l) = r0(nm, k, l)
                aakl(nm, k, l) = 0.0
                bbkl(nm, k, l) = 1.0
                cckl(nm, k, l) = 0.0
             enddo
          endif
       enddo
       !
       ! IMPLEMENTATION OF BOUNDARY CONDITIONS IN X-DIRECTION
       !
       do ic = 1, norow
          n = irocol(1, ic)
          mf = irocol(2, ic) - 1
          ml = irocol(3, ic)
          nmf = (n + ddb)*icy + (mf + ddb)*icx - icxy
          nml = (n + ddb)*icy + (ml + ddb)*icx - icxy
          nmlu = nml + icx
          if (kcu(nmf) == 1) then
             do k = kfsmin(nmf), kfsmax(nmf)
                aakl(nmf, k, l) = 0.0
                bbkl(nmf, k, l) = 1.0
                cckl(nmf, k, l) = 0.0
                ddkl(nmf, k, l) = rbnd(k, l, 1, ic)
             enddo
          endif
          if (kcu(nml) == 1) then
             do k = kfsmin(nmlu), kfsmax(nmlu)
                aakl(nmlu, k, l) = 0.0
                bbkl(nmlu, k, l) = 1.0
                cckl(nmlu, k, l) = 0.0
                ddkl(nmlu, k, l) = rbnd(k, l, 2, ic)
             enddo
          endif
       enddo
       call timer_stop(timer_difu_bounopen, gdp) 
       !
       !   set concentrations in temporary dry points and in open boundary points
       !
       call timer_start(timer_difu_rest, gdp)
       do nm = 1, nmmax
          if (kfs(nm) == 0 .and. kcs(nm) == 1) then
             do k = kfsmin(nm), kmax
                r1(nm, k, l) = r0(nm, k, l)
             enddo
          endif
          if (kcs(nm) == 2) then
             do k = kfsmin(nm), kmax
                r1(nm, k, l) = ddkl(nm, k, l)
             enddo
          endif
       enddo
       call timer_stop(timer_difu_rest, gdp)
       call timer_start(timer_difu_lhs, gdp)
       do nm = 1, nmmax
          if (kcs(nm) == 3 ) then
             !
             ! left hand-side is now set by Delft3D-FLOW instead of the mapper
             !
             do k = 1, kmax
                aakl(nm, k, l) = 0.0_fp
                bbkl(nm, k, l) = 1.0_fp
                cckl(nm, k, l) = 0.0_fp
                ddkl(nm, k, l) = r0(nm, k, l)
             enddo
          else 
             if (kfs(nm) /= 0) then
                !
                !***SCALE ROWS OF MATRIX/RIGHT HAND SIDE VECTOR
                !
                do k = kfsmin(nm), kfsmx0(nm)
                   rscale(nm, k)  = 1.0_fp / bbkl(nm, k, l)
                   aakl(nm, k, l) = aakl(nm, k, l) * rscale(nm, k)
                   bbkl(nm, k, l) = 1.0_fp
                   cckl(nm, k, l) = cckl(nm, k, l) * rscale(nm, k)
                   ddkl(nm, k, l) = ddkl(nm, k, l) * rscale(nm, k)
                enddo
             endif
          endif
       enddo
       call timer_stop(timer_difu_lhs, gdp)
       !
       ! D3dFlow_Build_ADI_Conc: poke the coupling equations into system
       !
       nhystp = nxtstp(d3dflow_build_adi_conc, gdp)
       !
       ! SOLUTION PROCEDURE SYSTEM OF EQUATIONS
       !
       if     (trasol == 'van leer-2   ') then
          !
          call z_difu_solv_expl()
          !
       elseif (trasol == 'iupw         ') then
          !
          call z_difu_solv_impl()
          !
       endif
    enddo
 9999 continue    
!
!
!
contains
!
!
!===============================================================================
!
! 
!
subroutine z_difu_horadv_expl()
    !
    ! horizontal advection using Van Leer-2 method (explicit)
    !
    ! local variables
    !
    real(fp)    :: rr1 ! Numerator of the Van Leer limiter
    real(fp)    :: rr2 ! Denominator of the Van Leer limiter
    !
    do nm = 1, nmmax
       !
       ! HORIZONTAL ADVECTION IN X-DIRECTION
       !
       nmd  = nm  - icx
       nmu  = nm  + icx
       nmuu = nmu + icx
       if (kfs(nm)*kfs(nmu) /= 0) then
          kmin = max(kfumin(nm), 1)
          do k = kmin, kmax
             cfl = u(nm, k) * timest / gvu(nm)
             if (qxk(nm, k) > 0.0) then
                do l = 1, lstsci
                   rr1 = abs(r0(nmd, k, l) - 2.0_fp*r0(nm, k, l) + r0(nmu, k, l))
                   rr2 = abs(r0(nmd, k, l) - r0(nmu, k, l))
                   if (kfsz1(nmd, k)*kfsz1(nmu, k) == 0 .or. rr1 >= rr2 .or. rr2 < eps_fp) then 
                      if (kfsz1(nm,k) == 1) then
                         r00 = r0(nm, k, l)
                      else
                         mink2 = min(kfsmx0(nm),kfsmax(nm))
                         r00 = r0(nm, mink2, l)
                      endif
                   else
                      r00 = r0(nm , k, l)                                     &
                          & + (1.0_fp - cfl)*(r0(nm , k, l) - r0(nmd, k, l))  &
                          &                 *(r0(nmu, k, l) - r0(nm , k, l))  &
                          &                 /(r0(nmu, k, l) - r0(nmd, k, l))
                   endif
                   flux = qxk(nm, k) * r00
                   if (kcs(nm) == 1) then
                      ddkl(nm, k, l) = ddkl(nm, k, l) - flux
                   endif
                   if (kcs(nm + icx) == 1) then
                      ddkl(nm + icx, k, l) = ddkl(nm + icx, k , l) + flux
                   endif
                enddo
             else
                do l = 1, lstsci
                   rr1 = abs(r0(nmuu, k, l) - 2.0_fp*r0(nmu, k, l) + r0(nm, k, l))
                   rr2 = abs(r0(nmuu, k, l) - r0(nm, k, l))
                   if (kfsz1(nm, k)*kfsz1(nmuu, k) == 0 .or. rr1 >= rr2 .or. rr2 < eps_fp) then
                      if (kfsz1(nmu,k) == 1) then
                         r00 = r0(nmu, k, l)
                      else
                         mink2 = min(kfsmx0(nmu),kfsmax(nmu))
                         r00 = r0(nmu, mink2, l)
                      endif
                   else
                      r00 = r0(nmu, k, l)                                      &
                          & + (1.0_fp + cfl)*(r0(nm , k, l) - r0(nmu , k, l))  &
                          &                 *(r0(nmu, k, l) - r0(nmuu, k, l))  &
                          &                 /(r0(nm , k, l) - r0(nmuu, k, l))
                   endif
                   flux = qxk(nm, k) * r00
                   if (kcs(nm) == 1) then
                      ddkl(nm, k, l) = ddkl(nm, k, l) - flux
                   endif
                   if (kcs(nm + icx) == 1) then
                      ddkl(nm + icx, k, l) = ddkl(nm + icx, k , l) + flux
                   endif
                enddo
             endif
          enddo
       endif
       !
       ! HORIZONTAL ADVECTION IN Y-DIRECTION
       !
       ndm  = nm  - icy
       num  = nm  + icy
       nuum = num + icy
       if (kfs(nm)*kfs(num) /= 0) then
          kmin = max(kfvmin(nm), 1)
          do k = kmin, kmax
             cfl = v(nm, k) * timest / guv(nm)
             if (qyk(nm, k) > 0.0) then
                do l = 1, lstsci
                   rr1 = abs(r0(ndm, k, l) - 2.0_fp*r0(nm, k, l) + r0(num, k, l))
                   rr2 = abs(r0(ndm, k, l) - r0(num, k, l))
                   if (kfsz1(ndm, k)*kfsz1(num, k) == 0 .or. rr1 >= rr2 .or. rr2 < eps_fp) then
                      if (kfsz1(nm,k) == 1) then
                         r00 = r0(nm, k, l)
                      else
                         mink2 = min(kfsmx0(nm),kfsmax(nm))
                         r00 = r0(nm, mink2, l)
                      endif
                   else
                      r00 = r0(nm , k, l)                                     &
                          & + (1.0_fp - cfl)*(r0(nm , k, l) - r0(ndm, k, l))  &
                          &                 *(r0(num, k, l) - r0(nm , k, l))  &
                          &                 /(r0(num, k, l) - r0(ndm, k, l))
                   endif
                   flux = qyk(nm, k) * r00
                   if (kcs(nm) == 1) then 
                      ddkl(nm, k, l) = ddkl(nm, k, l) - flux
                   endif
                   if (kcs(nm + icy) == 1 ) then
                      ddkl(nm + icy, k, l) = ddkl(nm + icy, k , l) + flux
                   endif
                enddo
             else
                do l = 1, lstsci
                   rr1 = abs(r0(nuum, k, l) - 2.0_fp*r0(num, k, l) + r0(nm, k, l))
                   rr2 = abs(r0(nuum, k, l) - r0(nm, k, l))
                   if (kfsz1(nm, k)*kfsz1(nuum, k) == 0 .or. rr1 >= rr2 .or. rr2 < eps_fp) then
                      if (kfsz1(num,k) == 1) then
                         r00 = r0(num, k, l)
                      else
                         mink2 = min(kfsmx0(num),kfsmax(num))
                         r00 = r0(num, mink2, l)
                      endif
                   else
                      r00 = r0(num, k, l)                                      &
                          & + (1.0_fp + cfl)*(r0(nm , k, l) - r0(num , k, l))  &
                          &                 *(r0(num, k, l) - r0(nuum, k, l))  &
                          &                 /(r0(nm , k, l) - r0(nuum, k, l))
                   endif
                   flux = qyk(nm, k) * r00
                   if (kcs(nm) == 1) then
                      ddkl(nm, k, l) = ddkl(nm, k, l) - flux
                   endif
                   if (kcs(nm + icy) == 1) then
                      ddkl(nm + icy, k, l) = ddkl(nm + icy, k , l) + flux
                   endif
                enddo
             endif
          enddo
       endif
    enddo
    !
end subroutine z_difu_horadv_expl
!
!
!===============================================================================
subroutine z_difu_horadv_impl()
    !
    ! Horizontal advection using first order upwind method (implicit)
    !
    do nm = 1, nmmax
       !
       ! HORIZONTAL ADVECTION IN X-DIRECTION
       !
       nmd  = nm - icx
       nmu  = nm + icx
       if (kfs(nm)*kfs(nmu) /= 0) then
          kmin = max(kfumin(nm), 1)
          mink = min(kfsmax(nm), kfsmx0(nm))
          do k = kmin, kmax
             if (qxk(nm, k) > 0.0_fp) then
                if (kfsz1(nm, k) * kfsz1(nmu, k) == 1) then
                   if (kcs(nmu) == 1) then
                      bdx(nmu, k) = - qxk(nm, k)*kfuz1(nm, k)
                   endif
                   do l = 1, lstsci
                      bbkl(nm, k, l) = bbkl(nm, k, l) + qxk(nm, k)*kfuz1(nm, k)
                   enddo
                else
                   do l = 1, lstsci
                      if (kfsz1(nm, k) == 1) then
                         flux = qxk(nm, k) * r0(nm, k, l)
                      else
                         mink2 = min(kfsmx0(nm),kfsmax(nm))
                         flux = qxk(nm, k) * r0(nm, mink2, l)
                      endif
                      if (kcs(nm) == 1) then 
                         ddkl(nm, k, l) = ddkl(nm, k, l) - flux
                      endif
                      if (kcs(nmu) == 1) then
                         ddkl(nmu, k, l) = ddkl(nmu, k , l) + flux
                      endif
                   enddo
                endif
             else
                if (kfsz1(nm, k) * kfsz1(nmu, k) == 1) then
                   if (kcs(nm) == 1) then
                      bux(nm, k) = qxk(nm, k)*kfuz1(nm, k)
                   endif
                   do l = 1, lstsci
                      bbkl(nmu, k, l) = bbkl(nmu, k, l) - qxk(nm, k)*kfuz1(nm, k)
                   enddo
                else
                   do l = 1, lstsci
                      if (kfsz1(nmu, k) == 1) then
                         flux = qxk(nm, k) * r0(nmu, k, l)
                      else
                         mink2 = min(kfsmx0(nmu),kfsmax(nmu))
                         flux = qxk(nm, k) * r0(nmu, mink2, l)
                      endif
                      if (kcs(nm)== 1) then 
                         ddkl(nm, k, l) = ddkl(nm, k, l) - flux
                      endif
                      if (kcs(nmu) == 1) then
                         ddkl(nmu, k, l) = ddkl(nmu, k , l) + flux
                      endif
                   enddo
                endif
             endif
          enddo
       endif
       !
       ! HORIZONTAL ADVECTION IN Y-DIRECTION
       !
       ndm  = nm - icy
       num  = nm + icy
       if (kfs(nm)*kfs(num)/=0) then
          kmin = max(kfvmin(nm), 1)
          do k = kmin, kmax
             if (qyk(nm, k) > 0.0_fp) then
                if (kfsz1(nm, k) * kfsz1(num, k) == 1) then
                   if (kcs(num) == 1) then
                      bdy(num, k) = - qyk(nm, k)*kfvz1(nm, k)
                   endif
                   do l = 1, lstsci
                      bbkl(nm, k, l) = bbkl(nm, k, l) + qyk(nm, k)*kfvz1(nm, k)
                   enddo
                else
                   do l = 1, lstsci
                      if (kfsz1(nm, k) == 1) then
                         flux = qyk(nm, k) * r0(nm, k, l)
                      else
                         mink2 = min(kfsmx0(nm),kfsmax(nm))
                         flux = qyk(nm, k) * r0(nm, mink2, l)
                      endif
                      if (kcs(nm) == 1) then 
                         ddkl(nm, k, l) = ddkl(nm, k, l) - flux
                      endif
                      if (kcs(num) == 1) then
                         ddkl(num, k, l) = ddkl(num, k , l) + flux
                      endif
                   enddo
                endif
             else
                if (kfsz1(nm, k) * kfsz1(num, k) == 1) then
                   if (kcs(nm) == 1) then
                      buy(nm, k) = qyk(nm, k)*kfvz1(nm, k)
                   endif
                   do l = 1, lstsci
                      bbkl(num, k, l) = bbkl(num, k, l) - qyk(nm, k)*kfvz1(nm, k)
                   enddo
                else
                   do l = 1, lstsci
                      if (kfsz1(num, k) == 1) then
                         flux = qyk(nm, k) * r0(num, k, l)
                      else
                         mink2 = min(kfsmx0(num),kfsmax(num))
                         flux = qyk(nm, k) * r0(num, mink2, l)
                      endif
                      if (kcs(nm) == 1) then 
                         ddkl(nm, k, l) = ddkl(nm, k, l) - flux
                      endif
                      if (kcs(num) == 1) then
                         ddkl(num, k, l) = ddkl(num, k , l) + flux
                      endif
                   enddo
                endif
             endif
          enddo
       endif
    enddo
    !
end subroutine z_difu_horadv_impl
!
!
!===============================================================================
subroutine z_difu_difhor_expl( )
    !
    ! EXPLICIT TIME INTEGRATION FOR HORIZONTAL DIFFUSION
    !
    ! Noted that the value of SIGDIF(L) = 0.7 (see TKECOF) for all LSTSCI
    !
    ! Local variables
    !
    integer :: maskval
    real(fp):: cl
    real(fp):: cr
    real(fp):: difl
    real(fp):: difr
    !
    ! IN X-DIRECTION
    !
    do l = 1, lstsci
       do nm = 1, nmmax
          nmu = nm + icx
          mink = min( min(kfsmx0(nm), kfsmx0(nmu)), kfumx0(nm) )
          do k = kfumin(nm), mink
             if (kfuz1(nm, k)==1 .and. kfsz1(nm, k)*kfsz1(nmu, k)/=0) then
                cl = r0(nm, k, l)
                difl = dicuv(nm, k)
                cr = r0(nmu, k, l)
                difr = dicuv(nmu, k)
                flux = 0.5*(cr - cl)*(difl + difr)/(0.7_fp*gvu(nm))
                maskval = abs(2 - kcs(nmu))
                ddkl(nm, k, l) = ddkl(nm, k, l) + areau(nm, k)*flux*maskval
                maskval = abs(2 - kcs(nm))
                ddkl(nmu, k, l) = ddkl(nmu, k, l) - areau(nm, k)*flux*maskval
             endif
          enddo
       enddo
    enddo
    !
    ! IN Y-DIRECTION
    !
    do l = 1, lstsci
       do nm = 1, nmmax
          num = nm + icy
          mink = min( min(kfsmx0(nm), kfsmx0(num)), kfvmx0(nm) )
          do k = kfvmin(nm), mink
             if (kfvz1(nm, k)==1 .and. kfsz1(nm, k)*kfsz1(num, k)/=0) then
                cl = r0(nm, k, l)
                difl = dicuv(nm, k)
                cr = r0(num, k, l)
                difr = dicuv(num, k)
                flux = 0.5*(cr - cl)*(difl + difr)/(0.7_fp*guv(nm))
                maskval = abs(2 - kcs(num))
                ddkl(nm, k, l) = ddkl(nm, k, l) + areav(nm, k)*flux*maskval
                maskval = abs(2 - kcs(nm))
                ddkl(num, k, l) = ddkl(num, k, l) - areav(nm, k)*flux*maskval
             endif
          enddo
       enddo
    enddo
    !
end subroutine z_difu_difhor_expl
!
!
!===============================================================================
subroutine z_difu_difhor_impl( )
    !
    ! IMPLICIT TIME INTEGRATION FOR HORIZONTAL DIFFUSION 
    !
    ! Noted that the value of SIGDIF(L) = 0.7 (see TKECOF) for all LSTSCI
    !
    ! Local variables
    !
    integer :: maskval1
    integer :: maskval2
    real(fp):: difl
    real(fp):: difr
    !
    ! IN X-DIRECTION
    !
    do nm = 1, nmmax
       nmu = nm + icx
       mink = min( min(kfsmx0(nm), kfsmx0(nmu)), kfumx0(nm) )
       do k = kfumin(nm), mink
          if (kfuz1(nm, k) == 1 .and. kfsz1(nm, k)*kfsz1(nmu, k) /= 0) then
             difl = dicuv(nm, k)
             difr = dicuv(nmu, k)
             flux = 0.5_fp * (difl + difr) / (0.7_fp*gvu(nm))
             maskval1 = abs(2 - kcs(nmu))
             maskval2 = abs(2 - kcs(nm ))
             bux(nm , k) = bux(nm , k) - areau(nm, k)*flux*maskval1
             bdx(nmu, k) = bdx(nmu, k) - areau(nm, k)*flux*maskval2
             do l = 1, lstsci
                bbkl(nm , k, l) = bbkl(nm , k, l) + areau(nm, k)*flux*maskval1
                bbkl(nmu, k, l) = bbkl(nmu, k, l) + areau(nm, k)*flux*maskval2
             enddo
          endif
       enddo
    enddo
    !
    ! IN Y-DIRECTION
    !
    do nm = 1, nmmax
       num = nm + icy
       mink = min( min(kfsmx0(nm), kfsmx0(num)), kfvmx0(nm) )
       do k = kfvmin(nm), mink
          if (kfvz1(nm, k) == 1 .and. kfsz1(nm, k)*kfsz1(num, k) /= 0) then
             difl = dicuv(nm, k)
             difr = dicuv(num, k)
             flux = 0.5_fp * (difl + difr) / (0.7_fp*guv(nm))
             maskval1 = abs(2 - kcs(num))
             maskval2 = abs(2 - kcs(nm ))
             buy(nm , k) = buy(nm , k) - areav(nm, k)*flux*maskval1
             bdy(num, k) = bdy(num, k) - areav(nm, k)*flux*maskval2
             do l = 1, lstsci
                bbkl(nm , k, l) = bbkl(nm , k, l) + areav(nm, k)*flux*maskval1
                bbkl(num, k, l) = bbkl(num, k, l) + areav(nm, k)*flux*maskval2
             enddo
          endif
       enddo
    enddo
    !
end subroutine z_difu_difhor_impl
!
!
!===============================================================================
subroutine z_difu_solv_expl( )
    !
    ! Solution procedure for the transport equation when advection was included explicitly
    !
    call timer_start(timer_difu_solve1, gdp) 
    do nm = 1, nmmax
       if (kfs(nm)/=0) then
          mink = kfsmin(nm)
          kmin = min(kfsmx0(nm), kfsmax(nm))
          bi = 1./bbkl(nm, mink, l)
          cckl(nm, mink, l) = cckl(nm, mink, l)*bi
          ddkl(nm, mink, l) = ddkl(nm, mink, l)*bi
          do k = mink + 1, kmin
             bi = 1./(bbkl(nm, k, l) - aakl(nm, k, l)*cckl(nm, k - 1, l))
             cckl(nm, k, l) = cckl(nm, k, l) * bi
             ddkl(nm, k, l) = (ddkl(nm, k, l) - aakl(nm, k, l)*ddkl(nm, k - 1, l)) * bi
          enddo
       endif
    enddo
    call timer_stop(timer_difu_solve1, gdp)
    !
    ! DD code added:
    !
    ! (re)solve system of equations
    !
  111  continue
    gdp%dd%difuiter = gdp%dd%difuiter + 1
    !
    ! DD code added end
    !
    call timer_start(timer_difu_solve2, gdp)  
    !
    ! back sweep
    !
    do nm = 1, nmmax
       nmd = nm - icx
       ndm = nm - 1
       if (kfs(nm)/=0) then
          mink = kfsmin(nm)
          kmin = min(kfsmx0(nm), kfsmax(nm))
          r1(nm, kmin, l) = ddkl(nm, kmin, l)
          do k = kmin - 1, mink, -1
             r1(nm, k, l) = ddkl(nm, k, l) - cckl(nm, k, l)*r1(nm, k + 1, l)
          enddo
          if (kfsmx0(nm)>kfsmax(nm)) then
             do k = kmin + 1, kmax
                kfsum = kfuz1(nm,k) + kfuz1(nmd,k) + &
                      & kfvz1(nm,k) + kfvz1(ndm,k)
                if (kfsum == 0) then
                   r1(nm, k, l) = 0.
                endif
             enddo
          elseif (kfsmx0(nm)<kfsmax(nm)) then
             do k = kmin + 1, kmax
                r1(nm, k, l) = r1(nm, kmin, l)
                kfsum = kfuz1(nm,k) + kfuz1(nmd,k) + &
                      & kfvz1(nm,k) + kfvz1(ndm,k)
                if (k > kfsmax(nm) .and. kfsum == 0) then
                   r1(nm, k, l) = 0.
                endif
             enddo
          else
          endif
       endif
    enddo
    call timer_stop(timer_difu_solve2, gdp)  
    !
    ! D3dFlow_Solve_ADI_Conc: Check for convergence
    !
    nhystp = nxtstp(d3dflow_solve_adi_conc, gdp)
    if (nhystp==d3dflow_solve_adi_conc) goto 111
    !
end subroutine z_difu_solv_expl
!
!
!===============================================================================
subroutine z_difu_solv_impl( )
    !
    ! Solution procedure for the transport equation when advection was included implicitly
    !
    call timer_start(timer_difu_solve1, gdp) 
    !
    ! Division by the pivot for k=1 is not needed anymore
    ! because of row scaling
    !
    do nm = 1, nmmax
       if (kfs(nm) /= 0) then
          mink = kfsmin(nm)
          kmin = min(kfsmx0(nm), kfsmax(nm))
          do k = mink + 1, kmin
             bi = 1.0_fp/(bbkl(nm, k, l) - aakl(nm, k, l)*cckl(nm, k - 1, l))
             bbkl(nm, k, l) = bi
             cckl(nm, k, l) = cckl(nm, k, l) * bi
          enddo
       endif
    enddo
    call timer_stop(timer_difu_solve1, gdp)
    !
    ! ITERATION LOOP
    !
    call timer_start(timer_difu_solve2, gdp)
    iter = 0
    do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1) then
          mink = kfsmin(nm)
          kmin = min(kfsmx0(nm), kfsmax(nm))
          do k = mink, kmin
             r1(nm, k, l) = r0(nm, k, l)
             uvdwk(nm, k) = r0(nm, k, l)
          enddo
       endif
    enddo
    call timer_stop(timer_difu_solve2, gdp)
    !
    ! DD code added:
    !
    ! (re)solve system of equations
    !
  222  continue
    gdp%dd%difuiter = gdp%dd%difuiter + 1
    !
    ! DD code added end
    !
  333  continue
    iter = iter + 1
    !
    ! ITERATIVE SOLUTION METHOD (JACOBI ITERATION)
    ! IN HORIZONTAL DIRECTION
    !
    itr = 0
    !
    !   set concentrations in coupling points
    !
    call timer_start(timer_difu_solve2, gdp)
    do nm = 1, nmmax
       if (kcs(nm) == 3) then
          mink = kfsmin(nm)
          kmin = min(kfsmx0(nm), kfsmax(nm))
          do k = mink, kmin
             r1(nm, k, l) = ddkl(nm, k, l)
          enddo
       endif
    enddo
    !
    do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1) then
          mink = kfsmin(nm)
          kmin = min(kfsmx0(nm), kfsmax(nm))
          do k = mink, kmin
             !
             ! COMPUTE RIGHT HAND SIDE
             !
             uvdwk(nm, k) = bdx (nm,k) * r1(nm-icx  ,k,l) &
                        & + bdy (nm,k) * r1(nm-icy  ,k,l) &
                        & + buy (nm,k) * r1(nm+icy  ,k,l) &
                        & + bux (nm,k) * r1(nm+icx  ,k,l)
             uvdwk(nm, k) = ddkl(nm, k, l) - rscale(nm, k)*uvdwk(nm,k)
          enddo
       endif
    enddo
    !
    ! forward sweep
    !
    do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1) then
          mink = kfsmin(nm)
          kmin = min(kfsmx0(nm), kfsmax(nm))
          vvdwk(nm, mink) = uvdwk(nm, mink)*bbkl(nm, mink, l)
          do k = mink + 1, kmin
             vvdwk(nm, k) = (uvdwk(nm, k) - aakl(nm, k, l)*vvdwk(nm, k - 1)) * bbkl(nm, k, l)
          enddo
       endif
    enddo
    !
    ! back sweep
    !
    do nm = 1, nmmax
       nmd = nm - icx
       ndm = nm - 1
       if (kcs(nm)*kfs(nm) == 1) then
          mink = kfsmin(nm)
          kmin = min(kfsmx0(nm), kfsmax(nm))
          do k = kmin - 1, mink, -1
             vvdwk(nm, k) = vvdwk(nm, k) - cckl(nm, k, l)*vvdwk(nm, k + 1)
          enddo
          !
          ! Check other layers
          !
          if (kfsmx0(nm) > kfsmax(nm)) then
             do k = kmin + 1, kmax
                kfsum = kfuz1(nm,k) + kfuz1(nmd,k) + &
                      & kfvz1(nm,k) + kfvz1(ndm,k)
                if (kfsum == 0) then
                   r1(nm, k, l) = 0.0_fp
                endif
                if (kfsum == 0) then 
                   vvdwk(nm, k) = 0.0_fp
                endif
             enddo
          elseif (kfsmx0(nm) < kfsmax(nm)) then
             do k = kmin + 1, kmax
                r1(nm, k, l) = r1(nm, kmin, l)
                kfsum = kfuz1(nm,k) + kfuz1(nmd,k) + &
                      & kfvz1(nm,k) + kfvz1(ndm,k)
                if (k > kfsmax(nm) .and. kfsum == 0) then
                   r1(nm, k, l) = 0.0_fp
                endif
                if (k > kfsmax(nm) .and. kfsum == 0) then
                   vvdwk(nm, k) = 0.0_fp
                endif
             enddo
          else
          endif
       endif
    enddo
    !
    ! CHECK FOR CONVERGENCE
    !
    do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1) then
          mink = kfsmin(nm)
          kmin = min(kfsmx0(nm), kfsmax(nm))
          do k = mink,kmin
             epsitr = max(1.0e-8_fp, 0.5e-3_fp*abs(r1(nm, k, l)))
             if (abs(vvdwk(nm, k) - r1(nm, k, l)) > epsitr) then
                itr = 1
             endif
             r1(nm, k, l) = vvdwk(nm, k)
          enddo
       endif
    enddo
    call timer_stop(timer_difu_solve2, gdp)
    !
    if (itr>0 .and. iter<50) goto 333
    !
    if (iter >= 50) then
       write (errtxt, '(i0,a,i0)') l, ' ', nst
       call prterr(lundia    ,'S206'    ,trim(errtxt)    )
    endif 
    !
    ! DD code added:
    !
    ! D3dFlow_Solve_ADI_Conc: Check for convergence
    !
    nhystp = nxtstp(d3dflow_solve_adi_conc, gdp)
    if (nhystp==d3dflow_solve_adi_conc) goto 222
    !
    ! DD code added end          
    !
end subroutine z_difu_solv_impl
!
!
!===============================================================================
end subroutine z_difu
