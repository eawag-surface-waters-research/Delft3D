subroutine erosilt(nmmax   ,icx     ,icy     ,kcs     ,kfs     ,kfu     , &
                 & kfv     ,kfsed   ,kmxsed  ,lsedtot ,lsed    ,thick   , &
                 & kmax    ,dps     ,s0      ,s1      ,taubmx  ,u0eul   , &
                 & v0eul   ,hrms    ,uorb    ,tp      ,teta    ,ws      , &
                 & wstau   ,entr    ,dicww   ,seddif  ,lundia  ,rhosol  , &
                 & rhowat  ,rlabda  ,z0urou  ,z0vrou  ,r0      ,lsal    , &
                 & ltem    ,saleqs  ,temeqs  ,vicmol  ,gdp     )
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
!    Function: Computes sediment fluxes at the bed using
!              the Partheniades-Krone formulations.
!              Arrays SOURSE and SINKSE are filled
!              Array seddif id filled with dicww for mud
! Method used:
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    use bedcomposition_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                         , pointer :: ag
    real(fp)                         , pointer :: vonkar
    real(fp)                         , pointer :: gammax
    real(fp), dimension(:)           , pointer :: dm
    real(fp), dimension(:,:)         , pointer :: fixfac
    real(fp), dimension(:,:)         , pointer :: frac
    real(fp), dimension(:)           , pointer :: mudfrac
    real(fp), dimension(:,:)         , pointer :: sinkse
    real(fp), dimension(:,:)         , pointer :: sourse
    real(fp)                         , pointer :: morfac
    real(fp)                         , pointer :: bed
    real(fp)                         , pointer :: timsec
    logical                          , pointer :: oldmudfrac
    logical                          , pointer :: wave
    logical                          , pointer :: sedim
    logical                          , pointer :: flmd2l
    real(fp)                         , pointer :: kssilt
    real(fp)                         , pointer :: kssand
    real(fp)      , dimension(:)     , pointer :: cdryb
    real(fp)      , dimension(:,:)   , pointer :: tcrdep
    real(fp)      , dimension(:,:)   , pointer :: tcrero
    real(fp)      , dimension(:)     , pointer :: thcmud
    real(fp)      , dimension(:,:)   , pointer :: eropar
    character(4)  , dimension(:)     , pointer :: sedtyp
    logical                          , pointer :: bsskin
    integer       , dimension(:)     , pointer :: iform
    !
    integer                                                   , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                                      !!  then computation proceeds in the X-
                                                                                      !!  dir. If icx=1 then computation pro-
                                                                                      !!  ceeds in the Y-dir.
    integer                                                   , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                   , intent(in)  :: kmax
    integer                                                   , intent(in)  :: lsedtot
    integer                                                   , intent(in)  :: lsed
    integer                                                   , intent(in)  :: lsal
    integer                                                   , intent(in)  :: ltem
    integer                                                                 :: lundia   !  Description and declaration in inout.igs
    integer                                                   , intent(in)  :: nmmax
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfu
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfv
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfsed
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, lsed)                      :: kmxsed
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps
    real(fp)                                                  , intent(in)  :: saleqs
    real(fp)                                                  , intent(in)  :: temeqs
    real(fp)                                                  , intent(in)  :: vicmol
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: entr
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: hrms
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: rlabda
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, *)     , intent(in)  :: r0
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s0
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s1
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: taubmx
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: teta
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: tp
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: uorb
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, *)   , intent(in)  :: ws
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: wstau
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: dicww
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: rhowat !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, lsed)              :: seddif
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: u0eul
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: v0eul
    real(fp)  , dimension(lsed)                               , intent(in)  :: rhosol
    real(fp)  , dimension(kmax)                               , intent(in)  :: thick
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0urou !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0vrou
!
! Local variables
!
    integer  :: k
    integer  :: ku
    integer  :: kv
    integer  :: kn
    integer  :: l
    integer  :: n
    integer  :: m
    integer  :: nm
    integer  :: ndm
    integer  :: nmd
    real(fp) :: h1
    real(fp) :: sour
    real(fp) :: sink
    real(fp) :: taub
    real(fp) :: taum
    real(fp) :: um
    real(fp) :: uuu
    real(fp) :: vm
    real(fp) :: vvv
    real(fp) :: thick0
    real(fp) :: thick1

    ! Interface to dll is in High precision!
    !
    real(fp)          :: z0rou
    real(fp)          :: chezy
    real(fp)          :: sag
    real(fp)          :: ee
    real(hp)          :: sink_dll
    real(hp)          :: sour_dll
    integer           :: error
    integer, external :: perf_function_erosilt
    character(256)    :: errmsg
    character(256)    :: message     ! Contains message from
    !
    character(256), dimension(:)         , pointer :: dll_function
    integer,        dimension(:)         , pointer :: dll_handle
    character(256), dimension(:)         , pointer :: dll_usrfil
    !
    integer                              , pointer :: max_integers
    integer                              , pointer :: max_reals
    integer                              , pointer :: max_strings
    integer       , dimension(:)         , pointer :: dll_integers
    real(hp)      , dimension(:)         , pointer :: dll_reals
    character(256), dimension(:)         , pointer :: dll_strings
!
!! executable statements ------------------
!
    ag                  => gdp%gdphysco%ag
    vonkar              => gdp%gdphysco%vonkar
    gammax              => gdp%gdnumeco%gammax
    dm                  => gdp%gderosed%dm
    fixfac              => gdp%gderosed%fixfac
    frac                => gdp%gderosed%frac
    mudfrac             => gdp%gderosed%mudfrac
    sinkse              => gdp%gderosed%sinkse
    sourse              => gdp%gderosed%sourse
    morfac              => gdp%gdmorpar%morfac
    bed                 => gdp%gdmorpar%bed
    oldmudfrac          => gdp%gdmorpar%oldmudfrac
    wave                => gdp%gdprocs%wave
    sedim               => gdp%gdprocs%sedim
    flmd2l              => gdp%gdprocs%flmd2l
    kssilt              => gdp%gdsedpar%kssilt
    kssand              => gdp%gdsedpar%kssand
    cdryb               => gdp%gdsedpar%cdryb
    tcrdep              => gdp%gdsedpar%tcrdep
    tcrero              => gdp%gdsedpar%tcrero
    thcmud              => gdp%gdsedpar%thcmud
    eropar              => gdp%gdsedpar%eropar
    sedtyp              => gdp%gdsedpar%sedtyp
    bsskin              => gdp%gdsedpar%bsskin
    !
    timsec              => gdp%gdinttim%timsec
    !
    iform               => gdp%gdeqtran%iform
    dll_function        => gdp%gdeqtran%dll_function
    dll_handle          => gdp%gdeqtran%dll_handle
    dll_usrfil          => gdp%gdeqtran%dll_usrfil
    !
    max_integers        => gdp%gdeqtran%max_integers
    max_reals           => gdp%gdeqtran%max_reals
    max_strings         => gdp%gdeqtran%max_strings
    dll_integers        => gdp%gdeqtran%dll_integers
    dll_reals           => gdp%gdeqtran%dll_reals
    dll_strings         => gdp%gdeqtran%dll_strings
    !
    ee     = exp(1.0_fp)
    sag    = sqrt(ag)
    !
    ! Determine total thickness of the mud layers
    ! to be used in computation of skin friction (Soulsby 2004)
    !
    if (bsskin) then
       call detthcmud(gdp%gdmorlyr  ,cdryb     ,sedtyp    ,thcmud    )
    endif
    !
    ! For 3D model set sediment diffusion coefficient
    ! NOTE THAT IF ALGEBRAIC OR K-L TURBULENCE MODEL IS USED THEN WAVES
    ! ONLY AFFECT THE VERTICAL TURBULENT MIXING VIA THE ENHANCED BED
    ! ROUGHNESS
    !
    if (kmax > 1) then
       do l = 1, lsedtot
          if (sedtyp(l)=='sand' .or. sedtyp(l)=='bedl') cycle
          !
          ! calculation both for mud and floc
          !
          do k = 1, kmax
             do nm = 1, nmmax
                if (kfs(nm)==1 .and. kcs(nm)<=2) then
                   seddif(nm, k, l) = dicww(nm, k)
                endif
             enddo
          enddo
       enddo
    endif
    !
    ! Main computational loop over all nm points
    !
    do nm = 1, nmmax
       !
       ! Don't compute anything if water depth is small or if point is DD-point
       !
       if (kfsed(nm)==0 .or. kcs(nm)>2) cycle
       !
       ! kfsed(nm) == 1
       !
       nmd  = nm - icx
       ndm  = nm - icy
       !
       ! Compute depth at cell-centre
       !
       h1   = max(0.01_fp, s1(nm) + real(dps(nm),fp))
       !
       ! Compute depth-averaged velocity components at cell centre
       !
       ku = max(1,kfu(nmd) + kfu(nm))
       kv = max(1,kfv(ndm) + kfv(nm))
       um = 0.0
       vm = 0.0
       do k = 1, kmax
          um = um + thick(k)*(u0eul(nm,k) + u0eul(nmd,k))/ku
          vm = vm + thick(k)*(v0eul(nm,k) + v0eul(ndm,k))/kv
       enddo
       uuu = (u0eul(nm,kmax) + u0eul(nmd,kmax))/ku
       vvv = (v0eul(nm,kmax) + v0eul(ndm,kmax))/kv
       !
       ! Calculate total (possibly wave enhanced) roughness
       !
       kn    = max(1, kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm))
       z0rou = (  kfu(nmd)*z0urou(nmd) + kfu(nm)*z0urou(nm) &
             &  + kfv(ndm)*z0vrou(ndm) + kfv(nm)*z0vrou(nm)  )/kn
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
          call compbsskin   (um      , vm        , h1      , wave    , &
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
       thick0 = thick(kmax) * max(0.01_fp , s0(nm)+real(dps(nm),fp))
       thick1 = thick(kmax) * h1
       !
       ! Bed transport following Partheniades and Krone
       ! but in case of fluid mud, source term is determined by
       ! fluid mud part (sourmu). Information is passed via entr()
       ! maximum erosion is sediment available at bed (ignores sediment
       ! settling during the current morphological timestep)
       ! In case of fluid mud the maximum erosion is determined in sourmu
       ! of the fluid mud module. So ignore this check when fluid mud.
       ! Also, taum is not required in the formulation since whether or not
       ! and how much entrainment occurs is entirely handled by the sourmu
       ! routine.
       !
       ! Main computational loop over all sediments
       !
       do l = 1, lsedtot
          if (sedtyp(l)=='sand' .or. sedtyp(l)=='bedl') cycle
          !
          ! calculation both for mud and floc
          !
          if (flmd2l) then
             !
             ! maximum erosion is sediment available at bed
             ! (ignores sediment settling during the current morphological timestep)
             !
             sour = entr(nm)
             if (tcrdep(nm, l) > 0.0) then
                sink = max(0.0_fp , 1.0-taub/tcrdep(nm, l))
             else
                sink = 0.0
             endif
          else
             if (iform(l) == -1) then
                !
                ! Default Partheniades-Krone formula
                !
                taum = max(0.0_fp, taub/tcrero(nm, l) - 1.0)
                sour = eropar(nm, l) * taum
                if (tcrdep(nm, l) > 0.0) then
                   sink = max(0.0_fp , 1.0-taub/tcrdep(nm, l))
                else
                   sink = 0.0
                endif
             elseif (iform(l) == 15) then
                !
                ! User defined formula in DLL
                ! Input parameters are passed via dll_reals/integers/strings-arrays
                !
                if (max_reals < 30) then
                   write(errmsg,'(a,a,a)') 'Insufficient space to pass real values to transport routine.'
                   call prterr (lundia,'U021', trim(errmsg))
                   call d3stop(1, gdp)
                endif
                dll_reals( 1) = real(timsec ,hp)
                dll_reals( 2) = real(um     ,hp)
                dll_reals( 3) = real(vm     ,hp)
                dll_reals( 4) = real(sqrt(um*um + vm*vm),hp)
                dll_reals( 5) = real(uuu    ,hp)
                dll_reals( 6) = real(vvv    ,hp)
                dll_reals( 7) = real(sqrt(uuu*uuu + vvv*vvv),hp)
                if (kmax>1) then
                   dll_reals( 8) = real(h1*thick(kmax)/2.0_fp,hp)
                else
                   dll_reals( 8) = real(h1/ee,hp)
                endif
                dll_reals( 9) = real(h1     ,hp)
                dll_reals(10) = real(chezy  ,hp)
                if (wave) then
                   dll_reals(11) = real(min(gammax*h1, hrms(nm)),hp)
                   dll_reals(12) = real(tp(nm)  ,hp)
                   dll_reals(13) = real(teta(nm),hp)
                   dll_reals(14) = real(rlabda(nm),hp)
                   dll_reals(15) = real(uorb(nm),hp)
                else
                   dll_reals(11) = 0.0_hp
                   dll_reals(12) = 0.0_hp
                   dll_reals(13) = 0.0_hp
                   dll_reals(14) = 0.0_hp
                   dll_reals(15) = 0.0_hp
                endif
                dll_reals(16) = 0.0_hp !real(di50   ,hp)
                dll_reals(17) = 0.0_hp !real(dss    ,hp)
                dll_reals(18) = 0.0_hp !real(dstar  ,hp)
                dll_reals(19) = 0.0_hp !real(d10    ,hp)
                dll_reals(20) = 0.0_hp !real(d90    ,hp)
                dll_reals(21) = 0.0_hp !real(mudfrac,hp)
                dll_reals(22) = 1.0_hp !real(hidexp ,hp)
                dll_reals(23) = real(ws(nm,kmax,l)   ,hp) ! Vertical velocity near bedlevel
                dll_reals(24) = real(rhosol(l) ,hp)
                dll_reals(25) = real(rhowat(nm,kmax) ,hp) ! Density of sediment and water
                if (lsal > 0) then
                   dll_reals(26) = real(r0(nm,kmax,lsal),hp)
                else
                   dll_reals(26) = real(saleqs,hp)
                endif
                if (ltem > 0) then
                   dll_reals(27) = real(r0(nm,kmax,ltem),hp)
                else
                   dll_reals(27) = real(temeqs,hp)
                endif
                dll_reals(28) = real(ag        ,hp)
                dll_reals(29) = real(vicmol    ,hp)
                dll_reals(30) = real(taub      ,hp)
                !
                if (max_integers < 4) then
                   write(errmsg,'(a,a,a)') 'Insufficient space to pass integer values to transport routine.'
                   call prterr (lundia,'U021', trim(errmsg))
                   call d3stop(1, gdp)
                endif
                call nm_to_n_and_m(nm, n, m, gdp)
                dll_integers( 1) = nm
                dll_integers( 2) = n
                dll_integers( 3) = m
                dll_integers( 4) = l
                !
                if (max_strings < 2) then
                   write(errmsg,'(a,a,a)') 'Insufficient space to pass strings to transport routine.'
                   call prterr (lundia,'U021', trim(errmsg))
                   call d3stop(1, gdp)
                endif
                dll_strings( 1) = gdp%runid
                dll_strings( 2) = dll_usrfil(l)
                !
                ! Initialisation of output variables of user defined transport formulae
                !
                sink_dll    = 0.0_hp
                sour_dll    = 0.0_hp
                message     = ' '
                !
                ! psem/vsem is used to be sure this works fine in DD calculations
                !
                call psemlun
                error = perf_function_erosilt(dll_handle(l)   , dll_function(l)   , &
                                              dll_integers    , max_integers      , &
                                              dll_reals       , max_reals         , &
                                              dll_strings     , max_strings       , &
                                              sink_dll        , sour_dll          , &
                                              message)
                call vsemlun
                if (error /= 0) then
                   write(errmsg,'(a,a,a)') 'Cannot find function "',trim(dll_function(l)),'" in dynamic library.'
                   call prterr (lundia,'U021', trim(errmsg))
                   call d3stop(1, gdp)
                endif
                if (message /= ' ') then
                   write (lundia,'(a,a,a)') '*** ERROR Message from user defined erosion/deposition formulae ',trim(dll_function(l)),' :'
                   write (lundia,'(a,a  )') '          ', trim(message)
                   write (lundia,'(a    )') ' '
                   call d3stop(1, gdp)
                endif
                !
                ! Output parameters
                !
                sour    = real(sour_dll,fp)
                sink    = real(sink_dll,fp)
             endif
          endif
          !
          wstau(nm)     = ws(nm, kmax, l) * sink
          if (.not.flmd2l) then
             if (oldmudfrac) then
                sour = fixfac(nm, l) * sour
             else
                sour = fixfac(nm, l) * frac(nm,l) * sour
             endif
          endif
          sourse(nm, l) = sour / thick0
          sinkse(nm, l) = wstau(nm) / thick1
          !
          ! sediment reference cell is always kmax for cohesive sediment
          !
          kmxsed(nm, l) = kmax
          !
       enddo ! next nm point
    enddo ! next sediment fraction
end subroutine erosilt
