subroutine eqtran(nm        ,ised      ,sig       ,thick     ,kmax      , &
                & h1        ,aks       ,ustarc    ,ws        ,ltur      , &
                & frac      ,tp        ,dstar     ,hrms      ,rlabda    , &
                & di50      ,d90       ,sigmol    ,rhosol    ,uuu       , &
                & vvv       ,umod      ,zumod     ,z0rou     , &
                & ce_nm     ,taurat    ,dicww     ,seddif    ,rsedeq    , &
                & kmaxsd    ,crep      ,sbcu      ,sbcv      ,sbwu      , &
                & sbwv      ,sswu      ,sswv      ,lundia    , &
                & uorb      ,rhowat    ,z0cur     ,teta      ,taucr0    , &
                & d10       ,taubmx    ,dss       ,rksrs     ,i2d3d     , &
                & ce_nmtmp  ,akstmp    ,mudfrac   ,lsecfl    ,spirint   , &
                & hidexp    ,suspfrac  ,ust2      ,tetacr    ,sa        , &
                & salmax    ,ws0       ,t_relax   ,dis       ,concin    , &
                & dzduu     ,dzdvv     ,ubot      ,temp      ,gdp       )
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
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                             , pointer :: eps
    integer                              , pointer :: max_integers
    integer                              , pointer :: max_reals
    integer                              , pointer :: max_strings
    character(256), dimension(:)         , pointer :: dll_function
    integer,        dimension(:)         , pointer :: dll_handle
    integer       , dimension(:)         , pointer :: dll_integers
    real(hp)      , dimension(:)         , pointer :: dll_reals
    character(256), dimension(:)         , pointer :: dll_strings
    character(256), dimension(:)         , pointer :: dll_usrfil
    integer,        dimension(:)         , pointer :: iform
    real(fp),       dimension(:,:)       , pointer :: par
    real(fp)                             , pointer :: sus
    real(fp)                             , pointer :: bed
    real(fp)                             , pointer :: susw
    real(fp)                             , pointer :: bedw
    real(fp)                             , pointer :: espir
    real(fp)                             , pointer :: rhow
    real(fp)                             , pointer :: ag
    real(fp)                             , pointer :: z0
    real(fp)                             , pointer :: vonkar
    real(fp)                             , pointer :: vicmol
    logical                              , pointer :: wind
    logical                              , pointer :: wave
    logical                              , pointer :: sedim
    logical                              , pointer :: scour
    logical                              , pointer :: epspar
    logical                              , pointer :: ubot_from_com
    real(fp)                             , pointer :: timsec
    real(fp)                             , pointer :: camax
    real(fp)                             , pointer :: dclay
    real(fp)                             , pointer :: dsilt
    real(fp)                             , pointer :: dsand
    real(fp)                             , pointer :: aksfac
    real(fp)                             , pointer :: rwave
    real(fp)                             , pointer :: rdc
    real(fp)                             , pointer :: rdw
    real(fp)                             , pointer :: pangle
    real(fp)                             , pointer :: fpco
    integer                              , pointer :: iopsus
    integer                              , pointer :: iopkcw
    integer                              , pointer :: subiw
!
! Global variables
!
    integer                             :: i2d3d
    integer                             :: kmax     !  Description and declaration in iidim.f90
    integer                             :: kmaxsd
    integer                             :: ised     !  i-th sediment
    integer                             :: lsecfl   !  Description and declaration in iidim.f90
    integer                             :: ltur     !  Description and declaration in iidim.f90
    integer                             :: lundia   !  Description and declaration in inout.igs
    integer                             :: nm
    integer                             :: n
    integer                             :: m
    real(fp)                            :: aks      !  Description and declaration in rjdim.f90
    real(fp)                            :: akstmp
    real(fp)                            :: ce_nm
    real(fp)                            :: ce_nmtmp
    real(fp)  , dimension(kmax)         :: concin
    real(fp)                            :: crep
    real(fp)                            :: d10
    real(fp)                            :: d90
    real(fp)                            :: di50
    real(fp)                            :: drho
    real(fp)              , intent(in)  :: dis
    real(fp)              , intent(out) :: dss      !  Description and declaration in rjdim.f90
    real(fp)              , intent(in)  :: dstar
    real(fp)              , intent(in)  :: frac     !  Description and declaration in rjdim.f90
    real(fp)              , intent(in)  :: dzduu     !  Description and declaration in rjdim.f90
    real(fp)              , intent(in)  :: dzdvv     !  Description and declaration in rjdim.f90
    real(fp)                            :: fsilt
    real(fp)                            :: h1
    real(fp)                            :: hidexp
    real(fp)                            :: hrms     !  Description and declaration in rjdim.f90
    real(fp)                            :: muc
    real(fp)              , intent(in)  :: mudfrac
    real(fp)                            :: ra
    real(fp)                            :: rhosol   !  Description and declaration in rjdim.f90
    real(fp)                            :: rhowat   !  Description and declaration in rjdim.f90
    real(fp)                            :: rksrs    !  Description and declaration in rjdim.f90
    real(fp)                            :: rlabda   !  Description and declaration in rjdim.f90
    real(fp)                            :: sa
    real(fp)                            :: salmax
    real(fp)                            :: sbcu
    real(fp)                            :: sbcv
    real(fp)                            :: sbwu
    real(fp)                            :: sbwv
    real(fp)                            :: sswu
    real(fp)                            :: sswv
    real(fp)              , intent(in)  :: sigmol   !  Description and declaration in rjdim.f90
    real(fp)              , intent(in)  :: spirint  !  Spiral flow intensity
    real(fp)                            :: t_relax
    real(fp)                            :: ta
    real(fp)                            :: taubcw
    real(fp)              , intent(out) :: taubmx   !  Description and declaration in rjdim.f90
    real(fp)                            :: tauc
    real(fp)              , intent(in)  :: taucr0
    real(fp)                            :: taucr1
    real(fp)                            :: taurat
    real(fp)                            :: tauwav
    real(fp)                            :: temp
    real(fp)                            :: teta     !  Description and declaration in rjdim.f90
    real(fp)              , intent(in)  :: tetacr
    real(fp)                            :: tp       !  Description and declaration in rjdim.f90
    real(fp)                            :: umod
    real(fp)                            :: ubot     !  Description and declaration in rjdim.f90
    real(fp)                            :: uorb     !  Description and declaration in rjdim.f90
    real(fp)                            :: ustarc
    real(fp)              , intent(out) :: ust2
    real(fp)                            :: usus     !  Description and declaration in rjdim.f90
    real(fp)                            :: uuu
    real(fp)                            :: uwb
    real(fp)                            :: vvv
    real(fp)                            :: ws0
    real(fp)                            :: z0cur
    real(fp)                            :: z0rou
    real(fp)                            :: zumod
    real(fp)                            :: zusus
    real(fp), dimension(0:kmax)         :: dicww    !  Description and declaration in rjdim.f90
    real(fp), dimension(0:kmax)         :: seddif   !  Description and declaration in rjdim.f90
    real(fp), dimension(0:kmax)         :: ws       !  Description and declaration in rjdim.f90
    real(fp), dimension(kmax)           :: rsedeq   !  Description and declaration in rjdim.f90
    real(fp), dimension(kmax)           :: sig      !  Description and declaration in rjdim.f90
    real(fp), dimension(kmax)           :: thick    !  Description and declaration in rjdim.f90
    logical                             :: suspfrac !  suspended sediment fraction
!
! Local variables
!
    integer           :: kvalue    
    integer           :: ierror
    integer           :: k
    integer           :: kode      ! ancient flag: always equal to 1
    integer           :: ntrsi     ! ancient flag: superceded by sbc_total/sus_total
    integer, external :: perf_function_eqtran
    real(fp)          :: aks0
    real(fp)          :: alphaspir
    real(fp)          :: avgcu
    real(fp)          :: avgu
    real(fp)          :: bakdif
    real(fp)          :: cesus
    real(fp)          :: chezy
    real(fp)          :: cosa
    real(fp)          :: deltas
    real(fp)          :: delw
    real(fp)          :: delm
    real(fp)          :: delr
    real(fp)          :: diffbt
    real(fp)          :: dz
    real(fp)          :: dzdx
    real(fp)          :: dzdy
    real(fp)          :: ee
    real(fp)          :: facce
    real(fp)          :: fact1
    real(fp)          :: fcc
    real(fp)          :: ff
    real(fp)          :: fc1
    real(fp)          :: fi
    real(fp)          :: fw1
    real(fp)          :: h
    real(fp)          :: lci
    real(fp)          :: phicur
    real(fp)          :: rz
    real(fp)          :: sag
    real(fp)          :: sbot
    real(fp)          :: sina
    real(fp)          :: sk
    real(fp)          :: ssus
    real(fp)          :: ssusx
    real(fp)          :: ssusy
    real(fp)          :: txg
    real(fp)          :: tyg
    real(fp)          :: u
    real(fp)          :: u2dhim
    real(fp)          :: uon
    real(fp)          :: uoff
    real(fp)          :: utot
    real(fp)          :: uwbih
    real(fp)          :: uwc
    real(fp)          :: v
    real(fp)          :: z
    real(fp)          :: fdamp
    real(fp)          :: psi
    real(fp)          :: htdif
    real(fp)          :: apower
    real(fp)          :: cavg
    real(fp)          :: avgcucor   
    real(fp)          :: epsbed
    real(fp)          :: epsmax
    real(fp)          :: epsmxc
    real(fp)          :: tauadd
    logical           :: error
 
    ! Interface to dll is in High precision!
    !
    real(hp)          :: cesus_dll
    real(hp)          :: sbc_dll
    real(hp)          :: sbcu_dll
    real(hp)          :: sbcv_dll
    real(hp)          :: sbwu_dll
    real(hp)          :: sbwv_dll
    real(hp)          :: ssus_dll
    real(hp)          :: sswu_dll
    real(hp)          :: sswv_dll
    real(hp)          :: t_relax_dll
    character(256)    :: errmsg
    character(256)    :: message     ! Contains message from
    logical           :: equi_conc   ! equilibrium concentration near bedlevel
    logical           :: sbc_total   ! total bed load given (instead of m,n components)
    logical           :: sus_total   ! total suspended load given (instead of m,n components)
!
!! executable statements -------------------------------------------------------
!
    eps                 => gdp%gdconst%eps
    max_integers        => gdp%gdeqtran%max_integers
    max_reals           => gdp%gdeqtran%max_reals
    max_strings         => gdp%gdeqtran%max_strings
    dll_function        => gdp%gdeqtran%dll_function
    dll_handle          => gdp%gdeqtran%dll_handle
    dll_integers        => gdp%gdeqtran%dll_integers
    dll_reals           => gdp%gdeqtran%dll_reals
    dll_strings         => gdp%gdeqtran%dll_strings
    dll_usrfil          => gdp%gdeqtran%dll_usrfil
    iform               => gdp%gdeqtran%iform
    par                 => gdp%gdeqtran%par
    sus                 => gdp%gdmorpar%sus
    bed                 => gdp%gdmorpar%bed
    susw                => gdp%gdmorpar%susw
    bedw                => gdp%gdmorpar%bedw
    espir               => gdp%gdmorpar%espir
    epspar              => gdp%gdmorpar%epspar 
    rhow                => gdp%gdphysco%rhow
    ag                  => gdp%gdphysco%ag
    z0                  => gdp%gdphysco%z0
    vonkar              => gdp%gdphysco%vonkar
    vicmol              => gdp%gdphysco%vicmol
    wind                => gdp%gdprocs%wind
    wave                => gdp%gdprocs%wave
    sedim               => gdp%gdprocs%sedim
    scour               => gdp%gdscour%scour
    timsec              => gdp%gdinttim%timsec
    camax               => gdp%gdmorpar%camax
    dclay               => gdp%gdmorpar%dclay
    dsilt               => gdp%gdmorpar%dsilt
    dsand               => gdp%gdmorpar%dsand
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
    !
    ierror    = 0
    equi_conc = .false.
    sbc_total = .false.
    sus_total = .false.
    akstmp    = aks
    kode  = 1
    ntrsi = 1
    !
    cesus  = 0.0_fp
    sbot   = 0.0_fp
    sbcu   = 0.0_fp
    sbcv   = 0.0_fp
    ssus   = 0.0_fp
    ssusx  = 0.0_fp
    ssusy  = 0.0_fp
    sbwu   = 0.0_fp
    sbwv   = 0.0_fp
    sswu   = 0.0_fp
    sswv   = 0.0_fp
    ee     = exp(1.0_fp)
    sag    = sqrt(ag)
    !
    par(3,ised) = rhosol
    par(4,ised) = (rhosol-rhow) / rhow
    par(6,ised) = di50
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
    dzdx=dzduu
    dzdy=dzdvv
    if (suspfrac) then
       !
       ! Suspended sediment (mud or sand)
       !
       if (iform(ised) == -2) then
          call bedbc2004(tp        ,rhosol    ,rhowat    , &
                       & h1        ,umod      ,d10       ,zumod     ,di50      , &
                       & d90       ,z0cur     ,z0rou     ,drho      ,dstar     , &
                       & taucr0    ,u2dhim    ,aks       ,ra        ,usus      , &
                       & zusus     ,uwb       ,muc       ,tauwav    ,ustarc    , &
                       & tauc      ,taurat    ,ta        ,ce_nm     ,dss       , &
                       & uwc       ,uuu       ,vvv       ,rlabda    , &
                       & hrms      ,delw      ,uon       ,uoff      ,uwbih     , &
                       & delm      ,fc1       ,fw1       ,phicur    ,rksrs     , &
                       & i2d3d     ,mudfrac   ,fsilt     ,taucr1    ,psi       , &
                       & dzduu     ,dzdvv     ,eps       ,camax     ,dsilt     , &
                       & dsand     ,iopsus    ,ag        ,wave      ,tauadd    ) 
       else
          call bedbc1993(tp        ,uorb      ,rhowat    ,h1        ,umod      , &
                       & zumod     ,di50      ,d90       ,z0cur     ,z0rou     , &
                       & dstar     ,taucr0    ,aks       ,usus      ,zusus     , &
                       & uwb       ,delr      ,muc       ,tauwav    ,ustarc    , &
                       & tauc      ,taubcw    ,taurat    ,ta        ,ce_nm     , &
                       & dss       ,mudfrac   ,eps       ,aksfac    ,rwave     , &
                       & camax     ,rdc       ,rdw       ,iopkcw    ,iopsus    , &
                       & vonkar    ,wave      ,tauadd    )
       endif
       !
       ! Find bottom cell for SAND sediment calculations and store for use
       ! in DIFU and DIF_WS
       !
       kmaxsd = 1
       do k = kmax - 1, 1, -1
          !
          ! Calculate level of lower cell interface
          !
          lci = (1.0_fp + sig(k) - thick(k)/2.0_fp) * h1
          if (lci >= aks) then
             kmaxsd = k
             exit
          endif
       enddo
       if (scour) then
          !
          ! copy to field
          !
          taubmx = tauc
       endif
       !
       ! Adjust ce_nm for presence of multiple sediment fractions.
       !
       if (iform(ised) <= 0) then
          ce_nm    = ce_nm * frac
          ce_nmtmp = ce_nm
          akstmp   = aks
       endif
       !
       ! Calculate vertical sediment diffusion coefficient
       !
       if (iform(ised) == -2) then
          !
          ! Calculate sediment mixing due to waves following
          ! Van Rijn 2004 - intra-wave approach for bed load (original TR2004)
          !
          if (ce_nm > 1.0e-6_fp) then
             call calseddf2004(ustarc    ,ws        ,tp        ,hrms      ,h1        , &
                             & seddif    ,kmax      ,sig       ,thick     ,dicww     , &
                             & tauwav    ,tauc      ,ltur      ,delw      ,rhowat    , &
                             & uwbih     ,aks       ,ce_nm     ,ce_nmtmp  ,deltas    , &
                             & akstmp    ,di50      ,sa        ,ws0       ,fdamp     , &
                             & psi       ,epsbed    ,epsmax    ,epsmxc    ,epspar    , &
                             & eps       ,dsand     ,bed       ,vonkar    ,wave      )
          else
             do k=1, kmax
                seddif(k) = dicww(k)
             enddo
          endif
       else
          call calseddf1993(ustarc    ,ws        ,tp        ,delr      ,dstar     , &
                          & uwb       ,hrms      ,h1        ,seddif    ,kmax      , &
                          & sig       ,thick     ,dicww     ,tauwav    ,tauc      , &
                          & ltur      ,eps       ,vonkar    ,wave      )
       endif
       !
       ! Calculate equilibrium concentration profile for sediment
       ! Note: option of selecting either Rouse profile or solution
       ! by numerical integration has been removed; only numerical
       ! integration.
       ! set background diffusion and effective beta factor
       !
       bakdif = vicmol / sigmol
       !
       ! Use simple expression based on upwind approximation for
       ! concentration and fall velocity, and central difference for
       ! concentration gradient.
       ! solution to stationary advection/diffusion equation in vertical.
       !
       if (iform(ised) == -2) then
          !
          ! In case of Van Rijn 2004
          !
          if (i2d3d==2 .or. epspar) then
             !
             ! write concentration concin in an empty array if 2D or
             ! Van Rijn's parametric model and susp. transp. due to waves is used       
             !
             if (ce_nm>1.0e-6_fp) then
                rsedeq(kmaxsd+1) = ce_nmtmp
                aks0             = akstmp / h1
                do k = kmaxsd, 1, -1
                   if (rsedeq(k+1) > 1.0e-6_fp) then
                      dz        = h1 * (sig(k)-sig(k+1))
                      fi        = max(1.0_fp + ((rsedeq(k+1)/0.65_fp)**0.8_fp) - 2.0_fp*((rsedeq(k+1)/0.65_fp)**0.4_fp), 0.01_fp)
                      seddif(k) = seddif(k) * fi                      
                      fcc       = -ws(k) * rsedeq(k+1) * (max(1.0_fp-rsedeq(k+1),0.5_fp))**5.0_fp / seddif(k)
                      ff        = 1.0_fp / rsedeq(k+1) * fcc
                      rsedeq(k) = exp(log(rsedeq(k+1))+dz*ff)
                   else
                      rsedeq(k) = 0.0_fp
                   endif
                enddo
                !
                ! And then work down
                !
                if (kmax > kmaxsd+1) then
                   do k = kmaxsd + 2, kmax
                      rsedeq(k) = rsedeq(k-1)
                   enddo
                endif
                do k = 1,kmax
                   rsedeq(k) = rsedeq(k) * rhosol
                   concin(k) = rsedeq(k)
                enddo
             else
                !
                ! ce_nm<=1.0e-6_fp
                !
                do k = 1, kmax
                   rsedeq(k) = 0.0_fp
                   concin(k) = 0.0_fp
                enddo
             endif
          else
             !
             ! use the r0 values copied into concin array by erosed
             !
             if (ce_nm<=1.0e-6_fp) then
                do k = 1, kmax
                   rsedeq(k) = 0.0_fp
                enddo
             endif
          endif
       else
          !
          ! If transport formula is not Transpor2004
          !
          if (ce_nm>1.0e-6_fp .or. iform(ised)>0) then
             ! Use simple expression based on upwind approximation for
             ! concentration and fall velocity, and central difference for
             ! concentration gradient.
             ! solution to stationary advection/diffusion equation in vertical.
             !
             aks0 = aks / h1
             !
             ! Put concentration in kmaxsd cell
             !
             sk     = 1.0_fp + sig(kmaxsd)
             dz     = h1 * (sk-aks0)
             diffbt = seddif(kmaxsd) + bakdif
             diffbt = max(diffbt , 0.1_fp*ws(kmaxsd)*dz)
             fact1  = 1.0_fp + dz * ws(kmaxsd) / diffbt
             !
             ! In case of other formulation than Van Rijn, ce_nm is not known but
             ! crep is given (computed furtheron). Compute vertical using ce_nm=1
             ! to obtain relation between ce_nm and crep
             !
             if (iform(ised) <= 0) then
                !
                ! Van Rijn
                !
                rsedeq(kmaxsd) = ce_nm / fact1 * rhosol
             else
                !
                ! All other formulations based on 2DH formulae
                !
                rsedeq(kmaxsd) = 1.0_fp
             endif
             !
             ! Now work upward
             !
             do k = kmaxsd - 1, 1, -1
                sk = 1.0_fp + sig(k)
                !
                ! Set diffusion coefficient at bottom of layer
                !
                diffbt    = seddif(k) + bakdif
                diffbt    = max(diffbt , 0.1_fp*ws(k)*dz)
                dz        = h1 * (sig(k)-sig(k+1))
                fact1     = 1.0_fp + dz * ws(k) / diffbt
                rsedeq(k) = rsedeq(k+1) / fact1
             enddo
             !
             ! And then work down
             !
             do k = kmaxsd + 1, kmax
                rsedeq(k) = rsedeq(k-1)
             enddo
          else
             !
             ! if ce_nm <= 1.0e-6 and iform(ised) <= 0 and .not.iform(ised) == -2
             !
             do k = 1, kmax
                rsedeq(k) = 0.0_fp
             enddo
          endif
       endif ! end calculation of equilibrium concentration profile for sediment
       !
       ! Compute depth-averaged velocity, representative concentration and transport
       !
       ! imaginary "depth-averaged current" which has a logarithmic
       ! velocity profile, and a velocity at the bottom zeta point equivalent
       ! to that calculated by the model for 3D current and waves is
       ! calculated in bedbc2004/ (also 1993??) = u2dhim
       !
       avgu     = 0.0_fp
       avgcu    = 0.0_fp
       avgcucor = 0.0_fp
       if (zumod > 0.0_fp) then
          do k = 1, kmax
             z     = (1.0_fp + sig(k)) * h1
             u     = log(1.0_fp + z/z0rou)
             avgu  = avgu  + u*thick(k)
             avgcu = avgcu + u*rsedeq(k)*thick(k)
          enddo
          crep = avgcu / max(avgu,eps)
          avgu = avgu * umod / log(1.0_fp + zumod/z0rou)
       else
          crep = 0.0_fp
       endif
    else
       !
       ! Non suspended sediment (bedload)
       !
       if (kmax == 1) then
          !
          ! for kmax == 1, zumod should be
          ! (1+h1/z0rou)**(z0rou/h1)*exp(-1)*(z0rou+h1)-z0rou
          ! the general formula below (kmax>1) then gives
          !
          avgu = umod
          !
          ! we need to set it here directly because zumod was
          ! approximated in DWNVEL as exp(-1)*h1
          !
       else
          if (zumod > 0.0_fp) then
             !
             ! the numerical integration
             !
             !avgu = 0.0
             !do k = 1, kmax
             !   z = (1.0 + sig(k))*h1
             !   u = log(1.0 + z/z0rou)
             !   avgu  = avgu  + u*thick(k)
             !enddo
             !
             ! can be replaced by analytical integration
             !
             avgu = (z0rou/h1+1.0_fp)*log(1.0_fp+h1/z0rou) - 1.0_fp
             avgu = avgu * umod / log(1.0_fp+zumod/z0rou)
          else
             avgu = 0.0_fp
          endif
       endif
       kmaxsd = kmax
    endif
    rz    = 1.0_fp + h1 / (max(1.0e-8_fp , ee*z0rou))
    chezy = sag * log(rz) / vonkar
    h     = h1
    if (scour) then
       utot = ustarc * chezy / sag
    else
       utot = avgu
    endif
    u     = utot * uuu / (umod+eps)
    v     = utot * vvv / (umod+eps)
    !dzdx  = 0.0_fp
    !dzdy  = 0.0_fp
    if (iform(ised) == -1) then
       if (bed > 0.0_fp) then
          call bedtr1993(uuu       ,vvv       ,utot      ,di50      ,d90       , &
                       & h1        ,taurat    ,ustarc    ,muc       ,rhosol    , &
                       & dstar     ,ws(1)     ,hrms      ,tp        ,teta      , &
                       & rlabda    ,umod      ,sbcu      ,sbcv      ,sbwu      , &
                       & sbwv      ,sswu      ,sswv      ,lundia    ,rhow      , &
                       & ag        ,wave      ,eps       ,error     )
          if (error) call d3stop(1, gdp)
       endif
       sbc_total = .false.
       sus_total = .false.
    elseif (iform(ised) == -2) then
       !
       ! VAN RIJN 2004 Instantaneous bed load
       !
       if ((bed>0.0_fp .or. bedw>0.0_fp .or. susw>0.0_fp) .and. ce_nm>0.0_fp) then
          call bedtr2004(u2dhim    ,di50      ,d90       ,h1        ,rhosol    , &
                       & tp        ,teta      ,lundia    ,uon       ,uoff      , &
                       & uwb       ,taucr1    ,delm      ,ra        ,z0cur     , &
                       & fc1       ,fw1       ,dstar     ,drho      ,phicur    , &
                       & sbcu      ,sbcv      ,sbwu      ,sbwv      ,sswu      , &
                       & sswv      ,tetacr    ,aks       ,fsilt     ,sig       , &
                       & thick     ,concin    ,kmax      ,deltas    ,ws(1)     , &
                       & rksrs     ,dzduu     ,dzdvv     ,rhow      , &
                       & ag        ,bedw      ,pangle    ,fpco      ,susw      , &
                       & dclay     ,wave      ,eps       ,subiw     ,error     )
          if (error) call d3stop(1, gdp)
       endif
       sbc_total = .false.
       sus_total = .false.
    elseif (iform(ised) == 1) then
       !
       ! Engelund-Hansen
       !
       call tranb1(kode      ,ntrsi      ,utot      ,di50      ,chezy     , &
                 & h         ,par(1,ised),sbot      ,ssus      )
       !
       ! transport formula will return ntrsi = 1 which means
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform(ised) == 2) then
       !
       ! Meyer-Peter-Muller
       !
       call tranb2(kode      ,ntrsi     ,utot       ,di50      ,d90       , &
                 & chezy     ,h         ,par(1,ised),hidexp    ,sbot      , &
                 & ssus      )
       !
       ! transport formula will return ntrsi = 1 which means
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform(ised) == 3) then
       !
       ! Ackers-White
       !
       call tranb3(kode      ,ntrsi      ,utot      ,d90       ,chezy     , &
                 & h         ,par(1,ised),sbot      ,ssus      )
       !
       ! transport formula will return ntrsi = 1 which means
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform(ised) == 4) then
       !
       ! general relation for bed load
       !
       call tranb4(kode       ,ntrsi     ,utot      ,di50      ,chezy     , &
                 & par(1,ised),hidexp    ,sbot      ,ssus      )
       !
       ! transport formula will return ntrsi = 1 which means
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform(ised) == 5) then
       !
       ! Bijker
       !
       call tranb5(kode      ,ntrsi      ,u         ,v         ,di50      , &
                 & d90       ,chezy      ,h         ,hrms      ,tp        , &
                 & teta      ,par(1,ised),dzdx      ,dzdy      ,sbcu      , &
                 & sbcv      ,ssusx      ,ssusy     ,cesus     ,vonkar    )
       !
       ! transport formula will return ntrsi = 2 which means
       !
       sbc_total = .false.
       sus_total = .false.
    elseif (iform(ised) == 6) then
       !
       ! Bailard
       !
       call prterr (lundia,'U021','Bailard method is disabled')
       call d3stop(1, gdp)
       !call tranb6(kode      ,ntrsi     ,utot      ,u          ,v         , &
       !          & chezy     ,h         ,hrms      ,tp         ,teta      , &
       !          & diss      ,dzdx      ,dzdy      ,par(1,ised),sbcu      , &
       !          & sbcv      ,ssusx     ,ssusy     ,gdp        )
       !
       ! transport formula will return ntrsi = 2 which means
       !
       sbc_total = .false.
       sus_total = .false.
    elseif (iform(ised) == 7) then
       !
       ! Van Rijn (1984, modified)
       !
       call tranb7(kode      ,ntrsi      ,utot      ,di50      ,d90       , &
                 & h         ,par(1,ised),sbot      ,ssus      ,vonkar    )
       !
       ! transport formula will return ntrsi = 1 which means
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform(ised) == 8) then
       !
       ! Van Rijn / Ribberink (1994)
       !
       call prterr (lundia,'U021','Van Rijn/Ribberink (1994) method is disabled')
       call d3stop(1, gdp)
       !call tranb8(kode       ,ntrsi     ,u         ,v         ,hrms      , &
       !          & h          ,teta      ,tp        ,di50      ,d90       , &
       !          & diss       ,dzdx      ,dzdy      ,nm        ,nm        , &
       !          & par(1,ised),sbcu      ,sbcv      ,ssusx     ,ssusy     , &
       !          & gdp        )
       !
       ! transport formula will return ntrsi = 2 which means
       !
       sbc_total = .false.
       sus_total = .false.
    elseif (iform(ised) == 9) then
       !
       ! Silt module
       !
       call prterr (lundia,'U021','Original Delft3D-MOR Silt module is disabled')
       call d3stop(1, gdp)
       !call tranb9(kode      ,ntrsi     ,utot      ,h         ,alfs      , &
       !          & sbot      ,ssus      )
       !
       ! transport formula will return ntrsi = 1 which means
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform(ised) == 10) then
       !
       ! Ashida and Michiue
       !
       call prterr (lundia,'U021','Ashida and Michiue method is disabled')
       call d3stop(1, gdp)
       !call trab10(kode       ,ntrsi     ,utot      ,di50      ,chezy     , &
       !          & h          ,cosa      ,sina      ,dzdx      ,dzdy      , &
       !          & par(1,ised),sbot      ,ssus      )
       !
       ! transport formula will return ntrsi = 1 which means
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform(ised) == 11) then
       !
       ! Soulsby and Van Rijn
       !
       call trab11(kode      ,ntrsi     ,u         ,v          ,hrms      , &
                 & h         ,tp        ,di50      ,par(1,ised),sbcu      , &
                 & sbcv      ,ssusx     ,ssusy     ,ubot       ,vonkar    , &
                 & ubot_from_com        )
       !
       ! transport formula will return ntrsi = 2 which means
       !
       sbc_total = .false.
       sus_total = .false.
    elseif (iform(ised) == 12) then
       !
       ! Soulsby
       !
       call trab12(kode      ,ntrsi     ,u         ,v         ,hrms       , &
                 & h         ,tp        ,teta      ,di50      ,par(1,ised), &
                 & sbcu      ,sbcv      ,ssusx     ,ssusy     ,ubot       , &
                 & vonkar    ,ubot_from_com        )
       !
       ! transport formula will return ntrsi = 2 which means
       !
       sbc_total = .false.
       sus_total = .false.
    elseif (iform(ised) == 13) then
       !
       ! test transport (Wang) Fredsoe
       !
       call tran9t(kode      ,utot      ,di50      ,d90        ,chezy     , &
                 & h         ,ntrsi     ,ustarc    ,par(1,ised),sbot      , &
                 & ssus      )
       !
       ! transport formula will return ntrsi = 1 which means
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform(ised) == 14) then
       !
       ! generalized Ashida and Michiue
       !
       call trab14(kode       ,ntrsi     ,utot      ,di50      ,chezy     , &
                 & par(1,ised),hidexp    ,sbot      ,ssus      )
       !
       ! transport formula will return ntrsi = 1 which means
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform(ised) == 15) then
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
       dll_reals( 2) = real(u      ,hp)
       dll_reals( 3) = real(v      ,hp)
       dll_reals( 4) = real(utot   ,hp)
       dll_reals( 5) = real(uuu    ,hp)
       dll_reals( 6) = real(vvv    ,hp)
       dll_reals( 7) = real(umod   ,hp)
       dll_reals( 8) = real(zumod  ,hp)
       dll_reals( 9) = real(h      ,hp)
       dll_reals(10) = real(chezy  ,hp)
       dll_reals(11) = real(hrms   ,hp)
       dll_reals(12) = real(tp     ,hp)
       dll_reals(13) = real(teta   ,hp)
       dll_reals(14) = real(rlabda ,hp)
       dll_reals(15) = real(uorb   ,hp)
       dll_reals(16) = real(di50   ,hp)
       dll_reals(17) = real(dss    ,hp)
       dll_reals(18) = real(dstar  ,hp)
       dll_reals(19) = real(d10    ,hp)
       dll_reals(20) = real(d90    ,hp)
       dll_reals(21) = real(mudfrac,hp)
       dll_reals(22) = real(hidexp ,hp)
       dll_reals(23) = real(ws(1)  ,hp) ! Vertical velocity near bedlevel
       dll_reals(24) = real(rhosol ,hp)
       dll_reals(25) = real(rhowat ,hp) ! Density of sediment and water
       dll_reals(26) = real(sa     ,hp)
       dll_reals(27) = real(temp   ,hp)
       dll_reals(28) = real(ag     ,hp)
       dll_reals(29) = real(vicmol ,hp)
       dll_reals(30) = real(taubmx ,hp)
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
       dll_integers( 4) = ised
       !
       if (max_strings < 2) then
          write(errmsg,'(a,a,a)') 'Insufficient space to pass strings to transport routine.'
          call prterr (lundia,'U021', trim(errmsg))
          call d3stop(1, gdp)
       endif
       dll_strings( 1) = gdp%runid
       dll_strings( 2) = dll_usrfil(ised)
       !
       ! Initialisation of output variables of user defined transport formulae
       !
       sbc_total   = .false. ! may be changed by user defined formulae
       sus_total   = .true.  ! always true for user defined formulae
       sbc_dll     = 0.0_hp
       sbcu_dll    = 0.0_hp
       sbcv_dll    = 0.0_hp
       sbwu_dll    = 0.0_hp
       sbwv_dll    = 0.0_hp
       !
       equi_conc   = .false.
       cesus_dll   = 0.0_hp
       ssus_dll    = 0.0_hp
       sswu_dll    = 0.0_hp
       sswv_dll    = 0.0_hp
       t_relax_dll = 0.0_hp
       message     = ' '
       !
       ! psem/vsem is used to be sure this works fine in DD calculations
       !
       call psemlun
       ierror = perf_function_eqtran(dll_handle(ised), dll_function(ised), &
                                     dll_integers    , max_integers      , &
                                     dll_reals       , max_reals         , &
                                     dll_strings     , max_strings       , &
                                     sbc_total, sbc_dll  , sbcu_dll      , &
                                     sbcv_dll , sbwu_dll , sbwv_dll      , &
                                     equi_conc, cesus_dll, ssus_dll      , &
                                     sswu_dll , sswv_dll , t_relax_dll   , &
                                     message)
       call vsemlun
       if (ierror /= 0) then
          write(errmsg,'(a,a,a)') 'Cannot find function "',trim(dll_function(ised)),'" in dynamic library.'
          call prterr (lundia,'U021', trim(errmsg))
          call d3stop(1, gdp)
       endif
       if (message /= ' ') then
          write (lundia,'(a,a,a)') '*** ERROR Message from user defined transport formulae ',trim(dll_function(ised)),' :'
          write (lundia,'(a,a  )') '          ', trim(message)
          write (lundia,'(a    )') ' '
          call d3stop(1, gdp)
       endif
       !
       ! Output parameters
       !
       sbot    = real(sbc_dll ,fp)
       sbcu    = real(sbcu_dll,fp)
       sbcv    = real(sbcv_dll,fp)
       sbwu    = real(sbwu_dll,fp)
       sbwv    = real(sbwv_dll,fp)
       !
       cesus   = real(cesus_dll,fp)
       ssus    = real(ssus_dll ,fp)
       sswu    = real(sswu_dll ,fp)
       sswv    = real(sswv_dll ,fp)
       !
       t_relax = real(t_relax_dll,fp)
    else
       call prterr (lundia,'U021','Transport formula not recognized')
       call d3stop(1, gdp)
    endif
    !
    if (iform(ised) > 0) then
       !
       ! Change from volume to mass concentrations/transport rates.
       !
       ! Van Rijn 1993 or Van Rijn 2004 include rhosol in the transport
       ! rates, the other formulae don't. So, multiply the transport
       ! rates and concentrations by rhosol now.
       !
       if (sbc_total) then
          sbot  = sbot * rhosol
       else
          sbcu  = sbcu * rhosol
          sbcv  = sbcv * rhosol
       endif
       !
       cesus = cesus * rhosol
       if (sus_total) then
          ssus = ssus * rhosol
       else
          ssusx = ssusx * rhosol
          ssusy = ssusy * rhosol
          ssus  = sqrt(ssusx**2 + ssusy**2)
       endif
       !
       sbwu = sbwu * rhosol
       sbwv = sbwv * rhosol
       sswu = sswu * rhosol
       sswv = sswv * rhosol
    endif
    !
    ! If only bed load transport magnitude is given, then the bed load
    ! should be oriented based on the near bed velocity. The near bed
    ! velocity is given by either the velocity (uuu,vvv) in 3D or
    ! the depth averaged velocity (uuu,vvv) corrected for spiral flow in 2D.
    !
    ust2 = (ag/chezy**2) * umod**2
    if (sbc_total) then
       if (umod > 0.0_fp) then
          !
          ! Correct bed load transport direction for spiral flow intensity
          !
          if (lsecfl == 0) then
             alphaspir = 0.0_fp
          else
             alphaspir = sqrt(ag) / 0.4_fp / chezy
             alphaspir = 12.5_fp * espir * (1.0_fp-0.5_fp*alphaspir)
             alphaspir = alphaspir * spirint / umod
          endif
          txg  = ust2 * (uuu + alphaspir*vvv) / umod
          tyg  = ust2 * (vvv - alphaspir*uuu) / umod
          ust2 = sqrt(txg**2 + tyg**2)
          if (ust2 > eps) then
             cosa = txg / ust2
             sina = tyg / ust2
          else
             cosa = 0.0_fp
             sina = 0.0_fp
          endif
          sbcu  = sbot * cosa
          sbcv  = sbot * sina
       else
          sbcu  = 0.0_fp
          sbcv  = 0.0_fp
      endif
    endif
    !
    ! Adjust for calibration factors
    !
    sbcu = bed  * sbcu
    sbcv = bed  * sbcv
    sbwu = bedw * sbwu
    sbwv = bedw * sbwv
    sswu = susw * sswu
    sswv = susw * sswv
    !
    if (iform(ised)>0) then
       cesus    = sus * cesus
       ssus     = sus * ssus
       ssusx    = sus * ssusx
       ssusy    = sus * ssusy
    else
       ce_nm    = sus * ce_nm
       ce_nmtmp = sus * ce_nmtmp
       do k = 1, kmax
          rsedeq(k) = sus * rsedeq(k)
       enddo
       crep     = sus * crep
    endif
    !
    if (iform(ised)>0 .and. suspfrac) then
       !
       ! If we are not using Van Rijn 1993 or Van Rijn 2004
       ! then we still need to compute values for ce_nm,
       ! ce_nmtmp, crep and rsedeq.
       !
       if (equi_conc) then
           !
           ! Concentration given by transport formula
           !
       else
           !
           ! Suspended transport rate given by transport formula,
           ! derive concentration
           !
           cesus = ssus / (utot+eps) / h1
       endif
       !
       ! Correct for fraction presence in the bed and calibration factor.
       ! Note that this multiplication has been done much earlier for
       ! Van Rijn 1993 and 2004.
       !
       cesus = cesus * frac
       !
       facce = cesus / (crep+eps)
       do k = 1, kmax
          rsedeq(k) = rsedeq(k) * facce
       enddo
       crep     = cesus
       sk       = 1.0_fp + sig(kmaxsd)
       dz       = h1 * (sk-aks0)
       diffbt   = seddif(kmaxsd) + bakdif
       diffbt   = max(diffbt , 0.1_fp*ws(kmaxsd)*dz)
       fact1    = 1.0_fp + dz * ws(kmaxsd) / diffbt
       ce_nm    = fact1 * facce / rhosol
       ce_nmtmp = ce_nm
    elseif (.not.suspfrac) then
       !
       ! Note: in case of bedload sediment type, the suspended load is added to the
       ! bed load to compute the total load. Bed slope effect will be applied to
       ! both the bed- and the suspended part.
       !
       ! The effect of frac is included in the bed load at a later stage.
       !
       if (iform(ised) <= 0) then
           !
           ! Van Rijn 1993 or 2004 formula
           ! NOTE 1: This doesn't work yet because Van Rijn 1993 and 2004
           ! routines get only called if suspfrac=true (so I can't get here
           ! or I don't have the right values).
           ! NOTE 2: crep is based on ce_nm which has been multiplied by
           ! frac above. Since sbcu/v will be multiplied by frac later (again)
           ! there is the risk of double correction. So I would have to
           ! "unmultiply" by division which actually doesn't work if frac=0.
           !
           sbcu = sbcu + crep * h1 * u / frac
           sbcv = sbcv + crep * h1 * v / frac
       elseif (equi_conc) then
           !
           ! Concentration given by transport formula
           !
           sbcu = sbcu + cesus * h1 * u
           sbcv = sbcv + cesus * h1 * v
       elseif (sus_total) then
           !
           ! Total suspended transport rate given by transport formula,
           ! assume that it is transported in depth-averaged direction
           !
           sbcu = sbcu + ssus * u / (utot+eps)
           sbcv = sbcv + ssus * v / (utot+eps)
       else
           !
           ! Suspended transport rate components given by transport
           ! formula, add them individually to the bed load
           !
           sbcu = sbcu + ssusx
           sbcv = sbcv + ssusy
       endif
    endif
    if (comparereal(akstmp,0.0_fp) == 0) then
        akstmp = aks
    endif
end subroutine eqtran
