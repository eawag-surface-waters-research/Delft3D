subroutine wri_nf_inp(u0    ,v0    ,rho    ,thick  ,kmax   ,dps    ,&
                    & s0    ,alfas ,time   ,taua   ,r0     ,lstsci ,&
                    & lsal  ,ltem  ,idensform      ,saleqs ,temeqs ,&
                    & idis  ,filename      ,linkinf, discharges, gdp)

!!--copyright-------------------------------------------------------------------
! Copyright (c) 2009, WL | Delft Hydraulics. All rights reserved.
!!--disclaimer------------------------------------------------------------------
! This code is part of the Delft3D software system. WL|Delft Hydraulics has
! developed c.q. manufactured this code to its best ability and according to the
! state of the art. Nevertheless, there is no express or implied warranty as to
! this software whether tangible or intangible. In particular, there is no
! express or implied warranty as to the fitness for a particular purpose of this
! software, whether tangible or intangible. The intellectual property rights
! related to this software code remain with WL|Delft Hydraulics at all times.
! For details on the licensing agreement, we refer to the Delft3D software
! license and any modifications to this license, if applicable. These documents
! are available upon request.
!!--version information---------------------------------------------------------
! $Author$
! $Date$
! $Revision$
!!--description-----------------------------------------------------------------
!
!    Function: Writes input for generic coupling
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
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
    integer ,dimension(:)          , pointer :: m_diff
    integer ,dimension(:)          , pointer :: n_diff
    integer ,dimension(:)          , pointer :: m_amb
    integer ,dimension(:)          , pointer :: n_amb
    integer ,dimension(:)          , pointer :: k_amb
    integer ,dimension(:)          , pointer :: m_intake
    integer ,dimension(:)          , pointer :: n_intake
    integer ,dimension(:)          , pointer :: k_intake

    real(fp),dimension(:)          , pointer :: q_diff
    real(fp),dimension(:)          , pointer :: t0_diff
    real(fp),dimension(:)          , pointer :: s0_diff
    real(fp),dimension(:)          , pointer :: d0
    real(fp),dimension(:)          , pointer :: h0
    real(fp),dimension(:)          , pointer :: sigma0
    integer                        , pointer :: lunsrc
!
! Global variables
!
    integer                                                     , intent(in) :: idis
    integer                                                     , intent(in) :: kmax
    integer                                                     , intent(in) :: lstsci
    integer                                                     , intent(in) :: lsal
    integer                                                     , intent(in) :: ltem
    integer                                                     , intent(in) :: idensform
    integer                                                     , intent(in) :: nmmax    !  Description and declaration in tricom.igs
    real(fp)                                                    , intent(out):: taua
    real(fp)   , dimension(8)                                   , intent(out):: linkinf
    real(fp)                                                    , intent(in) :: saleqs
    real(fp)                                                    , intent(in) :: temeqs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: alfas
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: s0
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: rho
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: u0
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: v0
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax,lstsci)  , intent(in) :: r0
    real(fp)   , dimension(kmax)                                , intent(in) :: thick
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub) , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub) , intent(in)  :: xz     !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub) , intent(in)  :: yz     !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(kmax)                  , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90 gs
    real(prec) , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: dps
    character*256                                                            :: filename
!
! Local variables
!
    integer                                :: ilen
    integer                                :: k
    integer                                :: nm_diff
    integer                                :: nmd_diff
    integer                                :: ndm_diff
    integer                                :: nm_amb
    integer                                :: nmd_amb
    integer                                :: ndm_amb
    integer                 , external     :: newlun
    integer                                :: luntmp
    real(fp)                               :: deg2rad
    real(fp)                               :: drohj
    real(fp)                               :: ha
    real(fp)                               :: hd
    real(fp)                               :: hint
    real(fp)                               :: pi
    real(fp)                               :: rad2deg
    real(fp)                               :: rhoam
    real(fp)                               :: rhoas
    real(fp)                               :: rhoab
    real(fp)                               :: taurel
    real(fp)                               :: time
    real(fp)                               :: uuu
    real(fp)                               :: ua
    real(fp)                               :: umag
    real(fp)                               :: vvv
    real(fp)                               :: sal
    real(fp)                               :: temp
    real(fp)                               :: dummy
    real(fp)                               :: add
    real(fp)                               :: rho0
    character*1                            :: tab
    character*1                            :: stype1
    character*1                            :: stype2
    character*3                            :: c_inode
    character*12                           :: ctime
    character*12                           :: ctime1
    character*12                           :: cha
    character*12                           :: chd
    character*12                           :: ch0
    character*12                           :: cua
    character*12                           :: crhoam
    character*12                           :: crhoas
    character*12                           :: crhoab
    character*12                           :: chint
    character*12                           :: cdrohj
    character*12                           :: cq0
    character*12                           :: crho0
    character*12                           :: cd0
    character*12                           :: ctaua
    logical                                :: linkinp
!
!
!! executable statements -------------------------------------------------------
!
    m_diff         => gdp%gdnfl%m_diff
    n_diff         => gdp%gdnfl%n_diff
    m_amb          => gdp%gdnfl%m_amb
    n_amb          => gdp%gdnfl%n_amb
    k_amb          => gdp%gdnfl%k_amb
    m_intake       => gdp%gdnfl%m_intake
    n_intake       => gdp%gdnfl%n_intake
    k_intake       => gdp%gdnfl%k_intake
    q_diff         => gdp%gdnfl%q_diff
    s0_diff        => gdp%gdnfl%s0_diff
    t0_diff        => gdp%gdnfl%t0_diff
    d0             => gdp%gdnfl%d0
    h0             => gdp%gdnfl%h0
    sigma0         => gdp%gdnfl%sigma0
    !
    write(c_inode(1:3),'(i3.3)') inode
    !
    pi      = acos(-1.0_fp)
    rad2deg = 180.0_fp / pi
    deg2rad = pi / 180.0_fp
    tab     = char(9)
    
    !
    ! Use the general diffusor characteritics from nf2ff input file
    !
    ! First convert from X,Y,Z to M, N, K
    !
    ! FIXME declaration of var discharges
    
    call findnmk(xz     ,yz     ,dps    ,s1     ,kcs    ,nmmax  ,thick   , &
                 & kmax   , discharges(idis)%x_diff ,discharges(idis)%y_diff  ,0  ,nm_diff ,k_diff  ,gdp )

    call findnmk(xz     ,yz     ,dps    ,s1     ,kcs    ,nmmax  ,thick   , &
                 & kmax   , discharges(idis)%x_amb ,discharges(idis)%y_amb  ,discharges(idis)%z_amb  ,nm_amb ,k_amb  ,gdp )
    
    call findnmk(xz     ,yz     ,dps    ,s1     ,kcs    ,nmmax  ,thick   , &
                 & kmax   , discharges(idis)%x_int ,discharges(idis)%y_int  ,discharges(idis)%z_int  ,nm_int ,k_int  ,gdp )
    
    !FIXME below based on the above
    call n_and_m_to_nm(n_diff(idis)    , m_diff(idis)     , nm_diff  , gdp)
    call n_and_m_to_nm(n_diff(idis) - 1, m_diff(idis)     , ndm_diff , gdp)
    call n_and_m_to_nm(n_diff(idis)    , m_diff(idis) - 1 , nmd_diff , gdp)
    call n_and_m_to_nm(n_amb(idis)     , m_amb(idis)      , nm_amb   , gdp)
    call n_and_m_to_nm(n_amb(idis)  - 1, m_amb(idis)      , ndm_amb  , gdp)
    call n_and_m_to_nm(n_amb(idis)     , m_amb(idis)  - 1 , nmd_amb  , gdp)

    !
    ! Compute the depths
    !

    ha = s0(nm_amb)+real(dps(nm_amb),fp)
    hd = s0(nm_diff)+real(dps(nm_diff),fp)

    !
    ! Compute depth averaged velocity magnitude and direction
    !

    uuu = 0.0_fp  
    vvv = 0.0_fp

    do k = 1, kmax
       uuu      = uuu + 0.5_fp * (u0(nm_amb ,k) + u0(nmd_amb ,k))*thick(k)
       vvv      = vvv + 0.5_fp * (v0(nm_amb ,k) + v0(ndm_amb ,k))*thick(k)
    enddo

    umag = sqrt (uuu*uuu + vvv*vvv)
    taua = atan2(vvv,uuu)*rad2deg + alfas(nm_amb)
    taua = mod(taua + 360.0_fp,360.0_fp)
    ua   = umag

    !
    ! Density profile classification (Cormixtype)
    !

    call determine_densprof(kmax      ,thick     ,s0(nm_amb),real(dps(nm_amb),fp),rho(nm_amb,:),ha        ,hd        , &
                           &stype1    ,stype2    ,rhoam     ,rhoas               ,rhoab        ,hint      ,drohj     )

    !
    ! Compute the density of the discharged water
    !

    sal  = s0_diff(idis)
    temp = t0_diff(idis)
    if (lsal /= 0) then
       call coupled (add,r0,kmax,lstsci,lsal,thick,m_intake(idis),n_intake(idis),k_intake(idis),gdp)
       sal = sal + add
    else
       sal = saleqs
    endif

    if (ltem /= 0) then
       call coupled (add,r0,kmax,lstsci,ltem,thick,m_intake(idis),n_intake(idis),k_intake(idis),gdp)
       temp = temp + add
    else
       temp = temeqs
    endif

    select case (idensform)
       case( dens_Eckart )
          call dens_eck    (temp, sal ,rho0, dummy, dummy)
       case( dens_Unesco)
          call dens_unes   (temp, sal ,rho0, dummy, dummy)
       case( dens_NaClSol)
          call dens_nacl   (temp, sal ,rho0, dummy, dummy)
    end select

    !
    ! Write generic coupling input file
    ! FIXME still cortime

    luntmp = newlun(gdp)
    linkinp   = .true.

    do while (linkinp)
       inquire (file=trim(gdp%gdnfl%base_path)//'cortime_'//trim(gdp%runid)//'_'//c_inode//'.linkinp',exist=linkinp)
    enddo

    open (luntmp,file=trim(gdp%gdnfl%base_path)//'cortime_'//trim(gdp%runid)//'_'//c_inode//'.linkinp',status='new')
    write (luntmp,'(''CorTime v7.0'')')
    write (luntmp,'()')
    write (luntmp,'(''File name='',a1,a )') tab,'cortime_'//trim(gdp%runid)//'_'//c_inode//'.linkinp'
    write (luntmp,'(''Base case='',a1,a )') tab,trim(filename)
    write (luntmp,'(''Node='',a1,a )') tab,trim(gdp%runid)
    write (luntmp,'(''TOTSTEP='' ,a1,i1)') tab,1
    write (luntmp,'()')
    write (luntmp,'(27(a,a1),a)') 'TIME' , tab, 'HA'   , tab, 'HD'   , tab, 'UA'     , tab, &
   &                              'UorS' , tab, 'RHOAM', tab, 'STYPE', tab, 'RHOAS'  , tab, &
   &                              'RHOAB', tab, 'HINT' , tab, 'DROHJ', tab, 'Q0'     , tab, &
   &                              'C0'   , tab, 'RHO0' , tab, 'Gamma', tab, 'Sigma'  , tab, &
   &                              'D0'   , tab, 'B0'   , tab, 'H0'   , tab, 'PollTyp', tab, &
   &                              'L1Sub', tab, 'L1Den', tab, 'L2Sub', tab, 'L2Den'  , tab, &
   &                              'L3Sub', tab, 'L3Den', tab, 'Distb', tab, 'PHI'

    !
    ! Make character strings from all requested input
    !

    write (ctime (1:12),'(f12.3)') time/60.0_fp
    write (cha   (1:12),'(f12.3)') ha
    write (chd   (1:12),'(f12.3)') hd
    write (cua   (1:12),'(f12.3)') max(ua,0.001_fp)
    if (stype1 == 'U') then
       write(crhoam(1:12),'(f12.3)') rhoam
       crhoas = '-'
       crhoab = '-'
       stype2 = '-'
       chint  = '-'
       cdrohj = '-'
    else
       crhoam ='-'
       write (crhoas(1:12),'(f12.3)') rhoas
       write (crhoab(1:12),'(f12.3)') rhoab
       if (stype2 == 'C') then
          write (chint (1:12),'(f12.3)') hint
          write (cdrohj(1:12),'(f12.3)') drohj
       else
          chint  = '-'
          cdrohj = '-'
       endif
    endif

    write (cq0   (1:12),'(f12.8)') q_diff(idis)
    write (crho0 (1:12),'(f12.3)') rho0
    write (ch0   (1:12),'(f12.3)') h0(idis)
    write (cd0   (1:12),'(f12.3)') d0(idis)

    !
    ! sigma0 given as direction relative to north in stead of main flow direction; 0, pointing to east, 90 pointing to north etc.
    ! ctaua is port direction relative to main flow direction
    !

    taua = mod(taua,360.0_fp)

    taurel = mod(sigma0(idis) - taua + 360.0_fp,360.0_fp)
    if (taurel > 179.0_fp .and. taurel < 181.0_fp) then
       taurel = 179.0_fp
    endif

    write (ctaua (1:12),'(f12.3)') taurel

    ctime = adjustl(ctime )
    cha   = adjustl(cha   )
    chd   = adjustl(chd   )
    cua   = adjustl(cua   )
    crhoam= adjustl(crhoam)
    crhoas= adjustl(crhoas)
    crhoab= adjustl(crhoab)
    chint = adjustl(chint )
    cdrohj= adjustl(cdrohj)
    cq0   = adjustl(cq0   )
    crho0 = adjustl(crho0 )
    ch0   = adjustl(ch0   )
    cd0   = adjustl(cd0   )
    ctaua = adjustl(ctaua )

    write (luntmp,'(27(a,a1),a)') trim(ctime)  , tab, trim(cha)    , tab, trim(chd)    , tab, trim(cua)      , tab, &
   &                              trim(stype1) , tab, trim(crhoam) , tab, trim(stype2) , tab, trim(crhoas)   , tab, &
   &                              trim(crhoab) , tab, trim(chint)  , tab, trim(cdrohj) , tab, trim(cq0)      , tab, &
   &                              '1.0'        , tab, trim(crho0)  , tab, '-'          , tab, '-'            , tab, &
   &                              trim(cd0)    , tab, '-'          , tab, '-'          , tab, '-'            , tab, &
   &                              '-'          , tab, '-'          , tab, '-'          , tab, '-'            , tab, &
   &                              '-'          , tab, '-'          , tab, '-'          , tab, trim(ctaua)

    close (luntmp)
    !
    ! Store some general information to write to the plume trajectory file
    !
    linkinf(1) = sal
    linkinf(2) = temp
    linkinf(3) = rho0
    linkinf(4) = ua
    linkinf(5) = taua
    linkinf(6) = taurel
    !
end subroutine wri_nf_inp
