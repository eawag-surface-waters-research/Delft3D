subroutine u_ppr(lundia    ,lunprt    ,error     ,versio    ,prsmap    , &
               & prshis    ,selmap    ,selhis    ,runid     ,rhow      , &
               & grdang    ,dtsec     ,nst       ,iphisc    , &
               & npmap     ,ithisc    ,itmapc    ,itdroc    ,itrstc    , &
               & ftstat    ,ftcros    ,gdp       )
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
!    Function: - Routine similar to POSTPR that may be used by
!                the user to define their own output
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'fsm.i'
    include 'tri-dyn.igd'
    integer               , pointer :: alfas
    integer               , pointer :: atr
    integer               , pointer :: c
    integer               , pointer :: ctr
    integer               , pointer :: dicuv
    integer               , pointer :: dicww
    integer               , pointer :: dp
    integer               , pointer :: dps
    integer               , pointer :: dpsed
    integer               , pointer :: dtr
    integer               , pointer :: enstro
    integer               , pointer :: entr
    integer               , pointer :: fltr
    integer               , pointer :: gro
    integer               , pointer :: guu
    integer               , pointer :: guv
    integer               , pointer :: gvu
    integer               , pointer :: gvv
    integer               , pointer :: hrms
    integer               , pointer :: hu
    integer               , pointer :: hv
    integer               , pointer :: qu
    integer               , pointer :: qxk
    integer               , pointer :: qyk
    integer               , pointer :: r1
    integer               , pointer :: rca
    integer               , pointer :: rho
    integer               , pointer :: rich
    integer               , pointer :: rlabda
    integer               , pointer :: rsed
    integer               , pointer :: rsedeq
    integer               , pointer :: rtur1
    integer               , pointer :: s1
    integer               , pointer :: sbtr
    integer               , pointer :: sbtrc
    integer               , pointer :: sbuu
    integer               , pointer :: sbvv
    integer               , pointer :: sstr
    integer               , pointer :: sstrc
    integer               , pointer :: ssuu
    integer               , pointer :: ssvv
    integer               , pointer :: taubpu
    integer               , pointer :: taubpv
    integer               , pointer :: taubsu
    integer               , pointer :: taubsv
    integer               , pointer :: teta
    integer               , pointer :: thick
    integer               , pointer :: tp
    integer               , pointer :: u1
    integer               , pointer :: umnldf
    integer               , pointer :: uorb
    integer               , pointer :: v1
    integer               , pointer :: vicuv
    integer               , pointer :: vicww
    integer               , pointer :: vmnldf
    integer               , pointer :: vortic
    integer               , pointer :: w1
    integer               , pointer :: wphy
    integer               , pointer :: ws
    integer               , pointer :: xydro
    integer               , pointer :: xz
    integer               , pointer :: yz
    integer               , pointer :: zalfas
    integer               , pointer :: zbdsed
    integer               , pointer :: zcuru
    integer               , pointer :: zcurv
    integer               , pointer :: zcurw
    integer               , pointer :: zdicww
    integer               , pointer :: zdps
    integer               , pointer :: zdpsed
    integer               , pointer :: zenst
    integer               , pointer :: zkfs
    integer               , pointer :: zqxk
    integer               , pointer :: zqyk
    integer               , pointer :: zrca
    integer               , pointer :: zrho
    integer               , pointer :: zrich
    integer               , pointer :: zrsdeq
    integer               , pointer :: zsbu
    integer               , pointer :: zsbv
    integer               , pointer :: zssu
    integer               , pointer :: zssv
    integer               , pointer :: ztauet
    integer               , pointer :: ztauks
    integer               , pointer :: ztur
    integer               , pointer :: zvicww
    integer               , pointer :: zvort
    integer               , pointer :: zwl
    integer               , pointer :: zws
    integer               , pointer :: rl
    integer               , pointer :: p1
    integer               , pointer :: hydprs
    integer               , pointer :: kcs
    integer               , pointer :: kfs
    integer               , pointer :: kfu
    integer               , pointer :: kfv
    integer               , pointer :: mnksrc
    integer               , pointer :: kfumin
    integer               , pointer :: kfvmin
    integer               , pointer :: kfsmin
    integer               , pointer :: kfumax
    integer               , pointer :: kfvmax
    integer               , pointer :: kfsmax
    integer               , pointer :: kfuz1
    integer               , pointer :: kfvz1
    integer               , pointer :: namdro
    integer               , pointer :: wrka1
    integer               , pointer :: wrka2
    integer               , pointer :: wrka3
    integer               , pointer :: wrka4
    integer               , pointer :: wrka5
    integer               , pointer :: wrkb3
    integer               , pointer :: wrkb4
    integer               , pointer :: nmax
    integer               , pointer :: mmax
    integer               , pointer :: nmaxus
    integer               , pointer :: kmax
    integer               , pointer :: lmax
    integer               , pointer :: lmaxd
    integer               , pointer :: lsts
    integer               , pointer :: lstsc
    integer               , pointer :: lstsci
    integer               , pointer :: lsal
    integer               , pointer :: lsed
    integer               , pointer :: lsedtot
    integer               , pointer :: ltem
    integer               , pointer :: ltur
    integer               , pointer :: kc
    integer               , pointer :: nsrc
    integer               , pointer :: nostat
    integer               , pointer :: ntruv
    integer               , pointer :: ntru
    integer               , pointer :: ndro
    real(fp)              , pointer :: timsec
    real(fp)              , pointer :: timmin
    integer               , pointer :: itstrt
    integer               , pointer :: itstop
    integer               , pointer :: julday
    character*256         , pointer :: filus1
    character*256         , pointer :: filus2
    character*256         , pointer :: filus3
    logical               , pointer :: first
    integer               , pointer :: lunus1
    integer               , pointer :: lunus2
    integer               , pointer :: lunus3
    logical               , pointer :: drogue
    logical               , pointer :: wave
    logical               , pointer :: zmodel
!
! Global variables
!
    integer             :: iphisc  !!  Current time counter for printing
                                   !!  history data
    integer             :: itdroc  !!  Current time counter for the dro-
                                   !!  gue data file
    integer             :: ithisc  !!  Current time counter for the his-
                                   !!  tory data file
    integer, intent(in) :: itmapc  !!  Current time counter for the MAP
    integer             :: itrstc  !!  Current time counter for the re-
                                   !!  start file. Start writing after
                                   !!  first interval is passed. Last time
                                   !!  will always be written to file for
                                   !!  ITRSTI > 0
    integer, intent(in) :: lundia  !  Description and declaration in inout.igs
    integer             :: lunprt  !  Description and declaration in inout.igs
    integer             :: npmap
    integer, intent(in) :: nst     !!  Current time step counter
    logical             :: error   !!  Flag=TRUE if an error is encountered
    logical, intent(in) :: ftcros  !!  Flag set when TCROSS is invoked
    logical, intent(in) :: ftstat  !!  Flag set when TSTAT  is invoked
    real(fp)            :: dtsec   !!  Integration time step [in seconds]
    real(fp)            :: grdang  !  Description and declaration in tricom.igs
    real(fp)            :: rhow    !  Description and declaration in rjdim.f90
    character(*)        :: runid   !!  Run identification code for the cur-
                                   !!  rent simulation (used to determine
                                   !!  the names of the in- /output files
                                   !!  used by the system)
    character(*)        :: selmap  !  Description and declaration in tricom.igs
    character(19)       :: prsmap  !  Description and declaration in tricom.igs
    character(23)       :: prshis  !  Description and declaration in tricom.igs
    character(23)       :: selhis  !  Description and declaration in tricom.igs
    character(5)        :: versio  !!  Version nr. of the current package
!
! Local variables
!
    integer                       :: icross
    integer                       :: idate  ! Current simulation date YYYMMDD 
    integer                       :: idro   ! Drogue number selected for output 
    integer                       :: ilay   ! Layer number selected for map output 
    integer                       :: istat  ! Station number selected 
    integer                       :: itime  ! Current simulation time MMHHSS 
    integer                       :: notim  ! No. of time step to be written to the file 
    integer, external             :: newlun
    character(131), dimension(10) :: header ! File header 
!
!! executable statements -------------------------------------------------------
!
    wrka1       => gdp%gdaddress%wrka1
    wrka2       => gdp%gdaddress%wrka2
    wrka3       => gdp%gdaddress%wrka3
    wrka4       => gdp%gdaddress%wrka4
    wrka5       => gdp%gdaddress%wrka5
    wrkb3       => gdp%gdaddress%wrkb3
    wrkb4       => gdp%gdaddress%wrkb4
    filus1      => gdp%gddatusr%filus1
    filus2      => gdp%gddatusr%filus2
    filus3      => gdp%gddatusr%filus3
    nmax        => gdp%d%nmax
    mmax        => gdp%d%mmax
    nmaxus      => gdp%d%nmaxus
    kmax        => gdp%d%kmax
    lmax        => gdp%d%lmax
    lmaxd       => gdp%d%lmaxd
    lsts        => gdp%d%lsts
    lstsc       => gdp%d%lstsc
    lstsci      => gdp%d%lstsci
    lsal        => gdp%d%lsal
    lsed        => gdp%d%lsed
    lsedtot     => gdp%d%lsedtot
    ltem        => gdp%d%ltem
    ltur        => gdp%d%ltur
    kc          => gdp%d%kc
    nsrc        => gdp%d%nsrc
    nostat      => gdp%d%nostat
    ntruv       => gdp%d%ntruv
    ntru        => gdp%d%ntru
    ndro        => gdp%d%ndro
    timsec      => gdp%gdinttim%timsec
    timmin      => gdp%gdinttim%timmin
    itstrt      => gdp%gdinttim%itstrt
    itstop      => gdp%gdinttim%itstop
    julday      => gdp%gdinttim%julday
    drogue      => gdp%gdprocs%drogue
    wave        => gdp%gdprocs%wave
    zmodel      => gdp%gdprocs%zmodel
    alfas       => gdp%gdr_i_ch%alfas
    atr         => gdp%gdr_i_ch%atr
    c           => gdp%gdr_i_ch%c
    ctr         => gdp%gdr_i_ch%ctr
    dicuv       => gdp%gdr_i_ch%dicuv
    dicww       => gdp%gdr_i_ch%dicww
    dp          => gdp%gdr_i_ch%dp
    dps         => gdp%gdr_i_ch%dps
    dpsed       => gdp%gdr_i_ch%dpsed
    dtr         => gdp%gdr_i_ch%dtr
    enstro      => gdp%gdr_i_ch%enstro
    entr        => gdp%gdr_i_ch%entr
    fltr        => gdp%gdr_i_ch%fltr
    gro         => gdp%gdr_i_ch%gro
    guu         => gdp%gdr_i_ch%guu
    guv         => gdp%gdr_i_ch%guv
    gvu         => gdp%gdr_i_ch%gvu
    gvv         => gdp%gdr_i_ch%gvv
    hrms        => gdp%gdr_i_ch%hrms
    hu          => gdp%gdr_i_ch%hu
    hv          => gdp%gdr_i_ch%hv
    qu          => gdp%gdr_i_ch%qu
    qxk         => gdp%gdr_i_ch%qxk
    qyk         => gdp%gdr_i_ch%qyk
    r1          => gdp%gdr_i_ch%r1
    rca         => gdp%gdr_i_ch%rca
    rho         => gdp%gdr_i_ch%rho
    rich        => gdp%gdr_i_ch%rich
    rlabda      => gdp%gdr_i_ch%rlabda
    rsed        => gdp%gdr_i_ch%rsed
    rsedeq      => gdp%gdr_i_ch%rsedeq
    rtur1       => gdp%gdr_i_ch%rtur1
    s1          => gdp%gdr_i_ch%s1
    sbtr        => gdp%gdr_i_ch%sbtr
    sbtrc       => gdp%gdr_i_ch%sbtrc
    sbuu        => gdp%gdr_i_ch%sbuu
    sbvv        => gdp%gdr_i_ch%sbvv
    sstr        => gdp%gdr_i_ch%sstr
    sstrc       => gdp%gdr_i_ch%sstrc
    ssuu        => gdp%gdr_i_ch%ssuu
    ssvv        => gdp%gdr_i_ch%ssvv
    taubpu      => gdp%gdr_i_ch%taubpu
    taubpv      => gdp%gdr_i_ch%taubpv
    taubsu      => gdp%gdr_i_ch%taubsu
    taubsv      => gdp%gdr_i_ch%taubsv
    teta        => gdp%gdr_i_ch%teta
    thick       => gdp%gdr_i_ch%thick
    tp          => gdp%gdr_i_ch%tp
    u1          => gdp%gdr_i_ch%u1
    umnldf      => gdp%gdr_i_ch%umnldf
    uorb        => gdp%gdr_i_ch%uorb
    v1          => gdp%gdr_i_ch%v1
    vicuv       => gdp%gdr_i_ch%vicuv
    vicww       => gdp%gdr_i_ch%vicww
    vmnldf      => gdp%gdr_i_ch%vmnldf
    vortic      => gdp%gdr_i_ch%vortic
    w1          => gdp%gdr_i_ch%w1
    wphy        => gdp%gdr_i_ch%wphy
    ws          => gdp%gdr_i_ch%ws
    xydro       => gdp%gdr_i_ch%xydro
    xz          => gdp%gdr_i_ch%xz
    yz          => gdp%gdr_i_ch%yz
    zalfas      => gdp%gdr_i_ch%zalfas
    zbdsed      => gdp%gdr_i_ch%zbdsed
    zcuru       => gdp%gdr_i_ch%zcuru
    zcurv       => gdp%gdr_i_ch%zcurv
    zcurw       => gdp%gdr_i_ch%zcurw
    zdicww      => gdp%gdr_i_ch%zdicww
    zdps        => gdp%gdr_i_ch%zdps
    zdpsed      => gdp%gdr_i_ch%zdpsed
    zenst       => gdp%gdr_i_ch%zenst
    zkfs        => gdp%gdr_i_ch%zkfs
    zqxk        => gdp%gdr_i_ch%zqxk
    zqyk        => gdp%gdr_i_ch%zqyk
    zrca        => gdp%gdr_i_ch%zrca
    zrho        => gdp%gdr_i_ch%zrho
    zrich       => gdp%gdr_i_ch%zrich
    zrsdeq      => gdp%gdr_i_ch%zrsdeq
    zsbu        => gdp%gdr_i_ch%zsbu
    zsbv        => gdp%gdr_i_ch%zsbv
    zssu        => gdp%gdr_i_ch%zssu
    zssv        => gdp%gdr_i_ch%zssv
    ztauet      => gdp%gdr_i_ch%ztauet
    ztauks      => gdp%gdr_i_ch%ztauks
    ztur        => gdp%gdr_i_ch%ztur
    zvicww      => gdp%gdr_i_ch%zvicww
    zvort       => gdp%gdr_i_ch%zvort
    zwl         => gdp%gdr_i_ch%zwl
    zws         => gdp%gdr_i_ch%zws
    rl          => gdp%gdr_i_ch%rl
    p1          => gdp%gdr_i_ch%p1
    hydprs      => gdp%gdr_i_ch%hydprs
    kcs         => gdp%gdr_i_ch%kcs
    kfs         => gdp%gdr_i_ch%kfs
    kfu         => gdp%gdr_i_ch%kfu
    kfv         => gdp%gdr_i_ch%kfv
    mnksrc      => gdp%gdr_i_ch%mnksrc
    kfumin      => gdp%gdr_i_ch%kfumin
    kfvmin      => gdp%gdr_i_ch%kfvmin
    kfsmin      => gdp%gdr_i_ch%kfsmin
    kfumax      => gdp%gdr_i_ch%kfumax
    kfvmax      => gdp%gdr_i_ch%kfvmax
    kfsmax      => gdp%gdr_i_ch%kfsmax
    kfuz1       => gdp%gdr_i_ch%kfuz1
    kfvz1       => gdp%gdr_i_ch%kfvz1
    namdro      => gdp%gdr_i_ch%namdro
    first       => gdp%gdu_ppr%first
    lunus1      => gdp%gdu_ppr%lunus1
    lunus2      => gdp%gdu_ppr%lunus2
    lunus3      => gdp%gdu_ppr%lunus3
    !
    lunus1 = 0
    lunus2 = 0
    lunus3 = 0
    !
    ! Open new unit number for the first time
    ! One may define as many file units as possible. In this example
    ! 3 formatted files are defined
    !
    if (first) then
       if (filus1 /= ' ') then
          lunus1 = newlun(gdp)
          open (lunus1, file = filus1, form = 'formatted')
          write (lundia, *) ' Writing user HIS file: ', filus1
       endif
       if (filus2 /= ' ') then
          lunus2 = newlun(gdp)
          open (lunus2, file = filus2, form = 'formatted')
          write (lundia, *) ' Writing user DRO file: ', filus2
       endif
       if (filus1 /= ' ') then
          lunus3 = newlun(gdp)
          open (lunus3, file = filus3, form = 'formatted')
          write (lundia, *) ' Writing user MAP file: ', filus3, itmapc
       endif
    endif
    if (filus1==' ' .and. filus2==' ' .and. filus3==' ') goto 9999
    !
    ! Calculate date and time YYYYMMDD HHMMSS
    !
    call timdat(julday    ,timsec    ,idate     ,itime     )
    !
    !-If not yet computed (not required) in POSTPR then compute now the
    ! quantities that has been defined along cross-sections:
    ! CTR, ATR, FLTR, DTR (see meaning in 'r-i-ch.inc' file)
    !-Compute flow- & concentration fluxes in defined cross-sections
    ! DO NOT CHANGE THE STRUCTURE OF THE ROUTINE BELOW
    !
    if (.not.ftcros) then
       call tcross(dtsec     ,prshis    ,selhis    ,ntruv     ,ntru      , &
                 & lstsci    ,nmaxus    ,nmax      ,mmax      ,kmax      , &
                 & i(kfu)    ,i(kfv)    ,r(ctr)    ,r(fltr)   , &
                 & r(atr)    ,r(dtr)    ,r(guu)    ,r(gvv)    ,r(guv)    , &
                 & r(gvu)    ,r(thick)  ,r(r1)     ,r(qxk)    ,r(qyk)    , &
                 & r(hu)     ,r(hv)     ,r(dicuv)  ,lsed      ,lsedtot   , &
                 & r(sbtr)   ,r(sstr)   ,r(sbtrc)  ,r(sstrc)  ,r(sbuu)   , &
                 & r(sbvv)   ,r(ssuu)   ,r(ssvv)   ,gdp       )
    endif
    !
    ! If not yet computed (not required) in POSTPR then compute now the
    ! quantities that are defined at stations (at zeta points):
    ! ZWL, ZCURU, ZCURV, ZCURW, ALFAS, GRO, ZTUR, ZTAUKS, ZTAUET,ZVICWW,
    ! ZDICWW, ZRHO, ZQXK, ZQYK (see meaning in 'r-i-ch.inc' file).
    ! DO NOT CHANGE THE STRUCTURE OF THE ROUTINE BELOW
    !
    if (.not.ftstat) then
       call tstat(prshis    ,selhis    ,rhow      ,zmodel    ,nostat    , &
                & nmax      ,mmax      ,kmax      ,lmax      ,lstsci    , &
                & ltur      ,lsal      ,ltem      ,lsed      ,lsedtot   , &
                & i(kfs)    ,i(kfu)    ,i(kfv)    ,i(kcs)    ,i(kfuz1)  , &
                & i(kfvz1)  ,i(kfumin) ,i(kfumax) ,i(kfvmin) ,i(kfvmax) , &
                & i(kfsmin) ,i(kfsmax) ,i(zkfs)   ,r(s1)     ,r(wrkb3)  , &
                & r(wrkb4)  ,r(r1)     ,r(rtur1)  ,r(wphy)   ,r(qxk)    , &
                & r(qyk)    ,r(taubpu) ,r(taubpv) ,r(taubsu) ,r(taubsv) , &
                & r(alfas)  ,r(vicww)  ,r(dicww)  ,r(rich)   ,r(rho)    , &
                & r(rsedeq) ,r(ws)     ,d(dps)    , &
                & r(zwl)    ,r(zalfas) ,r(zcuru)  ,r(zcurv)  ,r(zcurw)  , &
                & r(zqxk)   ,r(zqyk)   ,r(gro)    ,r(ztur)   ,            &
                & r(ztauks) ,r(ztauet) ,r(zvicww) ,r(zdicww) ,r(zrich)  , &
                & r(zrho)   ,r(zbdsed) ,r(zrsdeq) ,r(zdpsed) ,r(zdps)   , &
                & r(zws)    ,r(hydprs) ,r(p1)     ,r(vortic) ,r(enstro) , &
                & r(zvort)  ,r(zenst)  ,r(zsbu)   ,r(zsbv)   ,r(zssu)   , &
                & r(zssv)   ,r(sbuu)   ,r(sbvv)   ,r(ssuu)   ,r(ssvv)   , &
                & r(wrka1)  ,r(wrka2)  ,r(wrka3)  ,r(wrka4)  ,r(wrka5)  , &
                & r(hrms)   ,r(tp)     ,r(teta)   ,r(rlabda) ,r(uorb)   , &
                & wave      ,r(rca)    ,r(zrca)   ,gdp       )
    endif
    !
    ! HIS data (USE LUNUS1)
    ! In this example, history data is written after each time step
    ! for station number 5
    !
    if (nostat>=5 .and. filus1/=' ') then
       notim = itstop - itstrt + 1
       istat = 5
       write (lundia, *) '@@@@@ Writing user HIS file at tstep: ', nst
       !
       ! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       ! @                                              @
       ! @ PROGRAM YOUR OWN HISTORY OUTPUT ROUTINE HERE @
       ! @ worked out example: U_WHIS (see TRM FLOW)    @
       ! @                                              @
       ! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       !
       call u_whis(lunus1    ,header    ,runid     ,itime     ,idate     , &
                 & first     ,kmax      ,lmaxd     ,lstsci    ,ltur      , &
                 & lsal      ,ltem      ,lsed      ,istat     ,icross    , &
                 & ntru      ,ntruv     ,nostat    ,notim     ,zmodel    , &
                 & timmin    ,grdang    ,r(zalfas) ,r(zwl)    ,r(zcuru)  , &
                 & r(zcurv)  ,r(zcurw)  ,r(ztur)   ,r(zqxk)   ,r(zqyk)   , &
                 & r(ztauks) ,r(ztauet) ,r(zvicww) ,r(zdicww) ,r(zrich)  , &
                 & r(zrho)   ,r(zbdsed) ,r(zrsdeq) ,r(zdpsed) ,r(zdps)   , &
                 & r(zws)    ,r(gro)    ,r(hydprs) ,r(atr)    ,r(ctr)    , &
                 & r(dtr)    ,r(fltr)   ,gdp       )
    endif
    !
    ! DRO data (USE LUNUS2)
    ! In this example, DROGUE data for drogue nr. 1 is written after
    ! each time step
    !
    if (ndro>=1 .and. filus2/=' ') then
       notim = itstop - itstrt + 1
       idro = 1
       write (lundia, *) '@@@@@ Writing user DRO file at tstep: ', nst
       !
       ! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       ! @                                              @
       ! @  PROGRAM YOUR OWN DROGUE OUTPUT ROUTINE HERE @
       ! @  worked out example: U_WDRO (see TRM FLOW)   @
       ! @                                              @
       ! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       !
       call u_wdro(lunus2    ,first     ,header    ,runid     ,itime     , &
                 & idate     ,timmin    ,notim     ,idro      ,ndro      , &
                 & r(xydro)  ,ch(namdro))
    endif
    !
    ! MAP data (USE LUNUS3)
    ! In this example, map data (vertical eddy viscosity at layer=ILAY,
    ! water elevation + height at waterlvel points) is written at times
    ! identical to map times (MAKE SURE YOU HAVE MAP FILE !)
    !
    if (nst==itmapc .and. kmax>3 .and. filus3/=' ') then
       ilay = 3
       write (lundia, *) '@@@@@ Writing user MAP file at tstep: ', nst
       !
       ! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       ! @                                              @
       ! @   PROGRAM YOUR OWN MAP OUTPUT ROUTINE HERE   @
       ! @   worked out example: U_WMAP (see TRM FLOW)  @
       ! @                                              @
       ! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       !
       call u_wmap(lunus3    ,header    ,runid     ,itime     ,idate     , &
                 & timmin    ,mmax      ,nmax      ,kmax      ,nmaxus    , &
                 & nsrc      ,ltur      ,lmaxd     ,lstsci    ,lsal      , &
                 & ltem      ,ilay      ,zmodel    ,i(kfu)    ,i(kfv)    , &
                 & i(kcs)    ,i(kfs)    ,i(kfumin) ,i(kfvmin) ,i(kfumax) , &
                 & i(kfvmax) ,i(kfsmin) ,i(kfsmax) ,i(mnksrc) ,r(xz)     , &
                 & r(yz)     ,d(dps)    ,r(rho)    ,r(s1)     ,r(u1)     , &
                 & r(v1)     ,r(w1)     ,r(wphy)   ,r(r1)     ,r(p1)     , &
                 & r(rtur1)  ,r(taubpu) ,r(taubpv) ,r(taubsu) ,r(taubsv) , &
                 & r(vicww)  ,r(dicww)  ,r(vicuv)  ,r(rich)   ,r(umnldf) , &
                 & r(vmnldf) )
    endif
    !
    ! set first to false
    !
    if (first) first = .false.
    !
    ! Close all opened USER files at the end of simulation
    ! in this example all 3 files are closed
    !
 9999 continue
    if (nst == itstop) then
       if (lunus1 /= 0) close (lunus1)
       if (lunus2 /= 0) close (lunus2)
       if (lunus3 /= 0) close (lunus3)
    endif
end subroutine u_ppr
