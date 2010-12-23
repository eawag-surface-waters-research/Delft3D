subroutine gdp_dealloc(gdp)
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
! NONE
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sp_buffer
    !
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
    integer                                , pointer :: maxpolyp
    real(fp)              , dimension(:)   , pointer :: xpol
    real(fp)              , dimension(:)   , pointer :: ypol
    integer                                , pointer :: lmax
    integer                                , pointer :: lsed
    integer                                , pointer :: lsedtot
    integer                                , pointer :: nofou
    integer                                , pointer :: nveg
    integer               , dimension(:,:) , pointer :: planttype
    real(fp)              , dimension(:,:) , pointer :: nplants
    type (dpm_vegetation) , dimension(:)   , pointer :: vegs
    type (gd_dpmveg)                       , pointer :: gddpmveg
    real(fp)                               , pointer :: cp
    real(fp)                               , pointer :: rhum
    real(fp)                               , pointer :: tair
    real(fp)              , dimension(:)   , pointer :: rhumarr
    real(fp)              , dimension(:)   , pointer :: tairarr
    real(fp)              , dimension(:)   , pointer :: clouarr
    logical                                , pointer :: rhum_file
    logical                                , pointer :: tair_file
    logical                                , pointer :: clou_file
    type (gd_heat)                         , pointer :: gdheat
    integer                                , pointer :: maxpolpoint
    real(fp)              , dimension(:)   , pointer :: x
    real(fp)              , dimension(:)   , pointer :: y
    integer                                , pointer :: m_diff
    integer                                , pointer :: n_diff
    integer                                , pointer :: m_amb
    integer                                , pointer :: n_amb
    real(fp)                               , pointer :: q_diff
    real(fp)                               , pointer :: t0_diff
    real(fp)                               , pointer :: s0_diff
    real(fp)                               , pointer :: rho0_diff
    real(fp)                               , pointer :: d0
    real(fp)                               , pointer :: h0
    real(fp)                               , pointer :: sigma0
    real(fp)                               , pointer :: theta0
    character(256)                         , pointer :: nflmod
    type (gd_nfl)                          , pointer :: gdnfl
    logical                                , pointer :: const
    logical                                , pointer :: culvert
    logical                                , pointer :: dredge
    logical                                , pointer :: drogue
    logical                                , pointer :: struct
    logical                                , pointer :: htur2d
    logical                                , pointer :: zmodel
    logical                                , pointer :: nonhyd
    logical                                , pointer :: lftrto
    logical                                , pointer :: dpmveg
    logical                                , pointer :: bubble
    logical                                , pointer :: nfl
    type (gd_procs)                        , pointer :: gdprocs
    integer              , dimension(:)    , pointer :: nmapp
    integer              , dimension(:)    , pointer :: nmref
    logical                                , pointer :: scour
    real(fp)             , dimension(:)    , pointer :: factor
    real(fp)             , dimension(:)    , pointer :: tauv
    type (gd_scour)                        , pointer :: gdscour
    real(fp)             , dimension(:,:)  , pointer :: zrtcsta
    integer                                , pointer :: rtcmod
    integer              , dimension(:,:)  , pointer :: mnrtcsta
    character(20)        , dimension(:)    , pointer :: namrtcsta
!
! Global variables
!
!
!
! Local variables
!
    integer :: i
    integer :: istat
    integer :: locallsedtot
    integer :: localmaxpolyp
    integer :: localmaxpolpoint
    integer :: localnofou
    logical :: localscour
    logical :: localculvert
    logical :: localdpmveg
    logical :: localdrogue
    logical :: locallftrto
    logical :: localrhum_file
    logical :: localtair_file
    logical :: localclou_file
    logical :: localbubble
!
!! executable statements -------------------------------------------------------
!
    maxpolyp     => gdp%gdcrvout%maxpolyp
    xpol         => gdp%gdcrvout%xpol
    ypol         => gdp%gdcrvout%ypol
    lmax         => gdp%d%lmax
    lsed         => gdp%d%lsed
    lsedtot      => gdp%d%lsedtot
    nofou        => gdp%d%nofou
    nveg         => gdp%gddpmveg%nveg
    planttype    => gdp%gddpmveg%planttype
    nplants      => gdp%gddpmveg%nplants
    vegs         => gdp%gddpmveg%vegs
    gddpmveg     => gdp%gddpmveg
    cp           => gdp%gdheat%cp
    rhum         => gdp%gdheat%rhum
    tair         => gdp%gdheat%tair
    rhumarr      => gdp%gdheat%rhumarr
    tairarr      => gdp%gdheat%tairarr
    clouarr      => gdp%gdheat%clouarr
    rhum_file    => gdp%gdheat%rhum_file
    tair_file    => gdp%gdheat%tair_file
    clou_file    => gdp%gdheat%clou_file
    gdheat       => gdp%gdheat
    maxpolpoint  => gdp%gdipon%maxpolpoint
    x            => gdp%gdipon%x
    y            => gdp%gdipon%y
    m_diff       => gdp%gdnfl%m_diff
    n_diff       => gdp%gdnfl%n_diff
    m_amb        => gdp%gdnfl%m_amb
    n_amb        => gdp%gdnfl%n_amb
    q_diff       => gdp%gdnfl%q_diff
    t0_diff      => gdp%gdnfl%t0_diff
    s0_diff      => gdp%gdnfl%s0_diff
    rho0_diff    => gdp%gdnfl%rho0_diff
    d0           => gdp%gdnfl%d0
    h0           => gdp%gdnfl%h0
    sigma0       => gdp%gdnfl%sigma0
    theta0       => gdp%gdnfl%theta0
    nflmod       => gdp%gdnfl%nflmod
    gdnfl        => gdp%gdnfl
    const        => gdp%gdprocs%const
    culvert      => gdp%gdprocs%culvert
    dredge       => gdp%gdprocs%dredge
    drogue       => gdp%gdprocs%drogue
    struct       => gdp%gdprocs%struct
    htur2d       => gdp%gdprocs%htur2d
    zmodel       => gdp%gdprocs%zmodel
    nonhyd       => gdp%gdprocs%nonhyd
    lftrto       => gdp%gdprocs%lftrto
    dpmveg       => gdp%gdprocs%dpmveg
    bubble       => gdp%gdprocs%bubble
    nfl          => gdp%gdprocs%nfl
    gdprocs      => gdp%gdprocs
    nmapp        => gdp%gdscour%nmapp
    nmref        => gdp%gdscour%nmref
    scour        => gdp%gdscour%scour
    factor       => gdp%gdscour%factor
    tauv         => gdp%gdscour%tauv
    gdscour      => gdp%gdscour
    zrtcsta      => gdp%gdrtc%zrtcsta
    rtcmod       => gdp%gdrtc%rtcmod
    mnrtcsta     => gdp%gdrtc%mnrtcsta
    namrtcsta    => gdp%gdrtc%namrtcsta
    !
    ! copy 
    !    lsed 
    !    maxpolyp 
    !    dredge 
    !    scour
    !    lftrto
    !    etc.
    ! to local parameters, because they will be deallocated
    ! in the gdp structure, before they are used to deallocate their related structures
    !
    locallsedtot     = lsedtot
    localscour       = scour
    localculvert     = culvert
    localmaxpolyp    = maxpolyp
    localmaxpolpoint = maxpolpoint
    localdpmveg      = dpmveg
    localdrogue      = drogue
    locallftrto      = lftrto
    localrhum_file   = rhum_file
    localtair_file   = tair_file
    localclou_file   = clou_file
    localbubble      = bubble
    localnofou       = nofou
    !
    call nefisio_dealloc(gdp       )
    !
    call clradv2d(gdp)
    deallocate (gdp%gdadv2d)
    deallocate (gdp%gdaddress)
    deallocate (gdp%gdautok)
    if (localbubble) then
       if (associated(gdp%gdbubble%cpdis)) deallocate (gdp%gdbubble%cpdis, STAT = istat)
       if (associated(gdp%gdbubble%hsink)) deallocate (gdp%gdbubble%hsink, STAT = istat)
       if (associated(gdp%gdbubble%hsour)) deallocate (gdp%gdbubble%hsour, STAT = istat)
       if (associated(gdp%gdbubble%xlbub)) deallocate (gdp%gdbubble%xlbub, STAT = istat)
       if (associated(gdp%gdbubble%zbubl)) deallocate (gdp%gdbubble%zbubl, STAT = istat)
       if (associated(gdp%gdbubble%zvelo)) deallocate (gdp%gdbubble%zvelo, STAT = istat)
       if (associated(gdp%gdbubble%flbub)) deallocate (gdp%gdbubble%flbub, STAT = istat)
    endif
    deallocate (gdp%gdconst)
    deallocate (gdp%gdconstd)
    deallocate (gdp%gdcoup)
    deallocate (gdp%gddatusr)
    deallocate (gdp%gddiagno)
    deallocate (gdp%gddischarge)
    if (associated(gdp%gddischarge%capacity)) deallocate (gdp%gddischarge%capacity, STAT = istat)
    deallocate (gdp%d)
    if (localdpmveg) then
       do i=1,nveg
          if (associated(gdp%gddpmveg%vegs(i)%dia   )) deallocate (gdp%gddpmveg%vegs(i)%dia   , STAT = istat)
          if (associated(gdp%gddpmveg%vegs(i)%nstem )) deallocate (gdp%gddpmveg%vegs(i)%nstem , STAT = istat)
          if (associated(gdp%gddpmveg%vegs(i)%cdcoef)) deallocate (gdp%gddpmveg%vegs(i)%cdcoef, STAT = istat)
          if (associated(gdp%gddpmveg%vegs(i)%rho   )) deallocate (gdp%gddpmveg%vegs(i)%rho   , STAT = istat)
          if (associated(gdp%gddpmveg%vegs(i)%z     )) deallocate (gdp%gddpmveg%vegs(i)%z     , STAT = istat)
       enddo
       if (associated(gdp%gddpmveg%vegs     )) deallocate (gdp%gddpmveg%vegs     , STAT = istat)
       if (associated(gdp%gddpmveg%planttype)) deallocate (gdp%gddpmveg%planttype, STAT = istat)
       if (associated(gdp%gddpmveg%nplants  )) deallocate (gdp%gddpmveg%nplants  , STAT = istat)
    endif
    deallocate (gdp%gddpmveg)
    deallocate (gdp%gdexttim)
    deallocate (gdp%gdfmtbcc)
    deallocate (gdp%gdfmtbct)
    deallocate (gdp%gdfmtdis)
    if (localnofou > 0) then
       if (associated(gdp%gdfourier%fconno )) deallocate (gdp%gdfourier%fconno   , STAT = istat)
       if (associated(gdp%gdfourier%flayno )) deallocate (gdp%gdfourier%flayno   , STAT = istat)
       if (associated(gdp%gdfourier%fnumcy )) deallocate (gdp%gdfourier%fnumcy   , STAT = istat)
       if (associated(gdp%gdfourier%ftmsto )) deallocate (gdp%gdfourier%ftmsto   , STAT = istat)
       if (associated(gdp%gdfourier%ftmstr )) deallocate (gdp%gdfourier%ftmstr   , STAT = istat)
       if (associated(gdp%gdfourier%ifoupt )) deallocate (gdp%gdfourier%ifoupt   , STAT = istat)
       if (associated(gdp%gdfourier%iofset )) deallocate (gdp%gdfourier%iofset   , STAT = istat)
       if (associated(gdp%gdfourier%fknfac )) deallocate (gdp%gdfourier%fknfac   , STAT = istat)
       if (associated(gdp%gdfourier%foucomp)) deallocate (gdp%gdfourier%foucomp  , STAT = istat)
       if (associated(gdp%gdfourier%foufas )) deallocate (gdp%gdfourier%foufas   , STAT = istat)
       if (associated(gdp%gdfourier%fousma )) deallocate (gdp%gdfourier%fousma   , STAT = istat)
       if (associated(gdp%gdfourier%fousmb )) deallocate (gdp%gdfourier%fousmb   , STAT = istat)
       if (associated(gdp%gdfourier%fouvec )) deallocate (gdp%gdfourier%fouvec   , STAT = istat)
       if (associated(gdp%gdfourier%fv0pu  )) deallocate (gdp%gdfourier%fv0pu    , STAT = istat)
       if (associated(gdp%gdfourier%fouelp )) deallocate (gdp%gdfourier%fouelp   , STAT = istat)
       if (associated(gdp%gdfourier%founam )) deallocate (gdp%gdfourier%founam   , STAT = istat)
       if (associated(gdp%gdfourier%foutyp )) deallocate (gdp%gdfourier%foutyp   , STAT = istat)
    endif
    deallocate (gdp%gdfourier)
    if (associated(gdp%gdheat%secchi)) deallocate (gdp%gdheat%secchi, STAT = istat)
    if (localrhum_file) then
       if (associated(gdp%gdheat%rhumarr)) deallocate (gdp%gdheat%rhumarr, STAT = istat)
    endif
    if (localtair_file) then
       if (associated(gdp%gdheat%tairarr)) deallocate (gdp%gdheat%tairarr, STAT = istat)
    endif
    if (localclou_file) then
       if (associated(gdp%gdheat%clouarr)) deallocate (gdp%gdheat%clouarr, STAT = istat)
    endif
    deallocate (gdp%gdheat)
    deallocate (gdp%gdhtur2d)
    deallocate (gdp%gdhwid)
    deallocate (gdp%gdinout)
    deallocate (gdp%gdinttim)
    deallocate (gdp%gdiwearr)
    deallocate (gdp%gdiwepar)
    deallocate (gdp%gdkeywtd)
    deallocate (gdp%gdluntmp)
    deallocate (gdp%gdmudcoe)
    deallocate (gdp%gdnfl)
    deallocate (gdp%gdnumeco)
    deallocate (gdp%gdphysco)
    deallocate (gdp%gdpointrs)
    deallocate (gdp%gdprocs)
    deallocate (gdp%gdprognm)
    deallocate (gdp%gdr_i_ch)
    deallocate (gdp%gdrdpara)
    deallocate (gdp%gdrivpro)
    deallocate (gdp%gdsobek)
    deallocate (gdp%gdstations)
    deallocate (gdp%gdtfzeta)
    deallocate (gdp%gdtmpfil)
    if (locallftrto) then
       call clrtrachy(istat, gdp)
    endif
    deallocate (gdp%gdtrachy)
    deallocate (gdp%gdturcoe)
    deallocate (gdp%gdusrpar)
    deallocate (gdp%gdzmodel)
    deallocate (gdp%gdnonhyd)
    !
    if (localscour) then
       if (associated(gdp%gdavalan%depchange)) deallocate (gdp%gdavalan%depchange, STAT = istat)
    endif
    call clrbedformpar(istat, gdp)
    deallocate (gdp%gdbedformpar)
    deallocate (gdp%gdavalan)
    deallocate (gdp%gdbetaro)
    if (associated(gdp%gdbcdat%pindex))    deallocate (gdp%gdbcdat%pindex   , STAT = istat)
    if (associated(gdp%gdbcdat%bct_order)) deallocate (gdp%gdbcdat%bct_order, STAT = istat)
    if (associated(gdp%gdbcdat%ext_bnd))   deallocate (gdp%gdbcdat%ext_bnd  , STAT = istat)
    if (associated(gdp%gdbcdat%compnames)) deallocate (gdp%gdbcdat%compnames, STAT = istat)
    if (associated(gdp%gdbcdat%hydrbcf))   deallocate (gdp%gdbcdat%hydrbcf  , STAT = istat)
    deallocate (gdp%gdbcdat)
    if (localdrogue) then
        if (associated(gdp%gdcline%inc))  deallocate (gdp%gdcline%inc , STAT = istat)
        if (associated(gdp%gdcline%ud))   deallocate (gdp%gdcline%ud  , STAT = istat)
        if (associated(gdp%gdcline%xd))   deallocate (gdp%gdcline%xd  , STAT = istat)
        if (associated(gdp%gdcline%rdep)) deallocate (gdp%gdcline%rdep, STAT = istat)
    endif
    deallocate (gdp%gdcline)
    if (localculvert) then
        if (associated(gdp%gdculver%arcul))   deallocate (gdp%gdculver%arcul  , STAT = istat)
        if (associated(gdp%gdculver%calfa))   deallocate (gdp%gdculver%calfa  , STAT = istat)
        if (associated(gdp%gdculver%clcul))   deallocate (gdp%gdculver%clcul  , STAT = istat)
        if (associated(gdp%gdculver%cleng))   deallocate (gdp%gdculver%cleng  , STAT = istat)
        if (associated(gdp%gdculver%closs1))  deallocate (gdp%gdculver%closs1 , STAT = istat)
        if (associated(gdp%gdculver%closs2))  deallocate (gdp%gdculver%closs2 , STAT = istat)
        if (associated(gdp%gdculver%closs3))  deallocate (gdp%gdculver%closs3 , STAT = istat)
        if (associated(gdp%gdculver%cmann))   deallocate (gdp%gdculver%cmann  , STAT = istat)
        if (associated(gdp%gdculver%htcul))   deallocate (gdp%gdculver%htcul  , STAT = istat)
        if (associated(gdp%gdculver%numrel1)) deallocate (gdp%gdculver%numrel1, STAT = istat)
        if (associated(gdp%gdculver%numrel2)) deallocate (gdp%gdculver%numrel2, STAT = istat)
        if (associated(gdp%gdculver%numrel3)) deallocate (gdp%gdculver%numrel3, STAT = istat)
        if (associated(gdp%gdculver%poscul))  deallocate (gdp%gdculver%poscul , STAT = istat)
        if (associated(gdp%gdculver%wetar1))  deallocate (gdp%gdculver%wetar1 , STAT = istat)
        if (associated(gdp%gdculver%wetar2))  deallocate (gdp%gdculver%wetar2 , STAT = istat)
        if (associated(gdp%gdculver%wetar3))  deallocate (gdp%gdculver%wetar3 , STAT = istat)
        if (associated(gdp%gdculver%wtcul))   deallocate (gdp%gdculver%wtcul  , STAT = istat)
        if (associated(gdp%gdculver%dll_name    )) deallocate (gdp%gdculver%dll_name    , STAT = istat)
        if (associated(gdp%gdculver%dll_function)) deallocate (gdp%gdculver%dll_function, STAT = istat)
        if (associated(gdp%gdculver%dll_handle  )) deallocate (gdp%gdculver%dll_handle  , STAT = istat)
    endif
    deallocate (gdp%gdculver)
    deallocate (gdp%gddefsub)
    call clrflwpar(istat, gdp)
    deallocate (gdp%gdflwpar)
    if (locallsedtot>0) then
       call clreqtran(istat, gdp)
       call clrerosed(istat, gdp)
       call clrsedpar(istat, gdp)
       call clrmorpar(istat, gdp)
       call clrmorlyr(istat, gdp)
       call clrdredge(istat, gdp)
    endif
    deallocate (gdp%gddredge)
    deallocate (gdp%gdeqtran)
    deallocate (gdp%gderosed)
    deallocate (gdp%gdsedpar)
    deallocate (gdp%gdmorpar)
    deallocate (gdp%gdmorlyr)
    deallocate (gdp%gdf0isf1)
    deallocate (gdp%gdincbc)
    deallocate (gdp%gdincbcc)
    deallocate (gdp%gdinibcc)
    deallocate (gdp%gdinibct)
    deallocate (gdp%gdinidis)
    if (localmaxpolpoint>0) then
       if (associated(gdp%gdipon%x)) deallocate (gdp%gdipon%x, STAT = istat)
       if (associated(gdp%gdipon%y)) deallocate (gdp%gdipon%y, STAT = istat)
    endif
    deallocate (gdp%gdipon)
    deallocate (gdp%gdpostpr)
    deallocate (gdp%gdrestart)
    if (rtcmod == dataFromFLOWToRTC) then
       if (associated(gdp%gdrtc%mnrtcsta))   deallocate (gdp%gdrtc%mnrtcsta  , STAT = istat)
       if (associated(gdp%gdrtc%namrtcsta))  deallocate (gdp%gdrtc%namrtcsta , STAT = istat)
       if (associated(gdp%gdrtc%zrtcsta))    deallocate (gdp%gdrtc%zrtcsta   , STAT = istat)
    endif
    deallocate (gdp%gdrtc)
    if (localscour) then
       if (associated(gdp%gdscour%nmapp))  deallocate (gdp%gdscour%nmapp , STAT = istat)
       if (associated(gdp%gdscour%nmref))  deallocate (gdp%gdscour%nmref , STAT = istat)
       if (associated(gdp%gdscour%factor)) deallocate (gdp%gdscour%factor, STAT = istat)
       if (associated(gdp%gdscour%tauv))   deallocate (gdp%gdscour%tauv  , STAT = istat)
    endif
    deallocate (gdp%gdscour)
    deallocate (gdp%gdshearx)
    deallocate (gdp%gdsnel)
    deallocate (gdp%gdtimers)
    deallocate (gdp%gdtricom)
    deallocate (gdp%gdtrisol)
    deallocate (gdp%gdu_ppr)
    deallocate (gdp%gdupdbcc)
    deallocate (gdp%gdupdbct)
    deallocate (gdp%gdupddis)
    call clrwaqpar(istat, gdp)
    deallocate (gdp%gdwaqpar)
    deallocate (gdp%gdwrirst)
    deallocate (gdp%gdwrline)
    deallocate (gdp%gdz_initcg)
    !
    ! Delft3D-MOR
    !
    if (localmaxpolyp>0) then
       if (associated(gdp%gdcrvout%xpol)) deallocate (gdp%gdcrvout%xpol, STAT = istat)
       if (associated(gdp%gdcrvout%ypol)) deallocate (gdp%gdcrvout%ypol, STAT = istat)
    endif
    deallocate (gdp%gdcrvout)
    !
    deallocate (gdp%dd)
    !
    deallocate (gdp%runid)
    !
    if (parll) then
       if (associated(gdp%gdparall%iblkad)) deallocate (gdp%gdparall%iblkad, STAT = istat)
       if (associated(gdp%gdparall%iweig )) deallocate (gdp%gdparall%iweig , STAT = istat)
    endif
    deallocate (gdp%gdparall)
    !
    deallocate (gdp%arch)
    deallocate (gdp%errorcode)
    !
    call sbuff_dealloc
end subroutine gdp_dealloc
