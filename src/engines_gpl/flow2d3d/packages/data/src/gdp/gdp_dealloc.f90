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
!  $Id$
!  $HeadURL$
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
    use bedcomposition_module
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
!
!
! Local variables
!
    integer :: i
    integer :: istat
    !
    integer :: localmaxpolyp
    integer :: locallsedtot
    integer :: localnofou
    logical :: localrhum_file
    logical :: localtair_file
    logical :: localclou_file
    logical :: localculvert
    logical :: localdrogue
    logical :: locallftrto
    logical :: localdpmveg
    logical :: localbubble
    logical :: localscour
!
!! executable statements -------------------------------------------------------
!
    !
    ! copy 
    !    lsedtot
    !    maxpolyp 
    !    scour
    !    lftrto
    !    etc.
    ! to local parameters, because their gdp entries will be deallocated
    ! before they are used to deallocate their related structures
    !
    localmaxpolyp    = gdp%gdcrvout%maxpolyp
    locallsedtot     = gdp%d%lsedtot
    localnofou       = gdp%d%nofou
    localrhum_file   = gdp%gdheat%rhum_file
    localtair_file   = gdp%gdheat%tair_file
    localclou_file   = gdp%gdheat%clou_file
    localculvert     = gdp%gdprocs%culvert
    localdrogue      = gdp%gdprocs%drogue
    locallftrto      = gdp%gdprocs%lftrto
    localdpmveg      = gdp%gdprocs%dpmveg
    localbubble      = gdp%gdprocs%bubble
    localscour       = gdp%gdscour%scour
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
       do i=1,gdp%gddpmveg%nveg
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
    call clrbedformpar(istat, gdp)
    deallocate (gdp%gdbedformpar)
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
       istat = clrmorlyr(gdp%gdmorlyr)
       call clrdredge(istat, gdp)
    endif
    deallocate (gdp%gddredge)
    deallocate (gdp%gdeqtran)
    deallocate (gdp%gderosed)
    deallocate (gdp%gdsedpar)
    deallocate (gdp%gdmorpar)
    call clrmassbal(istat, gdp)
    deallocate (gdp%gdmassbal)
    deallocate (gdp%gdf0isf1)
    deallocate (gdp%gdincbc)
    deallocate (gdp%gdincbcc)
    deallocate (gdp%gdinibcc)
    deallocate (gdp%gdinibct)
    deallocate (gdp%gdinidis)
    deallocate (gdp%gdpostpr)
    deallocate (gdp%gdrestart)
    if (gdp%gdrtc%rtcmod == dataFromFLOWToRTC) then
       if (associated(gdp%gdrtc%mnrtcsta))   deallocate (gdp%gdrtc%mnrtcsta  , STAT = istat)
       if (associated(gdp%gdrtc%namrtcsta))  deallocate (gdp%gdrtc%namrtcsta , STAT = istat)
       if (associated(gdp%gdrtc%zrtcsta))    deallocate (gdp%gdrtc%zrtcsta   , STAT = istat)
    endif
    deallocate (gdp%gdrtc)
    if (localscour) then
       if (associated(gdp%gdscour%nmapp))     deallocate (gdp%gdscour%nmapp , STAT = istat)
       if (associated(gdp%gdscour%nmref))     deallocate (gdp%gdscour%nmref , STAT = istat)
       if (associated(gdp%gdscour%factor))    deallocate (gdp%gdscour%factor, STAT = istat)
       if (associated(gdp%gdscour%tauv))      deallocate (gdp%gdscour%tauv  , STAT = istat)
       if (associated(gdp%gdscour%depchange)) deallocate (gdp%gdscour%depchange, STAT = istat)
    endif
    deallocate (gdp%gdscour)
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
