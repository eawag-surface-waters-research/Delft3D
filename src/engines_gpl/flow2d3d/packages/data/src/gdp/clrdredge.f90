subroutine clrdredge(istat     ,gdp       )
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use flow_tables
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
    type (handletype)              , pointer :: tseriesfile
    real(fp)      , dimension(:,:) , pointer :: link_percentage
    real(fp)      , dimension(:)   , pointer :: link_distance
    real(fp)      , dimension(:,:) , pointer :: link_sum
    real(fp)      , dimension(:)   , pointer :: dzdred
    real(fp)      , dimension(:)   , pointer :: refplane
    real(fp)      , dimension(:,:) , pointer :: voldred
    real(fp)      , dimension(:)   , pointer :: voldune
    real(fp)      , dimension(:)   , pointer :: totvoldred
    real(fp)      , dimension(:)   , pointer :: globalareadred
    real(fp)      , dimension(:,:) , pointer :: voldump
    real(fp)      , dimension(:,:) , pointer :: percsupl
    real(fp)      , dimension(:)   , pointer :: totvoldump
    real(fp)      , dimension(:)   , pointer :: localareadump
    real(fp)      , dimension(:)   , pointer :: globalareadump
    real(fp)      , dimension(:)   , pointer :: globaldumpcap
    integer                        , pointer :: nadred
    integer                        , pointer :: nadump
    integer       , dimension(:,:) , pointer :: link_def
    character( 80), dimension(:)   , pointer :: dredge_areas
    character( 80), dimension(:)   , pointer :: dump_areas
    type (dredtype), dimension(:)  , pointer :: dredge_prop
    type (dumptype), dimension(:)  , pointer :: dump_prop
!
! Global variables
!
    integer,intent(out) :: istat
!
! Local variables
!
    integer                 :: i
    type(dredtype), pointer :: pdredge
    type(dumptype), pointer :: pdump
!
!! executable statements -------------------------------------------------------
!
    tseriesfile       => gdp%gddredge%tseriesfile
    link_percentage   => gdp%gddredge%link_percentage
    link_distance     => gdp%gddredge%link_distance
    link_sum          => gdp%gddredge%link_sum
    dzdred            => gdp%gddredge%dzdred
    refplane          => gdp%gddredge%refplane
    voldred           => gdp%gddredge%voldred
    voldune           => gdp%gddredge%voldune
    totvoldred        => gdp%gddredge%totvoldred
    globalareadred    => gdp%gddredge%globalareadred
    voldump           => gdp%gddredge%voldump
    percsupl          => gdp%gddredge%percsupl
    totvoldump        => gdp%gddredge%totvoldump
    localareadump     => gdp%gddredge%localareadump
    globalareadump    => gdp%gddredge%globalareadump
    globaldumpcap     => gdp%gddredge%globaldumpcap
    nadred            => gdp%gddredge%nadred
    nadump            => gdp%gddredge%nadump
    link_def          => gdp%gddredge%link_def
    dredge_areas      => gdp%gddredge%dredge_areas
    dump_areas        => gdp%gddredge%dump_areas
    dredge_prop       => gdp%gddredge%dredge_prop
    dump_prop         => gdp%gddredge%dump_prop
    !
    if (associated(gdp%gddredge%link_percentage)) deallocate (gdp%gddredge%link_percentage, STAT = istat)
    if (associated(gdp%gddredge%link_distance))   deallocate (gdp%gddredge%link_distance  , STAT = istat)
    if (associated(gdp%gddredge%link_sum))        deallocate (gdp%gddredge%link_sum       , STAT = istat)
    if (associated(gdp%gddredge%dzdred))          deallocate (gdp%gddredge%dzdred         , STAT = istat)
    if (associated(gdp%gddredge%refplane))        deallocate (gdp%gddredge%refplane       , STAT = istat)
    if (associated(gdp%gddredge%voldred))         deallocate (gdp%gddredge%voldred        , STAT = istat)
    if (associated(gdp%gddredge%totvoldred))      deallocate (gdp%gddredge%totvoldred     , STAT = istat)
    if (associated(gdp%gddredge%globalareadred))  deallocate (gdp%gddredge%globalareadred , STAT = istat)
    if (associated(gdp%gddredge%voldune))         deallocate (gdp%gddredge%voldune        , STAT = istat)
    if (associated(gdp%gddredge%percsupl))        deallocate (gdp%gddredge%percsupl       , STAT = istat)
    if (associated(gdp%gddredge%totvoldump))      deallocate (gdp%gddredge%totvoldump     , STAT = istat)
    if (associated(gdp%gddredge%localareadump))   deallocate (gdp%gddredge%localareadump  , STAT = istat)
    if (associated(gdp%gddredge%globalareadump))  deallocate (gdp%gddredge%globalareadump , STAT = istat)
    if (associated(gdp%gddredge%globaldumpcap))   deallocate (gdp%gddredge%globaldumpcap  , STAT = istat)
    if (associated(gdp%gddredge%voldump))         deallocate (gdp%gddredge%voldump        , STAT = istat)
    !
    if (associated(gdp%gddredge%link_def))        deallocate (gdp%gddredge%link_def       , STAT = istat)
    !
    if (associated(gdp%gddredge%dredge_areas))    deallocate (gdp%gddredge%dredge_areas   , STAT = istat)
    if (associated(gdp%gddredge%dump_areas))      deallocate (gdp%gddredge%dump_areas     , STAT = istat)
    !
    if (associated(gdp%gddredge%dredge_prop)) then
       do i = 1, nadred
          pdredge => gdp%gddredge%dredge_prop(i)
          if (associated(pdredge%nm))             deallocate (pdredge%nm                  , STAT = istat)
          if (associated(pdredge%inm))            deallocate (pdredge%inm                 , STAT = istat)
          if (associated(pdredge%area))           deallocate (pdredge%area                , STAT = istat)
          if (associated(pdredge%hdune))          deallocate (pdredge%hdune               , STAT = istat)
          if (associated(pdredge%dz_dredge))      deallocate (pdredge%dz_dredge           , STAT = istat)
          if (associated(pdredge%dunetoplevel))   deallocate (pdredge%dunetoplevel        , STAT = istat)
          if (associated(pdredge%triggerlevel))   deallocate (pdredge%triggerlevel        , STAT = istat)
          if (associated(pdredge%bedlevel))       deallocate (pdredge%bedlevel            , STAT = istat)
          if (associated(pdredge%troughlevel))    deallocate (pdredge%troughlevel         , STAT = istat)
          if (associated(pdredge%sedimentdepth))  deallocate (pdredge%sedimentdepth       , STAT = istat)
          if (associated(pdredge%sortvar))        deallocate (pdredge%sortvar             , STAT = istat)
          if (associated(pdredge%triggered))      deallocate (pdredge%triggered           , STAT = istat)
       enddo
       deallocate (gdp%gddredge%dredge_prop    , STAT = istat)
    endif
    !
    if (associated(gdp%gddredge%dump_prop)) then
       do i = 1, nadump
          pdump => gdp%gddredge%dump_prop(i)
          if (associated(pdump%nm))               deallocate (pdump%nm                    , STAT = istat)
          if (associated(pdump%inm))              deallocate (pdump%inm                   , STAT = istat)
          if (associated(pdump%area))             deallocate (pdump%area                  , STAT = istat)
          if (associated(pdump%hdune))            deallocate (pdump%hdune                 , STAT = istat)
          if (associated(pdump%bedlevel))         deallocate (pdump%bedlevel              , STAT = istat)
          if (associated(pdump%dz_dump))          deallocate (pdump%dz_dump               , STAT = istat)
          if (associated(pdump%sortvar))          deallocate (pdump%sortvar               , STAT = istat)
       enddo
       deallocate (gdp%gddredge%dump_prop      , STAT = istat)
    endif
    !
    call cleartable(gdp%gddredge%tseriesfile)
end subroutine clrdredge
