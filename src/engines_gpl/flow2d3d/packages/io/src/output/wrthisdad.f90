subroutine wrthisdad(lundia    ,error     ,trifil    ,ithisc    , &
                   & lsedtot   ,gdp       )
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
! Writes the time varying Dredge and Dump group to the NEFIS HIS-DAT file
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
    real(fp)            , dimension(:,:) , pointer :: link_sum
    real(fp)            , dimension(:,:) , pointer :: voldred
    real(fp)            , dimension(:)   , pointer :: totvoldred
    real(fp)            , dimension(:,:) , pointer :: voldump
    real(fp)            , dimension(:)   , pointer :: totvoldump
    integer                              , pointer :: nadred
    integer                              , pointer :: nadump
    integer                              , pointer :: nasupl
    integer                              , pointer :: nalink
    character(24)                        , pointer :: date_time
    logical                              , pointer :: first
    integer                              , pointer :: celidt
    integer             , dimension(:, :), pointer :: elmdms
    type (nefiselement)                  , pointer :: nefiselem
!
! Local parameters
!
    integer, parameter :: nelmx = 5
!
! Global variables
!
    integer          , intent(in)  :: ithisc
    integer          , intent(in)  :: lsedtot
    integer                        :: lundia  !  Description and declaration in inout.igs
    logical          , intent(out) :: error
    character(*)     , intent(in)  :: trifil
!
! Local variables
!
    integer                                    :: i           ! Data-group name defined for the NEFIS-files group 1 Data-group name defined for the NEFIS-files group 3 Help var.
    integer                                    :: ierror      ! Local errorflag for NEFIS files
    integer       , dimension(1)               :: idummy      ! Help array to read/write Nefis files
    integer       , dimension(nelmx)           :: nbytsg      ! Array containing the number of bytes of each single ELMTPS
    integer                         , external :: neferr
    logical                                    :: wrswch      ! tes of each single ELMTPS Flag to write file
    character(10) , dimension(nelmx)           :: elmunt
    character(16)                              :: grnam
    character(16) , dimension(nelmx)           :: elmnms
    character(16) , dimension(nelmx)           :: elmqty
    character(16) , dimension(nelmx)           :: elmtps
    character(256)                             :: filnam
    character(256)                             :: errmsg
    character(64) , dimension(nelmx)           :: elmdes
    character(24) , dimension(1)               :: datetimearr ! putgtc expects an array
!
! Data statements
!
    data grnam/'his-dad-series'/
    data elmnms/'ITHISC', 'DATE_TIME', 'LINK_SUM', 'DREDGE_VOLUME',   &
         &      'DUMP_VOLUME'/
    data elmqty/5*' '/
    data elmunt/'[   -   ]', '[   -   ]', '[  M3   ]', '[  M3   ]', '[  M3   ]'/
    data elmtps/'INTEGER', 'CHARACTER', 'REAL', 'REAL', 'REAL'/
    data nbytsg/4,24,4,4,4/
    data (elmdes(i), i = 1, 5)                                                    &
         & /'timestep number (ITHISC*DT*TUNIT := time in sec from ITDATE)  ',     &
         &  'Current simulation date and time [YYYY-MM-DD HH:MM:SS.FFFF]   ',     &
         &  'Cumulative dredged material transported via this link         ',     &
         &  'Cumulative dredged material for this dredge area              ',     &
         &  'Cumulative dumped material for this dump area                 '/
!
!! executable statements -------------------------------------------------------
!
    link_sum          => gdp%gddredge%link_sum
    voldred           => gdp%gddredge%voldred
    totvoldred        => gdp%gddredge%totvoldred
    voldump           => gdp%gddredge%voldump
    totvoldump        => gdp%gddredge%totvoldump
    nadred            => gdp%gddredge%nadred
    nadump            => gdp%gddredge%nadump
    nasupl            => gdp%gddredge%nasupl
    nalink            => gdp%gddredge%nalink
    date_time         => gdp%gdinttim%date_time
    nefiselem => gdp%nefisio%nefiselem(nefiswrthisdad)
    first             => nefiselem%first
    celidt            => nefiselem%celidt
    elmdms            => nefiselem%elmdms
    !
    !
    ! Initialize local variables
    !
    filnam = trifil(1:3) // 'h' // trifil(5:)
    !
    wrswch = .false.
    if (first) then
       call getcel(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(1) ,celidt    ,wrswch    ,ierror   )
    endif
    celidt = celidt + 1
    !
    errmsg = ' '
    wrswch = .true.
    !
    ! Set up the element dimensions
    !
    if (first) then
       first = .false.
       call filldm(elmdms    ,1         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,2         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,3         ,2         ,nalink    ,lsedtot   , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,4         ,1         ,nadred+nasupl,0      , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,5         ,1         ,nadump    ,0         , &
                 & 0         ,0         ,0         )
    endif
    !
    ! element 'ITHISC'
    !
    wrswch = .true.
    idummy(1) = ithisc
    call putgti(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(1) ,celidt    ,wrswch    ,ierror   ,idummy    )
    if (ierror/=0) goto 999
    !
    !element 'DATE_TIME'
    !
    datetimearr(1)=date_time
    call putgtc(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(2) ,celidt    ,wrswch    ,ierror   ,datetimearr )
    if (ierror/=0) goto 999
    !
    !element 'LINK_SUM'
    !
    call putgtr(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(3) ,celidt    ,wrswch    ,ierror   ,link_sum  )
    if (ierror/=0) goto 999
    !
    !element 'Dredge volumes'
    !
    call putgtr(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(4) ,celidt    ,wrswch    ,ierror   ,totvoldred)
    if (ierror/=0) goto 999
    !
    !element 'Dump volumes'
    !
    call putgtr(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(5) ,celidt    ,wrswch    ,ierror   ,totvoldump)
    if (ierror/=0) goto 999
    !
    ! write error message if error occurred and set error = .true.
    ! the files will be closed in clsnef (called in triend)
    !
  999 continue
    if (ierror/= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wrthisdad
