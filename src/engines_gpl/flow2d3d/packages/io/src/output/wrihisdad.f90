subroutine wrihisdad(lundia    ,error     ,trifil    ,itdate    , &
                   & tunit     ,dt        ,lsedtot   ,gdp       )
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
! Writes the initial Dredge and Dump group to HIS-DAT
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
    real(fp)      , dimension(:,:) , pointer :: link_percentage
    real(fp)      , dimension(:)   , pointer :: link_distance
    integer                        , pointer :: nadred
    integer                        , pointer :: nadump
    integer                        , pointer :: nasupl
    integer                        , pointer :: nalink
    integer       , dimension(:,:) , pointer :: link_def
    character( 80), dimension(:)   , pointer :: dredge_areas
    character( 80), dimension(:)   , pointer :: dump_areas
    logical                        , pointer :: first
    integer                        , pointer :: celidt
    integer, dimension(:, :)       , pointer :: elmdms
    type (nefiselement)            , pointer :: nefiselem
!
! Local parameters
!
    integer, parameter :: nelmx = 8
!
! Global variables
!
    integer     , intent(in)  :: itdate  !  Description and declaration in exttim.igs
    integer     , intent(in)  :: lsedtot !  Description and declaration in exttim.igs
    integer                   :: lundia  !  Description and declaration in inout.igs
    logical     , intent(out) :: error
    real(fp)    , intent(in)  :: dt      !  Description and declaration in rjdim.f90
    real(fp)    , intent(in)  :: tunit   !  Description and declaration in exttim.igs
    character(*), intent(in)  :: trifil
!
! Local variables
!
    integer                                             :: i      ! Data-group name defined for the NEFIS-files Help var.
    integer                                             :: ierror ! Local errorflag for NEFIS files
    integer       , dimension(2)                        :: ival   ! Local array for writing ITDATE and
    integer       , dimension(nelmx)                    :: nbytsg ! Help array for name constituents and turbulent quantities Array containing the number of by-
    integer                                  , external :: neferr
    real(fp)      , dimension(:), allocatable           :: rdummy ! putgtr expects an array
    logical                                             :: wrswch ! Flag to write file
    character(10) , dimension(nelmx)                    :: elmunt
    character(16)                                       :: grnam
    character(16) , dimension(nelmx)                    :: elmnms
    character(16) , dimension(nelmx)                    :: elmqty
    character(16) , dimension(nelmx)                    :: elmtps
    character(256)                                      :: filnam
    character(256)                                      :: errmsg
    character(64) , dimension(nelmx)                    :: elmdes
!
! Data statements
!
    data grnam/'his-dad-const'/
    data elmnms/'ITDATE', 'TUNIT', 'DT', 'DREDGE_AREAS', 'DUMP_AREAS', 'LINK_DEF', &
             &  'LINK_PERCENTAGES', 'LINK_DISTANCE'/
    data elmqty/8*' '/
    data elmunt/'[YYYYMMDD]', '[   S   ]', 4*'[   -   ]', '[   %   ]', '[   M   ]'/
    data elmtps/'INTEGER', 2*'REAL', 2*'CHARACTER', 'INTEGER', 2*'REAL'/
    data nbytsg/3*4, 2*80, 3*4/
    data (elmdes(i), i = 1, 8)                                                   &
         & /'Initial date (input) & time (default 00:00:00)                ',    &
         & 'Time scale related to seconds                                 ',     &
         & 'Time step (DT*TUNIT sec)                                      ',     &
         & 'Names identifying dredge areas/dredge polygons                ',     &
         & 'Names identifying dump areas/dump polygons                    ',     &
         & 'Actual transports from dredge(1st col) to dump(2nd col) areas ',     &
         & 'Distribution of dredged material from dredge to dump areas    ',     &
         & 'Link Distance between dredge and dump areas                   '/
!
!! executable statements -------------------------------------------------------
!
    link_percentage   => gdp%gddredge%link_percentage
    link_distance     => gdp%gddredge%link_distance
    nadred            => gdp%gddredge%nadred
    nadump            => gdp%gddredge%nadump
    nasupl            => gdp%gddredge%nasupl
    nalink            => gdp%gddredge%nalink
    link_def          => gdp%gddredge%link_def
    dredge_areas      => gdp%gddredge%dredge_areas
    dump_areas        => gdp%gddredge%dump_areas
    nefiselem => gdp%nefisio%nefiselem(nefiswrihisdad)
    first             => nefiselem%first
    celidt            => nefiselem%celidt
    elmdms            => nefiselem%elmdms
    !
    ! Initialize local variables
    !
    ierror = 0
    celidt = 1
    allocate(rdummy(nalink))
    !
    filnam = trifil(1:3) // 'h' // trifil(5:)
    errmsg = ' '
    wrswch = .true.
    !
    ! Set up the element dimensions
    !
    if (first) then
       first = .false.
       call filldm(elmdms    ,1         ,1         ,2         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,2         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,3         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,4         ,1         ,nadred+nasupl    ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,5         ,1         ,nadump    ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,6         ,2         ,nalink    ,2         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,7         ,2         ,nalink    ,lsedtot   , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,8         ,2         ,nalink    ,1         , &
                 & 0         ,0         ,0         )
    endif
    !
    ! element 'ITDATE'
    !
    ival(1) = itdate
    ival(2) = 000000
    call putgti(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(1) ,celidt    ,wrswch    ,ierror    ,ival      )
    if (ierror/=0) goto 999
    !
    ! element 'TUNIT'
    !
    rdummy(1) = tunit
    call putgtr(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(2) ,celidt    ,wrswch    ,ierror    ,rdummy    )
    if (ierror/=0) goto 999
    !
    ! element 'DT'
    !
    rdummy(1) = dt
    call putgtr(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(3) ,celidt    ,wrswch    ,ierror    ,rdummy    )
    if (ierror/=0) goto 999
    !
    ! element 'DREDGE_AREAS'
    !
    call putgtc(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms       , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg       , &
              & elmnms(4) ,celidt    ,wrswch    ,ierror    ,dredge_areas )
    if (ierror/=0) goto 999
    !
    ! element 'DUMP_AREAS'
    !
    call putgtc(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms     , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg     , &
              & elmnms(5) ,celidt    ,wrswch    ,ierror    ,dump_areas )
    if (ierror/=0) goto 999
    !
    ! element 'LINK_DEF'
    !
    call putgti(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(6) ,celidt    ,wrswch    ,ierror    ,link_def  )
    if (ierror/=0) goto 999
    !
    ! element 'LINK_PERCENTAGE'
    !
    call putgtr(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms          , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg          , &
              & elmnms(7) ,celidt    ,wrswch    ,ierror    ,link_percentage )
    if (ierror/=0) goto 999
    !
    ! element 'LINK_DISTANCE'
    !
    call putgtr(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms          , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg          , &
              & elmnms(8) ,celidt    ,wrswch    ,ierror    ,link_distance )
    if (ierror/=0) goto 999
    !
    ! write errormessage if error occurred and set error = .true.
    ! the files will be closed in clsnef (called in triend)
    !
  999 continue
    if (ierror/= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
    deallocate(rdummy)
end subroutine wrihisdad
