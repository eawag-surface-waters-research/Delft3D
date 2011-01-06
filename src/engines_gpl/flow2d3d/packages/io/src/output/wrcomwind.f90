subroutine wrcomwind(error     ,comfil    ,itcur    ,itimc     , &
                   & mmax      ,nmaxus    , &
                   & windu     ,windv     ,gdp       )
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
    use sp_buffer
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical              , pointer :: first
    integer              , pointer :: celidt
    type (nefiselement)  , pointer :: nefiselem
    integer              , pointer :: lundia
!
! Global variables
!
    integer                                                      , intent(in)  :: itcur  ! Current time counter for the COM file
    integer                                                      , intent(in)  :: itimc  ! Current time step counter
    integer                                                                    :: mmax   ! Description and declaration in iidim.f90
    integer                                                                    :: nmaxus ! Description and declaration in iidim.f90
    logical                                                      , intent(out) :: error  ! Flag=TRUE if an error is encountered
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: windu  ! Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: windv  ! Description and declaration in rjdim.f90
    character(60)                                                , intent(in)  :: comfil ! COM file name
!
! Local variables
!
    integer                 :: ierror       ! Local errorflag for NEFIS files 
    integer                 :: fds
    integer                 :: i
    integer                 :: m            ! Help var. 
    integer                 :: n            ! Help var. 
    integer, dimension(1)   :: idummy       ! Help array to read/write Nefis files 
    integer, dimension(3,5) :: uindex
    integer, external       :: putelt
    integer, external       :: clsnef
    integer, external       :: open_datdef
    integer, external       :: neferr
    character(16)           :: grnam
    character(256)          :: errmsg       ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(60)           :: filnam       ! Help var. for FLOW file name 
    character(1024)         :: error_string
!
! Data statements
!
    data grnam/'WIND'/
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefiswrcomwind)
    first   => nefiselem%first
    celidt  => nefiselem%celidt
    lundia  => gdp%gdinout%lundia
    !
    ! Initialize local variables
    !
    filnam = comfil
    errmsg = ' '
    !
    ! initialize group index time dependent data
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time
    !
    if (first) then
       call addelm(nefiswrcomwind,'TIMCUR',' ','[ TSCALE]','INTEGER',4    , &
          & 'Time of current field rel.to reference date/time              ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrcomwind,'WINDU',' ','[  M/S  ]','REAL',4    , &
          & 'Wind-velocity in zeta-point in x-direction at end of time interval', &
          & 2         ,nmaxus    ,mmax      ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrcomwind,'WINDV',' ','[  M/S  ]','REAL',4    , &
          & 'Wind-velocity in zeta-point in y-direction at end of time interval', &
          & 2         ,nmaxus    ,mmax      ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call defnewgrp(nefiswrcomwind, filnam, grnam, gdp)
       !
       ! Get start celidt for writing
       !
       nefiselem => gdp%nefisio%nefiselem(nefiswrcomwind)
    first   => nefiselem%first
    celidt  => nefiselem%celidt
       first = .false.
    endif
    !
    ierror = open_datdef(filnam, fds)
    if (ierror/= 0) goto 9999
    !
    ! Writing of output on every itcur
    !
    celidt = itcur
    uindex(1,1) = celidt
    uindex(2,1) = celidt
    !
    ! TIMCUR
    !
    idummy(1)   = itimc
    ierror     = putelt(fds, grnam, 'TIMCUR', uindex, 1, idummy)
    if (ierror/=0) goto 9999
    !
    ! WINDU
    !
    call sbuff_checksize(mmax*nmaxus)
    i = 0
    do m = 1, mmax
       do n = 1, nmaxus
          i        = i+1
          sbuff(i) = real(windu(n, m),sp)
       enddo
    enddo
    ierror = putelt(fds, grnam, 'WINDU', uindex, 1, sbuff)
    if (ierror/=0) goto 9999
    !
    ! WINDV
    !
    call sbuff_checksize(mmax*nmaxus)
    i = 0
    do m = 1, mmax
       do n = 1, nmaxus
          i        = i+1
          sbuff(i) = real(windv(n, m),sp)
       enddo
    enddo
    ierror = putelt(fds, grnam, 'WINDV', uindex, 1, sbuff)
    if (ierror/=0) goto 9999
    !
    ierror = clsnef(fds)
    !
    ! write errormessage if error occurred and set error = .true.
    ! the files will be closed in clsnef (called in triend)
    !
 9999 continue
    if (ierror/= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wrcomwind
