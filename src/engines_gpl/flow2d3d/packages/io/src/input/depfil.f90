subroutine depfil(lundia    ,error     ,fildep    ,fmttmp    ,mmax      , &
                & nmax      ,nmaxus    ,dp        ,gdp       )
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
!    Function: Reads the depth values from the attribute file 
! Method used: 
! 
!!--pseudo code and references-------------------------------------------------- 
! NONE 
!!--declarations---------------------------------------------------------------- 
    use precision 
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
! 
! Global variables 
! 
    integer                                                                    :: lundia !  Description and declaration in inout.igs 
    integer                                                      , intent(in)  :: mmax   !  Description and declaration in iidim.f90 
    integer                                                                    :: nmax   !  Description and declaration in iidim.f90 
    integer                                                      , intent(in)  :: nmaxus !  Description and declaration in iidim.f90 
    logical                                                      , intent(out) :: error  !!  Flag=TRUE if an error is encountered 
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: dp     !  Description and declaration in rjdim.f90 
    character(*)                                                               :: fildep !!  Name of the relevant file 
    character(11)                                                , intent(in)  :: fmttmp !!  Help var. for the attribute file 
                                                                                         !!  formats (eg. the thin dams file) 
! 
! Local variables 
! 
    integer, pointer                      :: mfg 
    integer, pointer                      :: mlg 
    integer, pointer                      :: nfg 
    integer, pointer                      :: nlg 
    integer, pointer                      :: mmaxgl 
    integer, pointer                      :: nmaxgl 
    integer                               :: iocond ! Help variable for iostat condition  
    integer                               :: lfile  ! Length of file name  
    integer                               :: luntmp ! Unit number for attribute file  
    integer                               :: m 
    integer                               :: n 
    integer                 , external    :: newlun 
    real(fp), dimension(:,:), allocatable :: dtmp   ! Temporary array containing dp of entire domain 
    logical                 , external    :: exifil 
    character(300)                        :: errmsg ! Character string containing the errormessage to be written to file. The message depends on the error.  
! 
!! executable statements ------------------------------------------------------- 
! 
    mfg    => gdp%gdparall%mfg 
    mlg    => gdp%gdparall%mlg 
    nfg    => gdp%gdparall%nfg 
    nlg    => gdp%gdparall%nlg 
    nmaxgl => gdp%gdparall%nmaxgl 
    mmaxgl => gdp%gdparall%mmaxgl 
    !
    error = .false.
    ! 
    ! Test file existence, if so read 
    ! 
    lfile = len(fildep) 
    if (exifil(fildep(1:lfile), lundia, 'G004', gdp)) then 
       ! 
       ! File exists 
       ! 
       ! 
       ! allocate temporary array to store dp of entire domain read from depth file 
       ! 
       allocate (dtmp(nmaxgl,mmaxgl)) 
       ! 
       ! the master opens and reads the depth file 
       ! 
       if ( inode /= master ) goto 10 
       ! 
       luntmp = newlun(gdp) 
       open (luntmp, file = fildep(1:lfile), form = fmttmp, status = 'old') 
       ! 
       if (fmttmp(1:2) == 'un') then 
          ! 
          ! unformatted file 
          ! read per nmaxus, mmax values in dp array 
          ! NOTE: nmaxus and mmax equal nmaxgl and mmaxgl, respectively (for entire domain) 
          !       in case of parallel runs. Moreover, dp is associated with subdomain and 
          !       therefore, dp for entire domain is stored in temporary array dtmp 
          ! end of error in file = not ok 
          ! 
          do n = 1, nmaxgl 
             read (luntmp, iostat = iocond) (dtmp(n, m), m = 1, mmaxgl) 
             if (iocond /= 0) then 
                if (iocond < 0) then 
                   call prterr(lundia, 'G006', fildep(1:lfile))
                else 
                   call prterr(lundia, 'G007', fildep(1:lfile))
                endif 
                error = .true. 
                goto 9999 
             endif 
             ! 
             ! If a NaN is read -> error
             ! 
             do m = 1, mmax 
                if ( isnan(dtmp(n, m)) ) then  
                    write(errmsg,'(2a)') 'NaN found in file ', fildep(1:lfile) 
                    call prterr(lundia, 'P004', errmsg)
                    error = .true. 
                    goto 9999 
                endif 
            enddo 
          enddo 
       else 
          ! 
          ! Freeformatted file 
          ! Skip lines starting with a '*' 
          ! 
          call skipstarlines(luntmp) 
          ! 
          ! read per nmaxus, mmax values in dp array 
          ! NOTE: nmaxus and mmax equal nmaxgl and mmaxgl, respectively (for entire domain) 
          !       in case of parallel runs. Moreover, dp is associated with subdomain and 
          !       therefore, dp for entire domain is stored in temporary array dtmp 
          ! end of error in file = not ok 
          ! 
          do n = 1, nmaxgl 
             read (luntmp, *, iostat = iocond) (dtmp(n, m), m = 1, mmaxgl) 
             if (iocond /= 0) then 
                if (iocond < 0) then 
                   call prterr(lundia, 'G006', fildep(1:lfile))
                else 
                   call prterr(lundia, 'G007', fildep(1:lfile))
                endif 
                error = .true. 
                goto 9999 
             endif 
             ! 
             ! If a NaN is read -> error
             ! 
             do m = 1, mmax 
                if ( isnan(dtmp(n, m)) ) then  
                    write(errmsg,'(2a)') 'NaN found in file ', fildep(1:lfile) 
                    call prterr(lundia, 'P004', errmsg)
                    error = .true. 
                    goto 9999 
                endif 
            enddo 
          enddo 
       endif 
       ! 
       ! Stop reading file 
       ! 
       9999  continue 
       ! 
       ! Close file 
       ! 
       close (luntmp) 
 10    continue 
       ! 
       ! check whether something went wrong with reading file 
       ! 
       call dfbroadc ( iocond, 1, dfint, gdp ) 
       ! 
       if (iocond /= 0) then 
          if (iocond < 0) then 
             call prterr(lundia    ,'G006'    ,fildep(1:lfile)      )
          else 
             call prterr(lundia    ,'G007'    ,fildep(1:lfile)      )
          endif 
          error = .true. 
       else 
          ! 
          ! scatter array dtmp to all nodes 
          ! 
          call dfbroadc ( dtmp, nmaxgl*mmaxgl, dfloat, gdp ) 
          ! 
          ! put copies of parts of dp for each subdomain 
          ! 
          do m = mfg, mlg 
             do n = nfg, nlg 
                dp(n-nfg+1,m-mfg+1) = dtmp(n,m) 
             enddo 
          enddo 
       endif 
       ! 
       deallocate(dtmp) 
    else 
       ! 
       ! File does not exist 
       ! Exifil has produced a nice error message 
       ! 
       call d3stop(1, gdp) 
    endif 
end subroutine depfil 
