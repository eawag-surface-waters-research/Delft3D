subroutine wrmfluff(lundia    ,error     ,mmax      ,nmaxus    ,lsed      , &
                  & irequest  ,fds       ,grpnam    ,gdp       )
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
!    Function: Writes the time varying data for the fluff layer
!              to the sediment group on the NEFIS FLOW MAP file
!
! Method used:
!
!!--declarations----------------------------------------------------------------
    use precision
    use sp_buffer
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                 , pointer :: celidt
    type (nefiselement)     , pointer :: nefiselem
    real(fp), dimension(:,:), pointer :: mfluff
!
! Global variables
!
    integer                                                         :: fds
    integer                                                         :: irequest
    integer                                                         :: lsed
    integer                                                         :: lundia
    integer                                                         :: mmax
    integer                                                         :: nmaxus
    logical                                                         :: error
    character(16)                                                   :: grpnam
!
! Local variables
!
    integer                 :: ierror    ! Local errorflag for NEFIS files
    integer                 :: i
    integer                 :: l
    integer                 :: m
    integer                 :: n
    integer                 :: nm
    integer, external       :: neferr
    integer, external       :: putelt
    integer, dimension(3,5) :: uindex
    character(256)          :: errmsg
!
!! executable statements -------------------------------------------------------
!
    if (gdp%gdmorpar%flufflyr%iflufflyr==0) return
    if (lsed == 0) return
    !
    nefiselem => gdp%nefisio%nefiselem(nefiswrsedm)
    celidt  => nefiselem%celidt
    !
    select case (irequest)
    case (1)
       !
       ! Define elements
       !
       call addelm(nefiswrsedm,'MFLUFF',' ','[ KG/M2 ]','REAL',4, &
           & 'Sediment mass in fluff layer (kg/m2)'             , &
           & 3         ,nmaxus ,mmax     ,lsed     ,0       ,0  , &
           & lundia    ,gdp    )
    case (2)
       !
       ! Write data to file
       !
       uindex (1,1) = celidt
       uindex (2,1) = celidt
       uindex (3,1) = 1 ! increment in time
       !
       ! element 'MFLUFF'
       !
       mfluff => gdp%gdmorpar%flufflyr%mfluff
       call sbuff_checksize(lsed*mmax*nmaxus)
       i = 0
       do l = 1, lsed
          do m = 1, mmax
             do n = 1, nmaxus
                i        = i+1
                call n_and_m_to_nm(n, m, nm, gdp)
                sbuff(i) = real(mfluff(l, nm),sp)
             enddo
          enddo
       enddo
       ierror = putelt(fds, grpnam, 'MFLUFF', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       ! write errormessage if error occurred and set error = .true.
       ! the files will be closed in clsnef (called in triend)
       !
 9999  continue
       if (ierror/= 0) then
          ierror = neferr(0, errmsg)
          call prterr(lundia, 'P004', errmsg)
          error = .true.
       endif
    endselect
end subroutine wrmfluff
