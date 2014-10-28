subroutine wrhfluff(lundia    ,error     ,nostat    ,nostatto  ,nostatgl  ,lsed      , &
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
    use dfparall, only: inode, master, parll
    use dffunctionals, only: dfgather_filter
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                             , pointer :: celidt
    type (nefiselement)                 , pointer :: nefiselem
    real(fp)         , dimension(:,:)   , pointer :: mfluff
    integer          , dimension(:,:)   , pointer :: mnstat
    integer          , dimension(:)     , pointer :: order_sta
!
! Global variables
!
    integer         , intent(in)  :: fds         ! Nefis file pointer to write to
    integer         , intent(in)  :: irequest    ! 1: define elements, 2: write data
    integer         , intent(in)  :: lsed        ! Number of sediment constituents
    integer         , intent(in)  :: lundia      ! File pointer to diagnosis file
    integer         , intent(in)  :: nostat      ! local  number of stations
    integer         , intent(in)  :: nostatgl    ! global number of stations (i.e. original number excluding duplicate stations located in the halo regions)
    integer         , intent(in)  :: nostatto    ! total  number of stations (including "duplicate" stations located in halo regions)
    logical         , intent(out) :: error
    character(16)   , intent(in)  :: grpnam
!
! Local variables
!
    integer                               :: ierror    ! Local errorflag for NEFIS files
    integer                               :: i
    integer                               :: ii
    integer                               :: istat
    integer                               :: l
    integer                               :: n
    integer                               :: nm
    integer                               :: m
    integer , external                    :: neferr
    integer , external                    :: putelt
    integer , dimension(3,5)              :: uindex
    real(fp), dimension(:,:), allocatable :: rfbuff    ! work array
    real(sp), dimension(:,:), allocatable :: rsbuff    ! work array
    character(256)                        :: errmsg
!
!! executable statements -------------------------------------------------------
!
    error = .false.
    if (gdp%gdmorpar%flufflyr%iflufflyr==0) return
    if (lsed == 0) return
    !
    nefiselem => gdp%nefisio%nefiselem(nefiswrsedh)
    celidt    => nefiselem%celidt
    mnstat    => gdp%gdstations%mnstat
    order_sta => gdp%gdparall%order_sta
    !
    select case (irequest)
    case (1)
       !
       ! Define elements
       !
       if (inode /= master) return
       !
       call addelm(nefiswrsedh,'MFLUFF',' ','[ KG/M2 ]','REAL',4, &
           & 'Sediment mass in fluff layer (kg/m2)'             , &
           & 2         ,nostatgl,lsed    ,0        ,0       ,0  , &
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
       allocate (rfbuff(1:nostat,1:lsed), stat=istat)
       rfbuff = 0.0_fp
       do l = 1, lsed
          do ii = 1, nostat
             m  = mnstat(1,ii)
             if (m<0) cycle
             n  = mnstat(2,ii)
             if (n<0) cycle
             !
             call n_and_m_to_nm(n, m, nm, gdp)
             !
             rfbuff(ii,l) = mfluff(l,nm)
          enddo
       enddo
       !
       if (inode == master) allocate(rsbuff(1:nostatgl, 1:lsed), stat=istat)
       if (istat /= 0) then
          call prterr(lundia, 'P004', 'wrhfluff: memory allocation error')
       endif
       if (parll) then
          call dfgather_filter(lundia, nostat, nostatto, nostatgl, 1, lsed, order_sta, rfbuff, rsbuff, gdp)
       else
          rsbuff = real(rfbuff,sp)
       endif
       deallocate(rfbuff, stat=istat)
       !
       if (inode == master) then
          call sbuff_checksize(nostatgl*lsed)
          i = 0
          do l = 1, lsed
             do ii = 1, nostatgl
                i = i+1
                sbuff(i) = rsbuff(ii,l)
             enddo
          enddo
          deallocate(rsbuff, stat=istat)
          ierror = putelt(fds, grpnam, 'MFLUFF', uindex, 1, sbuff)
          if (ierror/= 0) goto 9999
       endif !inode==master
       !
       ! write errormessage if error occurred and set error = .true.
       ! the files will be closed in clsnef (called in triend)
       !
 9999  continue
       if (ierror /= 0) then
          ierror = neferr(0, errmsg)
          call prterr(lundia, 'P004', errmsg)
          error = .true.
       endif
    endselect
end subroutine wrhfluff
