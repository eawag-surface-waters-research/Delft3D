subroutine dfwrmfluff(lundia    ,error     ,mmax      ,nmaxus    ,lsed      , &
                    & irequest  ,fds       ,grpnam    ,gdp  )
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
    use bedcomposition_module
    use dfparall
    use globaldata
    use dffunctionals
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer              , pointer :: celidt
    type (nefiselement)  , pointer :: nefiselem
    integer              , pointer :: mfg
    integer              , pointer :: mlg
    integer              , pointer :: nfg
    integer              , pointer :: nlg
    integer              , pointer :: nmaxgl
    integer              , pointer :: mmaxgl
!
! Global variables
!
    integer                                                :: fds
    integer                                                :: irequest
    integer                                                :: lsed
    integer                                                :: lundia
    integer                                                :: mmax
    integer                                                :: nmaxus
    logical                                                :: error
    character(16)                                          :: grpnam
!
! Local variables
!
    integer                                 :: ierror       ! Local errorflag for NEFIS files
    integer                                 :: i
    integer                                 :: l
    integer                                 :: m
    integer                                 :: n
    integer                                 :: nm
    integer, external                       :: neferr
    integer, external                       :: putelt
    integer, dimension(3,5)                 :: uindex
    integer , dimension(4,0:nproc-1)        :: iarrc  ! array containing collected grid indices 
    integer                                 :: lenlo  ! length of field of current subdomain
    integer                                 :: lengl  ! length of field containing collected data
    integer , dimension(0:nproc-1)          :: mf     ! first index w.r.t. global grid in x-direction
    integer , dimension(0:nproc-1)          :: ml     ! last index w.r.t. global grid in x-direction
    integer                                 :: msiz   ! size of present subdomain in x-direction
    integer , dimension(0:nproc-1)          :: nf     ! first index w.r.t. global grid in y-direction
    integer , dimension(0:nproc-1)          :: nl     ! last index w.r.t. global grid in y-direction
    integer                                 :: nsiz   ! size of present subdomain in y-direction
    real(fp), dimension(:,:,:), allocatable :: rbuff3
    character(256)                          :: errmsg
    real(fp) , dimension(:,:) , pointer     :: mfluff
!
!! executable statements -------------------------------------------------------
!
    if (gdp%gdmorpar%flufflyr%iflufflyr==0) return
    if (lsed == 0) return
    !
    nefiselem => gdp%nefisio%nefiselem(nefiswrsedm)
    celidt    => nefiselem%celidt
    mfg       => gdp%gdparall%mfg
    mlg       => gdp%gdparall%mlg
    nfg       => gdp%gdparall%nfg
    nlg       => gdp%gdparall%nlg
    mmaxgl    => gdp%gdparall%mmaxgl
    nmaxgl    => gdp%gdparall%nmaxgl
    !
    ierror = 0
    !
    select case (irequest)
    case (1)
       !
       ! Define elements
       !
       call addelm(nefiswrsedm,'MFLUFF',' ','[ KG/M2 ]','REAL',4, &
           & 'Sediment mass in fluff layer (kg/m2)'             , &
           & 3      ,nmaxgl ,mmaxgl   ,lsed     ,0       ,0     , &
           & lundia ,gdp    )
    case (2)
       !
       ! allocate data arrays for collection data 
       !
       ! gather LOCAL grid indices of all partitions
       !
       call dfsync(gdp)
       call dfgather_grddim(lundia, nfg, nlg, mfg, mlg, nmaxgl, mmaxgl, &
          &                 nf, nl, mf, ml, iarrc, lengl, lenlo, gdp )
       !
       ! broadcast LOCAL grid indices to ALL partitions
       ! so every partition knows the dimensions and positions
       ! of the other partitions in the global domain
       !
       call dfbroadc_gdp ( iarrc, 4*nproc, dfint, gdp )
       call dfbroadc_gdp ( nf, nproc, dfint, gdp )
       call dfbroadc_gdp ( nl, nproc, dfint, gdp )
       call dfbroadc_gdp ( mf, nproc, dfint, gdp )
       call dfbroadc_gdp ( ml, nproc, dfint, gdp )
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
       allocate(rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsed))
       do l = 1, lsed
          do m = 1, mmax
             do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff3(n,m,l) = mfluff(l, nm)
             enddo
          enddo
       enddo
       call dfgather(rbuff3,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff3)
       if (inode == master) then
          ierror = putelt(fds, grpnam, 'MFLUFF', uindex, 1, glbarr3)
       endif
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
end subroutine dfwrmfluff
