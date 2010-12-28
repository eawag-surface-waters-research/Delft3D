subroutine dfwrplot(filnam    ,lundia    ,error     ,mmax      ,nmax      , &
                  & nmaxus    ,kcs       ,ibuff     ,xz        ,yz        , &
                  & rbuff     ,gdp       )
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
!
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
    integer, pointer               :: mmaxgl
    integer, pointer               :: nmaxgl
    integer, pointer               :: nfg
    integer, pointer               :: nlg
    integer, pointer               :: mfg
    integer, pointer               :: mlg
    logical                  , pointer :: first
    integer                  , pointer :: celidt
    integer, dimension(:, :) , pointer :: elmdms
    type (nefiselement)      , pointer :: nefiselem
!
! Local parameters
!
    integer, parameter :: nelmx = 4
!
! Global variables
!
    integer         :: lundia !  Description and declaration in inout.igs
    integer         :: mmax !  Description and declaration in iidim.f90
    integer         :: nmax !  Description and declaration in iidim.f90
    integer         :: nmaxus !  Description and declaration in iidim.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: kcs !  Description and declaration in iidim.f90
    integer, dimension(nmaxus, mmax) :: ibuff !  Description and declaration in iidim.f90
    logical, intent(out)           :: error
                                   !!  Flag=TRUE if an error is encountered
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: xz !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: yz !  Description and declaration in rjdim.f90
    real(fp), dimension(nmaxus, mmax) :: rbuff !  Description and declaration in r-i-ch.igs
    character(*)    :: filnam
                                   !!  Name for output file
                                   !!  Comm. file: com-<case><label>
                                   !!  Map file: trim-<case><label>
!
!
! Local variables
!
    integer                        :: ierr                 ! Flag for error when writing to Communication file
    integer                        :: m
    integer                        :: md
    integer                        :: n
    integer                        :: nd
    integer, dimension(nelmx)      :: nbytsg               ! Array containing the number of by- tes of each single ELMTPS
    integer, external               :: neferr
    logical                        :: wrswch               ! Flag to write file .TRUE. : write to  file .FALSE.: read from file
    character(10), dimension(nelmx) :: elmunt ! Array with element physical unit
    character(16)                  :: grpnam ! Data-group name defined for the COM-files
    character(16), dimension(nelmx) :: elmnms ! Element name defined for the COM-files
    character(16), dimension(nelmx) :: elmqty ! Array with element quantity
    character(16), dimension(nelmx) :: elmtps ! Array containing the types of the elements (real, ch. , etc. etc.)
    character(256)                 :: errmsg ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(64), dimension(nelmx) :: elmdes ! Array with element description
!
    integer, dimension(4,0:nproc-1)       :: iarrc  ! array containing collected grid indices 
    integer                               :: indx   ! array index
    integer                               :: ip     ! node number 
    integer                               :: istart ! start pointer for each subdomain in collected array
    integer                               :: len    ! length of field of current subdomain
    integer                               :: lengl   ! length of field containing collected data
    integer                               :: lenlo   ! length of field containing collected data
    integer, dimension(0:nproc-1)         :: mf     ! first index w.r.t. global grid in x-direction
    integer, dimension(0:nproc-1)         :: ml     ! last index w.r.t. global grid in x-direction
    integer                               :: msiz   ! size of present subdomain in x-direction
    integer, dimension(0:nproc-1)         :: nf     ! first index w.r.t. global grid in y-direction
    integer, dimension(0:nproc-1)         :: nl     ! last index w.r.t. global grid in y-direction
    integer                               :: nsiz   ! size of present subdomain in y-direction
    integer                               :: maxdim4! maximum size in 4'th dimension 
    real(fp), dimension(:), allocatable   :: rdum   ! global array r1 gathered from all nodes
    integer, dimension(:), allocatable    :: idum   ! global array r1 gathered from all nodes
    real(sp), allocatable                 :: rsbuff(:,:)
    integer, allocatable                  :: isbuff1(:,:)
    integer, allocatable                  :: isbuff2(:,:)
!
! Data statements
!
    data grpnam/'TEMPOUT'/
    data elmnms/'XWAT', 'YWAT', 'CODB', 'CODW'/
    data elmqty/4*' '/
    data elmunt/2*'[   M   ]', 2*'[   -   ]'/
    data elmtps/2*'REAL', 2*'INTEGER'/
    data nbytsg/4*4/
    data elmdes/'X-coord. water level point in local system                    '&
       & , 'Y-coord. water level point in local system                    ',    &
        & '1/-1 Active/Non-active bottom point ( w.r.t. coordinates )    ',      &
        & '1/-1 Active/Non-active water level point (w.r.t. coordinates )'/
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefiswrplot)
    first   => nefiselem%first
    celidt  => nefiselem%celidt
    elmdms  => nefiselem%elmdms
    !
    !-----Initialize local variables
    !
    ierr = 0
    wrswch = .true.
    !
    call dfsync(gdp)
    mmaxgl    => gdp%gdparall%mmaxgl
    nmaxgl    => gdp%gdparall%nmaxgl
    nfg       => gdp%gdparall%nfg
    nlg       => gdp%gdparall%nlg
    mfg       => gdp%gdparall%mfg
    mlg       => gdp%gdparall%mlg
    !
    ! gather grid indices of all subdomains
    !
    call dfgather_grddim(lundia, nfg, nlg, mfg, mlg, nmaxgl, mmaxgl, &
       &                 nf, nl, mf, ml, iarrc, lengl, lenlo, gdp )
    !
    !-----Set up the element dimensions
    !
    if (first .and. inode==master) then
       first = .false.
       call filldm(elmdms    ,1         ,2         ,nmaxgl    ,mmaxgl    , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,2         ,2         ,nmaxgl    ,mmaxgl    , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,3         ,2         ,nmaxgl    ,mmaxgl    , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,4         ,2         ,nmaxgl    ,mmaxgl    , &
                 & 0         ,0         ,0         )
    endif
    !
    !-----Write all elements to file; all definition and creation of files,
    !     data groups, cells and elements is handled by PUTGET.
    !
    !-----element  1 XZ
    !
    if (inode == master) then
       allocate(rdum(lengl))
    endif
    call dfgather_lowlevel ( rdum, lengl, xz, lenlo, dfloat, gdp )
    if (inode == master) then
       allocate( rsbuff(nmaxgl, mmaxgl) )
       istart = 0
       do ip = 0, nproc-1
          !
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          !
          do n = nf(ip), nl(ip)
             do m = mf(ip), ml(ip)
                indx = istart + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                rsbuff(n, m) = real(rdum(indx), sp)
             enddo
          enddo
          !
          istart = istart + msiz*nsiz
          !
       enddo
       !
       call putgtr(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(1) ,celidt    ,wrswch    ,ierr      ,rsbuff     )
       deallocate( rsbuff )
    endif
    !
    if (ierr/=0) goto 9999
    !
    !-----element  2 YZ
    !
    call dfgather_lowlevel ( rdum, lengl, yz, lenlo, dfloat, gdp )
    if (inode == master) then
       allocate( rsbuff(nmaxgl, mmaxgl) )
       istart = 0
       do ip = 0, nproc-1
          !
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          !
          do n = nf(ip), nl(ip)
             do m = mf(ip), ml(ip)
                indx = istart + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                rsbuff(n, m) = real(rdum(indx), sp)
             enddo
          enddo
          !
          istart = istart + msiz*nsiz
          !
       enddo
       !
       call putgtr(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(2) ,celidt    ,wrswch    ,ierr      ,rsbuff     )
       deallocate( rsbuff )
    endif
    !
    if (ierr/=0) goto 9999
    !
    !-----element  3 CODB
    !
    if (inode == master) then
       allocate(idum(lengl))
    endif
    call dfgather_lowlevel ( idum, lengl, kcs, lenlo, dfint, gdp )
    if (inode == master) then
       allocate(isbuff1(nmaxgl,mmaxgl))
       istart = 0
       do ip = 0, nproc-1
          !
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          !
          do n = nf(ip), nl(ip)
             do m = mf(ip), ml(ip)
                indx = istart + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                isbuff1(n, m) = idum(indx)
             enddo
          enddo
          !
          istart = istart + msiz*nsiz
          !
       enddo
       allocate(isbuff2(nmaxgl,mmaxgl))
       do m = 1, mmaxgl
          isbuff2(1, m) = -1
       enddo
       do n = 1, nmaxgl
          isbuff2(n, 1) = -1
       enddo
       do m = 2, mmaxgl
          md = m - 1
          do n = 2, nmaxgl
             nd = n - 1
             isbuff2(n, m) = -1
             if (isbuff1(n, m)==1) then
                isbuff2(n, m) = 1
                isbuff2(n, md) = 1
                isbuff2(nd, m) = 1
                isbuff2(nd, md) = 1
             endif
          enddo
       enddo
       call putgti(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(3) ,celidt    ,wrswch    ,ierr      ,isbuff2    )
    endif
    !
    if (ierr/=0) goto 9999
    !
    !-----element  4 CODW
    !
    if (inode == master) then
       do m = 1, mmaxgl
          do n = 1, nmaxgl
             isbuff2(n, m) = -1
             if (isbuff1(n, m)==1) isbuff2(n, m) = 1
          enddo
       enddo
       call putgti(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(4) ,celidt    ,wrswch    ,ierr      ,isbuff2    )
       deallocate( isbuff2 )
       deallocate( isbuff1 )
    endif
    if (ierr/=0) then
    endif
    !
 9999 continue
    if (inode == master) then
       deallocate( rdum )
       deallocate( idum )
    endif
    if (ierr/=0) then
       ierr = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine dfwrplot
