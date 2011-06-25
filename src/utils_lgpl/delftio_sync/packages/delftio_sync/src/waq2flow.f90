subroutine waq2flow(dps, mmax, nmaxus, kmax, lundia, mlb, mub, nlb, nub)
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!    Function: - Receive updated bed level
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dio_plt_rw
    !
    implicit none
!
! Global variables
!
    integer                                , intent(in)  :: mlb
    integer                                , intent(in)  :: mub
    integer                                , intent(in)  :: nlb
    integer                                , intent(in)  :: nub
    integer                                , intent(in)  :: mmax
    integer                                , intent(in)  :: nmaxus
    integer                                , intent(in)  :: kmax
    integer                                , intent(in)  :: lundia
    real(prec), dimension(nlb:nub, mlb:mub)              :: dps    !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer, external                 :: diocreatestreamsynched
    integer, external                 :: dioGetPltDataSetInfo
    integer, save                     :: diooutset
    integer, save                     :: diooutstream
    integer                           :: ierr_alloc    
    integer                           :: ilumon
    integer                           :: istep,iseg,m,n
    integer                           :: noseg
    integer                           :: nrtims
    integer                           :: nrvar
    logical, save                     :: first = .true.
    real, allocatable     , save      :: dps0(:,:)
    real, allocatable     , save      :: parval(:,:)
    real, allocatable     , save      :: thick(:,:) ! sediment thickness in m
    real, allocatable     , save      :: thick0(:,:) ! sediment thickness in m
    double precision                  :: time(1)
    character(len=20)     , save      :: datasetname
    character, allocatable, save      :: locs(:)
    character(len=20)     , save      :: parnam(1)
    character(len=20)     , save      :: streamname
    character*(dioMaxTimLen)          :: tims(1)
!
!! executable statements -------------------------------------------------------
!
    !
    ! initialise
    !
    noseg = nmaxus*mmax*kmax
    nrvar =   1
    if (first) then
       !
       ! allocate output array
       !
       allocate(parval(nrvar,noseg),locs(noseg),stat=ierr_alloc)
       allocate(thick(nlb:nub, mlb:mub))
       allocate(thick0(nlb:nub, mlb:mub))
       allocate(dps0(nlb:nub, mlb:mub))
       if ( ierr_alloc .ne. 0 ) then
          write(*,*)      'errror waq2flow: allocating work array'
          write(lundia,*) 'errror waq2flow: allocating work array'
          stop 'errror waq2flow: allocating work array'
       endif
       !
       write(lundia,*) '--------------------------------------------'
       write(lundia,*) '| WAQ2FLOW communication anticipated       |'
       write(lundia,*) '| quantity expected:                       |'
       write(lundia,*) '|        sediment thickness in m           |'
       write(lundia,*) '--------------------------------------------'
       !
       ! create DelftIO stream
       !
       streamname   = 'waq2flow'
       datasetname  = 'datawaq2flow'
       write(*,*) '--------------' 
       write(*,*) 'FLOW: waiting for waq2flow DIO stream to open'
       diooutstream = diocreatestreamsynched(dio_binary_stream, streamname, 'r')
       write(*,*) 'FLOW: waq2flow DIO stream is open'
       write(*,*) 'FLOW: waiting for GetPltDataSet waq2flow (datawaq2flow)'
       diooutset    = dioGetPltDataSetInfo (diooutstream, datasetname, &
                      & nrvar, parnam, noseg, locs, nrtims, tims)
       write(*,*) 'FLOW: GetPltDataSet waq2flow (datawaq2flow) returned'
       !
       istep = 0
    endif
    !
    write(*,*) 'FLOW: waiting for GetPltDataSetReals waq2flow (datawaq2flow)'
    call diogetpltdatasetreals(diooutset, tims(1), nrvar, noseg, parval)
    write(*,*) 'FLOW: GetPltDataSetReals waq2flow (datawaq2flow) returned'
    write(*,*) '--------------' 
    !
    iseg=mmax*nmaxus*(kmax-1)
    do m=1,mmax
       do n=1,nmaxus
          iseg=iseg+1
          thick(n,m)=parval(1,iseg)
       enddo
    enddo
    !
    if (first) then
       first = .false.
       thick0=thick
       dps0=dps
    else
       thick=thick-thick0
       dps=dps0-thick
    endif
end subroutine waq2flow
