subroutine wrthis(lundia    ,error     ,trifil    ,selhis    ,ithisc    , &
                  & itstrt    ,ithisi    ,zmodel    ,nostat    ,ntruv     , &
                  & kmax      ,lmax      ,lstsci    ,lsal      ,ltem      , &
                  & ltur      ,zkfs      ,zwl       ,zcuru     ,zcurv     , &
                  & zcurw     ,zqxk      ,zqyk      ,ztauks    ,ztauet    , &
                  & zvicww    ,zdicww    ,zrich     ,zrho      ,gro       , &
                  & ztur      ,zvort     ,zenst     ,hydprs    ,fltr      , &
                  & ctr       ,atr       ,dtr       ,velt      ,zdps      , &
                  & gdp       )
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
!    Function: Writes the time varying groups (1 & 3) to the
!              NEFIS HIS-DAT file
!              Selection is done using SELHIS. For elements like
!              ZCURW where KMAX must be > 1 this coupling between
!              KMAX and SELHIS is done in subroutine RDPRFL
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sp_buffer
    use dfparall
    use globaldata
    !
    use dffunctionals
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer       , dimension(:)    , pointer :: line_orig
    logical                         , pointer :: first
    integer                         , pointer :: celidt
    integer       , dimension(:, :) , pointer :: elmdms
    type (nefiselement)             , pointer :: nefiselem
    integer       , dimension(:, :) , pointer :: mnstat
    real(fp)      , dimension(:, :) , pointer :: xystat
    integer       , dimension(:)    , pointer :: order_sta
    character(20) , dimension(:)    , pointer :: namst
    integer                         , pointer :: mfg
    integer                         , pointer :: nfg

    character*(10)                  , pointer :: trans_unit !  Unit of the variables ATR and DTR


!
! Global variables
!
    integer                                        , intent(in)  :: ithisc !!  Current time counter for the history data file
    integer                                                      :: ithisi !  Description and declaration in inttim.igs
    integer                                                      :: itstrt !  Description and declaration in inttim.igs
    integer                                                      :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                      :: lmax   !  Description and declaration in dimens.igs
    integer                                                      :: lsal   !  Description and declaration in dimens.igs
    integer                                                      :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                      :: ltem   !  Description and declaration in dimens.igs
    integer                                                      :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                                      :: lundia !  Description and declaration in inout.igs
    integer                                                      :: nostat !  Description and declaration in dimens.igs
    integer                                                      :: ntruv  !  Description and declaration in dimens.igs
    integer      , dimension(nostat)                             :: zkfs   !  KFS in monitoring station
    logical                                        , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    logical                                        , intent(in)  :: zmodel !  Description and declaration in procs.igs
    real(fp)     , dimension(nostat)                             :: zdps   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat)                             :: ztauet !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat)                             :: ztauks !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat)                             :: zwl    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, 0:kmax)                     :: zdicww !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, 0:kmax)                     :: zrich  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, 0:kmax)                     :: zvicww !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, 0:kmax, ltur)               :: ztur   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                       :: hydprs !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                       :: zcuru  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                       :: zcurv  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                       :: zcurw  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                       :: zenst  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                       :: zqxk   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                       :: zqyk   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                       :: zrho   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                       :: zvort  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax, lstsci)               :: gro    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(ntruv)                              :: ctr    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(ntruv)                              :: fltr   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(ntruv, lstsci)                      :: atr    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(ntruv, lstsci)                      :: dtr    !  Description and declaration in esm_alloc_real.f90
    character(*)                                   , intent(in)  :: trifil !!  File name for FLOW NEFIS output
                                                                           !! files (tri"h/m"-"casl""labl".dat/def)
    character(23)                                  , intent(in)  :: selhis !  Description and declaration in tricom.igs
    character(10)                                  , intent(in)  :: velt   !! Velocity type 'eulerian' or 'GLM'
!
! Local variables
!
    integer                                          :: fds
    integer                                          :: i         ! Help var. 
    integer                                          :: ierror   ! Local errorflag for NEFIS files 
    integer                                          :: lastcl
    integer        , dimension(1)                    :: idummy    ! Help array to read/write Nefis files 
    integer        , dimension(3,5)                  :: uindex
    integer                           , external     :: getelt
    integer                           , external     :: putelt
    integer                           , external     :: inqmxi
    integer                           , external     :: clsnef
    integer                           , external     :: open_datdef
    integer                           , external     :: neferr
    integer                                          :: n         ! Help var.
    integer                                          :: m         ! Help var.
    integer                                          :: nostatgl  ! global number of stations (i.e. original number excluding duplicate stations located in the halo regions)
    integer                                          :: nostatto  ! total number of stations (including "duplicate" stations located in halo regions)
    integer                                          :: ntruvgl   ! global number of tracks (i.e. original number excluding duplicate stations located in the halo regions)
    integer                                          :: ntruvto   ! total number of tracks (including "duplicate" stations located in halo regions)
    integer        , dimension(:)    , allocatable   :: ibuff1    ! work array
    integer        , dimension(:,:)  , allocatable   :: ibuff2    ! work array
    integer        , dimension(:,:)  , allocatable   :: ibuff2b   ! work array
    integer        , dimension(:)    , allocatable   :: nostatarr ! number of stations per partition
    logical                                          :: cross_sec ! option to sum results from cross-sections across partitions
    real(fp)       , dimension(:)      , allocatable :: rbuff1
    real(fp)       , dimension(:,:)    , allocatable :: rbuff2
    real(fp)       , dimension(:,:,:)  , allocatable :: rbuff3
    real(fp)       , dimension(:,:,:,:), allocatable :: rbuff4
    real(sp)       , dimension(:)      , allocatable :: rsbuff1   ! work array
    real(sp)       , dimension(:,:)    , allocatable :: rsbuff2   ! work array
    real(sp)       , dimension(:,:,:)  , allocatable :: rsbuff3   ! work array
    character(16)                                    :: grnam1    ! Data-group name defined for the NEFIS-files group 1 
    character(16)                                    :: grnam3    ! Data-group name defined for the NEFIS-files group 3 
    character(256)                                   :: filnam    ! Help var. for FLOW file name 
    character(256)                                   :: errmsg    ! Character var. containing the errormessage to be written to file. The message depends on the error. 
!
! Data statements
!
    data grnam1/'his-info-series'/
    data grnam3/'his-series'/
!
!! executable statements -------------------------------------------------------
!
    line_orig  => gdp%gdstations%line_orig
    nefiselem  => gdp%nefisio%nefiselem(nefiswrthisinf)
    first      => nefiselem%first
    celidt     => nefiselem%celidt
    elmdms     => nefiselem%elmdms
    mnstat     => gdp%gdstations%mnstat
    xystat     => gdp%gdstations%xystat
    order_sta  => gdp%gdparall%order_sta
    namst      => gdp%gdstations%namst
    mfg        => gdp%gdparall%mfg
    nfg        => gdp%gdparall%nfg
    !
    ! Initialize local variables
    !
    filnam = trifil(1:3) // 'h' // trifil(5:)
    !
    !
    errmsg = ' '
    !
    ! initialize group index time dependent data
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time

    if (parll) then 
       !
       ! Recalculates the effective number of stations, filtering out duplicates affected to more
       ! than one partition (i.e. located in halos)
       !
       call dfsync(gdp)
       call dffind_duplicate(lundia, nostat, nostatto, nostatgl, order_sta, gdp)
       !
       ! Recalculates the effective global number of cross sections
       !
       call dfsync(gdp)
       call dffind_duplicate(lundia, ntruv, ntruvto, ntruvgl, line_orig, gdp)

    else
       nostatto = nostat
       nostatgl = nostat
       ntruvto = ntruv
       ntruvgl = ntruv
    endif

    !
    ! Set up the element dimensions
    !
    if (first .and. inode == master) then
       !
       ! Set up the element chracteristics
       !
       ! his-info-series
       !
       call addelm(nefiswrthisinf,'ITHISC',' ','[   -   ]','INTEGER',4    , &
          & 'timestep number (ITHISC*DT*TUNIT := time in sec from ITDATE)  ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call defnewgrp(nefiswrthisinf ,filnam    ,grnam1   ,gdp)
       !
       ! his-series
       !
       if (nostatgl > 0) then
          if (selhis(1:1)=='Y') then
             call addelm(nefiswrthis,'ZKFS',' ','[   -   ]','INTEGER',4              , &
                & 'Non-active (0) or active (1) zeta point (time-dependent)      ', &
                & 1         ,nostatgl  ,0         ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (selhis(1:1)=='Y') then
             call addelm(nefiswrthis,'ZWL',' ','[   M   ]','REAL',4              , &
                & 'Water-level in station (zeta point)                           ', &
                & 1         ,nostatgl  ,0         ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (index(selhis(2:3), 'Y')>0) then
             call addelm(nefiswrthis,'ZCURU',' ','[  M/S  ]','REAL',4              , &
                & 'U-velocity per layer in station (zeta point, '//velt//')      ', &
                & 2         ,nostatgl  ,kmax      ,0         ,0         ,0      , &
                & lundia    ,gdp       )
             call addelm(nefiswrthis,'ZCURV',' ','[  M/S  ]','REAL',4              , &
                & 'V-velocity per layer in station (zeta point, '//velt//')      ', &
                & 2         ,nostatgl  ,kmax      ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (selhis(4:4)=='Y') then
             call addelm(nefiswrthis,'ZCURW',' ','[  M/S  ]','REAL',4              , &
                & 'W-velocity per layer in station (zeta point)                  ', &
                & 2         ,nostatgl  ,kmax      ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (selhis(20:20)=='Y') then
             call addelm(nefiswrthis,'ZQXK',' ','[  M3/S ]','REAL',4              , &
                & 'U-discharge per layer in station (zeta point)                 ', &
                & 2         ,nostatgl  ,kmax      ,0         ,0         ,0      , &
                & lundia    ,gdp       )
             call addelm(nefiswrthis,'ZQYK',' ','[  M3/S ]','REAL',4              , &
                & 'V-discharge per layer in station (zeta point)                 ', &
                & 2         ,nostatgl  ,kmax      ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (index(selhis(5:12), 'Y')/=0) then
             call addelm(nefiswrthis,'GRO',' ','[   -   ]','REAL',4              , &
                & 'Concentrations per layer in station (zeta point)              ', &
                & 3         ,nostatgl  ,kmax      ,lstsci    ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (index(selhis(13:14), 'Y')/=0) then
             call addelm(nefiswrthis,'ZTUR',' ','[   -   ]','REAL',4              , &
                & 'Turbulent quantity per layer in station (zeta point)          ', &
                & 3         ,nostatgl  ,kmax + 1  ,ltur      ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (index(selhis(15:16), 'Y')>0) then
             call addelm(nefiswrthis,'ZTAUKS',' ','[  N/M2 ]','REAL',4              , &
                & 'Bottom stress U in station (zeta point)                       ', &
                & 1         ,nostatgl  ,0         ,0         ,0         ,0      , &
                & lundia    ,gdp       )
             call addelm(nefiswrthis,'ZTAUET',' ','[  N/M2 ]','REAL',4              , &
                & 'Bottom stress V in station (zeta point)                       ', &
                & 1         ,nostatgl  ,0         ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (selhis(17:17)=='Y') then
             call addelm(nefiswrthis,'ZVICWW',' ','[  M2/S ]','REAL',4              , &
                & 'Vertical eddy viscosity-3D in station (zeta point)            ', &
                & 2         ,nostatgl  ,kmax + 1  ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (selhis(18:18)=='Y') then
             call addelm(nefiswrthis,'ZDICWW',' ','[  M2/S ]','REAL',4              , &
                & 'Vertical eddy diffusivity-3D in station (zeta point)          ', &
                & 2         ,nostatgl  ,kmax + 1  ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (index(selhis(17:18), 'Y')>0) then
             call addelm(nefiswrthis,'ZRICH',' ','[   -   ]','REAL',4              , &
                & 'Richardson number in station (zeta point)                     ', &
                & 2         ,nostatgl  ,kmax + 1  ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (selhis(19:19)=='Y') then
             call addelm(nefiswrthis,'ZRHO',' ','[ KG/M3 ]','REAL',4              , &
                & 'Density per layer in station (zeta point)                     ', &
                & 2         ,nostatgl  ,kmax      ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (zmodel) then
             if (selhis(2:2)=='Y') then
                call addelm(nefiswrthis,'HYDPRES',' ','[  N/M2 ]','REAL',4              , &
                   & 'Non-hydrostatic pressure at station (zeta point)              ', &
                   & 2         ,nostatgl  ,kmax      ,0         ,0         ,0      , &
                   & lundia    ,gdp       )
             endif
          endif
          call addelm(nefiswrthis,'XYSTAT',' ','[   M   ]','REAL',4              , &
             & '(X,Y) coordinates of monitoring stations                      ', &
             & 2         ,2         ,nostatgl  ,0         ,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrthis,'MNSTAT',' ','[   -   ]','INTEGER',4              , &
             & '(M,N) indices of monitoring stations                          ', &
             & 2         ,2         ,nostatgl  ,0         ,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrthis,'DPS',' ','[   M   ]','REAL',4              , &
             & 'Depth in station                                              ', & ! same as in wrihis
             & 1         ,nostatgl  ,0         ,0         ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (ntruvgl > 0) then
          if (selhis(20:20)=='Y') then
             call addelm(nefiswrthis,'FLTR',' ','[   M3  ]','REAL',4              , &
                & 'Total discharge through cross section (velocity points)       ', &
                & 1         ,ntruvgl   ,0         ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (selhis(21:21)=='Y') then
             call addelm(nefiswrthis,'CTR',' ','[  M3/S ]','REAL',4              , &
                & 'Monumentary discharge through cross section (velocity points) ', &
                & 1         ,ntruvgl   ,0         ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (selhis(22:22)=='Y') then
             call addelm(nefiswrthis,'ATR',' ','[   -   ]','REAL',4              , &
                & 'Advective transport through cross section (velocity points)   ', &
                & 2         ,ntruvgl   ,lstsci    ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (selhis(23:23)=='Y') then
             call addelm(nefiswrthis,'DTR',' ','[   -   ]','REAL',4              , &
                & 'Dispersive transport through cross section (velocity points)  ', &
                & 2         ,ntruvgl   ,lstsci    ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
       endif
       !
       call defnewgrp(nefiswrthis ,filnam    ,grnam3   ,gdp)
       !
       ! Get start celidt for writing
       !
       nefiselem => gdp%nefisio%nefiselem(nefiswrthisinf)
       first     => nefiselem%first
       celidt    => nefiselem%celidt
    endif
    !
    call dfsync(gdp)
    ierror = 0
    if (inode == master) ierror = open_datdef(filnam   ,fds      )
    if (ierror/= 0) goto 999
    if (inode == master) then
       if (first) then
          !
          ! end of initialization, don't come here again
          !
          ierror = inqmxi(fds, grnam1, celidt)
          first = .false.
       endif
       !
       ! Writing of output on every ithisc
       !
       celidt = celidt + 1
       !
       ! Overwriting instead of appending if time is already on file
       !
       ! group 1, element 1 'ITHISC'
       !
       !-->
      10 continue
       if (celidt>1) then
          idummy(1) = -1
          lastcl = celidt - 1
          uindex(1,1) = lastcl
          uindex(2,1) = lastcl
          ierror     = getelt(fds, grnam1, 'ITHISC', uindex, 1, 4, idummy)
          if (ierror/=0) goto 999
          if (idummy(1)>=ithisc) then
             celidt = lastcl
             goto 10
          endif
       else
          celidt = 1
       endif
       !<--
       idummy(1) = ithisc
       uindex(1,1) = celidt
       uindex(2,1) = celidt
       !
       ! celidt is obtained by investigating group his-inf-series, identified
       ! with nefiswrthisinf.
       ! Group his-series, identified with nefiswrthis, must use the same
       ! value for celidt.
       ! Easy solution:
       gdp%nefisio%nefiselem(nefiswrthis)%celidt = celidt
       ! Neat solution in pseudo code:
       ! subroutine wrthis
       ! integer :: celidt
       ! celidt = detectcelidt(nefiswrthisinf)
       ! call wrthisinf(celidt)
       ! call wrthisdat(celidt)
       ! end subroutine
       !
       ierror     = putelt(fds, grnam1, 'ITHISC', uindex, 1, idummy)
    endif
    !
    if (ierror/=0) goto 999
    !
    ! group 3, first 16 depend on NOSTAT > 0
    !
    if (nostatgl > 0) then
       !
       ! group 3: element 'ZKFS' only if SELHIS( 1: 1) = 'Y'
       !
       if (selhis(1:1)=='Y') then
          if (inode == master) allocate( ibuff1(1:nostatgl) )
          if (parll) then    
             call dfgather_filter(lundia, nostat, nostatto, nostatgl, order_sta, zkfs, ibuff1, gdp )
          else
             ibuff1 = zkfs
          endif     
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'ZKFS', uindex, 1, ibuff1)
             deallocate( ibuff1 )
          endif
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'ZWL' only if SELHIS( 1: 1) = 'Y'
       !
       if (selhis(1:1)=='Y') then
          if (inode == master) allocate( rsbuff1(1:nostatgl) )
          if (parll) then   
             call dfgather_filter(lundia, nostat, nostatto, nostatgl, order_sta, zwl, rsbuff1, gdp )
          else
             rsbuff1 = real(zwl, sp)
          endif
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'ZWL', uindex, 1, rsbuff1)
             deallocate( rsbuff1 )
          endif
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'ZCURU' & 'ZCURV'
       ! only if SELHIS( 2: 3) <> 'NN'
       !
       if (index(selhis(2:3), 'Y')>0) then
          if (inode == master) allocate( rsbuff2(1:nostatgl, 1:kmax) )
          if (parll) then
             call dfgather_filter(lundia, nostat, nostatto, nostatgl, 1, kmax, order_sta, zcuru, rsbuff2, gdp)
          else
             rsbuff2 = real(zcuru, sp)
          endif  
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'ZCURU', uindex, 1, rsbuff2)
             deallocate( rsbuff2 )
          endif
          if (ierror/=0) goto 999
          !
          ! group 3: element 'ZCURV'
          !
          if (inode == master) allocate( rsbuff2(1:nostatgl, 1:kmax) )
          if (parll) then  
             call dfgather_filter(lundia, nostat, nostatto, nostatgl, 1, kmax, order_sta, zcurv, rsbuff2, gdp)
          else
             rsbuff2 = real(zcurv, sp)
          endif 
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'ZCURV', uindex, 1, rsbuff2)
             deallocate( rsbuff2 )
          endif
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'ZCURW', only if KMAX > 1
       ! (:= SELHIS( 4: 4) = 'Y')
       !
       if (selhis(4:4)=='Y') then
          if (inode == master) allocate( rsbuff2(1:nostatgl, 1:kmax) )
          if (parll) then
             call dfgather_filter(lundia, nostat, nostatto, nostatgl, 1, kmax, order_sta, zcurw, rsbuff2, gdp)
          else
             rsbuff2 = real(zcurw, sp)
          endif  
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'ZCURW', uindex, 1, rsbuff2)
             deallocate( rsbuff2 )
          endif
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'ZQXK' only if SELHIS(20:20) = 'Y'
       !
       if (selhis(20:20)=='Y') then
          if (inode == master) allocate( rsbuff2(1:nostatgl, 1:kmax) )
          if (parll) then 
             call dfgather_filter(lundia, nostat, nostatto, nostatgl, 1, kmax, order_sta, zqxk, rsbuff2, gdp)
          else
             rsbuff2 = real(zqxk, sp)
          endif 
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'ZQXK', uindex, 1, rsbuff2)
             deallocate( rsbuff2 )
          endif
          if (ierror/=0) goto 999
          !
          ! group 3: element 'ZQYK' only if SELHIS(20:20) = 'Y'
          !
          if (inode == master) allocate( rsbuff2(1:nostatgl, 1:kmax) )
          if (parll) then
             call dfgather_filter(lundia, nostat, nostatto, nostatgl, 1, kmax, order_sta, zqyk, rsbuff2, gdp)
          else
             rsbuff2 = real(zqyk, sp)
          endif 
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'ZQYK', uindex, 1, rsbuff2)
             deallocate( rsbuff2 )
          endif
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'GRO', only if LSTSCI > 0
       ! (:= SELHIS( 5:12) <> 'NNNNNNNN')
       !
       if (index(selhis(5:12), 'Y')/=0) then
          if (inode == master) allocate( rsbuff3(1:nostatgl, 1:kmax, 1:lstsci) )
          if (parll) then
             call dfgather_filter(lundia, nostat, nostatto, nostatgl, 1, kmax, 1, lstsci, order_sta, gro, rsbuff3, gdp)
          else
             rsbuff3 = real(gro, sp)
          endif
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'GRO', uindex, 1, rsbuff3)
             deallocate( rsbuff3 )
          endif
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'ZTUR', only if LTUR > 0
       ! (:= SELHIS(13:14) <> 'NN')
       !
       if (index(selhis(13:14), 'Y')/=0) then
          if (inode == master) allocate( rsbuff3(1:nostatgl, 0:kmax, 1:ltur) )
          if (parll) then
             call dfgather_filter(lundia, nostat, nostatto, nostatgl, 0, kmax, 1, ltur, order_sta, ztur, rsbuff3, gdp)
          else
             rsbuff3 = real(ztur, sp)
          endif
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'ZTUR', uindex, 1, rsbuff3)
             deallocate( rsbuff3 )
          endif
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'ZTAUKS' & 'ZTAUET'
       ! only if SELHIS(15:16) <> 'NN'
       !
       if (selhis(15:15)=='Y') then
          if (inode == master) allocate( rsbuff1(1:nostatgl) )
          if (parll) then
             call dfgather_filter(lundia, nostat, nostatto, nostatgl, order_sta, ztauks, rsbuff1, gdp)
          else
             rsbuff1 = real(ztauks, sp)
          endif
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'ZTAUKS', uindex, 1, rsbuff1)
             deallocate( rsbuff1 )
          endif
          if (ierror/=0) goto 999
          !
          ! group 3: element 'ZTAUET'
          !
          if (inode == master) allocate( rsbuff1(1:nostatgl) )
          if (parll) then 
            call dfgather_filter(lundia, nostat, nostatto, nostatgl, order_sta, ztauet, rsbuff1, gdp)
          else
             rsbuff1 = real(ztauet, sp)
          endif
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'ZTAUET', uindex, 1, rsbuff1)
             deallocate( rsbuff1 )
          endif
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'ZVICWW', only if KMAX > 1
       ! (:= SELHIS(17:17) = 'Y')
       !
       if (selhis(17:17)=='Y') then
          if (inode == master) allocate( rsbuff2(1:nostatgl, 0:kmax) )
          if (parll) then 
             call dfgather_filter(lundia, nostat, nostatto, nostatgl, 0, kmax, order_sta, zvicww, rsbuff2, gdp)
          else
             rsbuff2 = real(zvicww, sp)
          endif
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'ZVICWW', uindex, 1, rsbuff2)
             deallocate( rsbuff2 )
          endif
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'ZDICWW', only if KMAX > 1
       ! (:= SELHIS(18:18) = 'Y')
       !
       if (selhis(18:18)=='Y') then
          if (inode == master) allocate( rsbuff2(1:nostatgl, 0:kmax) )
          if (parll) then  
             call dfgather_filter(lundia, nostat, nostatto, nostatgl, 0, kmax, order_sta, zdicww, rsbuff2, gdp)
          else
             rsbuff2 = real(zdicww, sp)
          endif 
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'ZDICWW', uindex, 1, rsbuff2)
             deallocate( rsbuff2 )
          endif
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'ZRICH', only if KMAX > 1
       ! (:= SELHIS(17:18) <> 'NN')
       !
       if (index(selhis(17:18), 'Y')>0) then
          if (inode == master) allocate( rsbuff2(1:nostatgl, 0:kmax) )
          if (parll) then
             call dfgather_filter(lundia, nostat, nostatto, nostatgl, 0, kmax, order_sta, zrich, rsbuff2, gdp)
          else
             zrich = real(rsbuff2, sp)
          endif
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'ZRICH', uindex, 1, rsbuff2)
             deallocate( rsbuff2 )
          endif
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'ZRHO', only if LSAL > 0 or LTEM > 0
       ! (:= SELHIS(19:19) = 'Y')
       !
       if (selhis(19:19)=='Y') then
          if (inode == master) allocate( rsbuff2(1:nostatgl, 1:kmax) )
          if (parll) then 
             call dfgather_filter(lundia, nostat, nostatto, nostatgl, 1, kmax, order_sta, zrho, rsbuff2, gdp)
          else
             rsbuff2 = real(zrho, sp)
          endif
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'ZRHO', uindex, 1, rsbuff2)
             deallocate( rsbuff2 )
          endif
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'HYDPRES'
       ! only if selhis( 2: 2) <> 'N'
       !
       if (index(selhis(2:2), 'Y')>0 .and. zmodel) then
          if (inode == master) allocate( rsbuff2(1:nostatgl, 1:kmax) )
          if (parll) then
             call dfgather_filter(lundia, nostat, nostatto, nostatgl, 1, kmax, order_sta, hydprs, rsbuff2, gdp)
          else
             rsbuff2 = real(hydprs, sp)
          endif 
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'HYDPRES', uindex, 1, rsbuff2)
             deallocate( rsbuff2 )
          endif
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'XYSTAT'
       !
       if (inode == master) allocate( rsbuff2(2,1:nostatgl) )
       if (parll) then
          call dfgather_filter(lundia, nostat, nostatto, nostatgl, 1, 2, order_sta, xystat, rsbuff2, gdp )
       else
          rsbuff2 = real(xystat, sp)
       endif
       if (inode == master) then
          ierror = putelt(fds, grnam3, 'XYSTAT', uindex, 1, rsbuff2)
          deallocate( rsbuff2 )
       endif
       if (ierror/=0) goto 999
       !
       ! group 3: element 'MNSTAT'
       !
       allocate(ibuff2b(2,nostat))
       do i=1,nostat
          !
          ! mnstat contains indices with respect to this partion
          ! transfer into global indices
          !
          ibuff2b(1,i) = mnstat(1,i) + mfg - 1
          ibuff2b(2,i) = mnstat(2,i) + nfg - 1
       enddo
       if (inode == master) allocate( ibuff2(2,nostatgl) )
       if (parll) then
          call dfgather_filter(lundia, nostat, nostatto, nostatgl, 1, 2, order_sta, ibuff2b, ibuff2, gdp)
       else
          ibuff2 = ibuff2b
       endif 
       deallocate(ibuff2b)

       if (inode == master) then
          ierror = putelt(fds, grnam3, 'MNSTAT', uindex, 1, ibuff2)
          deallocate( ibuff2 )
       endif
       if (ierror/=0) goto 999
       !
       ! group 3: element 'DPS'
       !
       if (inode == master) allocate( rsbuff1(1:nostatgl) )
       if (parll) then
          call dfgather_filter(lundia, nostat, nostatto, nostatgl, order_sta, zdps, rsbuff1, gdp )
       else
          rsbuff1 = real(zdps, sp)
       endif
       if (inode == master) then
          ierror = putelt(fds, grnam3, 'DPS', uindex, 1, rsbuff1)
          deallocate( rsbuff1 )
       endif
       if (ierror/=0) goto 999
    endif
    !
    ! group 3: next 4 depend on NTRUV > 0
    !
    if (ntruvgl > 0) then
       cross_sec = .true.
       !
       ! group 3: element 20 'FLTR' only if SELHIS(20:20) = 'Y'
       !
       if (selhis(20:20)=='Y') then
          if (inode == master) then
             allocate( rsbuff1(1:ntruvgl) )
             rsbuff1 = 0.0
          endif
          if (parll) then  
             call dfgather_filter(lundia, ntruv, ntruvto, ntruvgl, line_orig, fltr, rsbuff1, gdp, cross_sec )
          else
             rsbuff1 = real(fltr, sp) 
          endif
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'FLTR', uindex, 1, rsbuff1)
             deallocate( rsbuff1 )
          endif
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 21 'CTR' only if SELHIS(21:21) = 'Y'
       !
       if (selhis(21:21)=='Y') then
          if (inode == master) then
             allocate( rsbuff1(1:ntruvgl) )
             rsbuff1 = 0.0
          endif
          if (parll) then 
             call dfgather_filter(lundia, ntruv, ntruvto, ntruvgl, line_orig, ctr, rsbuff1, gdp, cross_sec )
          else
             rsbuff1 = real(ctr, sp)
          endif
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'CTR', uindex, 1, rsbuff1)
             deallocate( rsbuff1 )
          endif
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 22 'ATR', only if LSTSCI > 0
       ! (:= SELHIS(22:22) = 'Y')
       !
       if (selhis(22:22)=='Y') then
          if (inode == master) allocate( rsbuff2(1:ntruvgl, 1:lstsci) )
          if (parll) then
             call dfgather_filter(lundia, ntruv, ntruvto, ntruvgl, 1, lstsci, line_orig, atr, rsbuff2, gdp, cross_sec)
          else
             rsbuff2 = real(atr, sp)
          endif
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'ATR', uindex, 1, rsbuff2)
             deallocate( rsbuff2 )
          endif
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 23 'DTR', only if LSTSCI > 0
       ! (:= SELHIS(23:23) = 'Y')
       !
       if (selhis(23:23)=='Y') then
          if (inode == master) allocate( rsbuff2(1:ntruvgl, 1:lstsci) )
          if (parll) then
             call dfgather_filter(lundia, ntruv, ntruvto, ntruvgl, 1, lstsci, line_orig, dtr, rsbuff2, gdp, cross_sec)
          else
             rsbuff2 = real(dtr, sp)
          endif
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'DTR', uindex, 1, rsbuff2)
             deallocate( rsbuff2 )
          endif
          if (ierror/=0) goto 999
       endif
    endif
    !
    if (inode == master) ierror = clsnef(fds)
    !
    ! write errormessage if error occurred and set error = .true.
    ! the files will be closed in clsnef (called in triend)
    !
    !
  999 continue
    if (parll .and. inode == master) then
       call dfcleanup_glbarrs
    endif
    call dfsync(gdp)
    if (ierror/=0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wrthis
