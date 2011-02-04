subroutine flow_nefis_restart(lundia    ,error     ,restid1   ,lturi     ,mmax      , &
                            & nmaxus    ,kmax      ,lstsci    ,ltur      , &
                            & s1        ,u1        ,v1        ,r1        ,rtur1     , &
                            & umnldf    ,vmnldf    ,kfu       ,kfv       , &
                            & dp        ,ex_nfs    ,gdp       )
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
! Reads initial field condition records from an
! NEFIS flow output map file
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use precision
use properties
!
    use globaldata
    !
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)       , pointer :: tstart
    real(fp)       , pointer :: dt
    logical        , pointer :: temp
    logical        , pointer :: const
    logical        , pointer :: htur2d
    integer        , pointer :: i_restart
    logical        , pointer :: dp_from_map_file
    logical        , pointer :: kfuv_from_restart
    logical        , pointer :: rst_dp
    character(256) , pointer :: restid
    real(hp)       , pointer :: morft
    real(hp)       , pointer :: morft0
    real(fp)       , pointer :: bed
!
! Global variables
!
    integer                                                                    , intent(in)  :: kmax
    integer                                                                    , intent(in)  :: lstsci
    integer                                                                    , intent(in)  :: ltur
    integer                                                                    , intent(out) :: lturi
    integer                                                                                  :: lundia
    integer                                                                    , intent(in)  :: mmax
    integer                                                                    , intent(in)  :: nmaxus
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(out) :: kfu
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(out) :: kfv
    logical                                                                                  :: error
    logical                                                                                  :: ex_nfs !  Flag indicating whether Nefis restart files exist
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: dp
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: s1
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: umnldf
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: vmnldf
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, ltur), intent(out) :: rtur1
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(out) :: u1
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(out) :: v1
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci), intent(out) :: r1
    character(*)                                                                             :: restid1
!
! Local variables
!
    integer                               :: lrid        ! character variables for files Help var., length of restid
    integer, external                     :: crenef
    integer, external                     :: getelt
    integer, external                     :: clsnef
    integer                               :: ierror
    integer                               :: fds
    integer, external                     :: inqmxi
    integer, external                     :: neferr
    integer                               :: i
    integer                               :: j
    integer                               :: itmapc
    integer                               :: max_index
    integer                               :: rst_lstci
    integer                               :: rst_ltur
    integer, dimension(3,5)               :: cuindex
    integer, dimension(3,5)               :: uindex
    integer, dimension(:,:,:,:), pointer  :: ibuff
    logical                               :: found
    real(fp)                              :: dtm          ! time step in minutes (flexible precision)
    real(fp)                              :: t_restart
    real(sp)                              :: dtms         ! time step in minutes (single precision)
    real(sp), dimension(:,:,:,:), pointer :: sbuff
    character(1024)                       :: error_string
    character(256)                        :: dat_file
    character(256)                        :: def_file
    integer                     , pointer :: mfg
    integer                     , pointer :: mlg
    integer                     , pointer :: nfg
    integer                     , pointer :: nlg
    integer                     , pointer :: nmaxgl
    integer                     , pointer :: mmaxgl   
    real(sp), dimension(:,:), allocatable :: s1_g
    real(sp), dimension(:,:), allocatable :: dp_g
    real(sp), dimension(:,:,:), allocatable :: u1_g
    real(sp), dimension(:,:,:), allocatable :: v1_g        
    real(sp), dimension(:,:), allocatable :: umnldf_g
    real(sp), dimension(:,:), allocatable :: vmnldf_g
    integer, dimension(:,:), allocatable :: kfu_g
    integer, dimension(:,:), allocatable :: kfv_g        
    real(sp), dimension(:,:,:,:), allocatable :: r1_g
    real(sp), dimension(:,:,:,:), allocatable :: rtur1_g
    integer                                   :: ier1, ier2, ier3, ier4,ier5,ier6,ier7,ier8,ier9
!
!! executable statements -------------------------------------------------------
!
    tstart              => gdp%gdexttim%tstart
    dt                  => gdp%gdexttim%dt
    temp                => gdp%gdprocs%temp
    const               => gdp%gdprocs%const
    htur2d              => gdp%gdprocs%htur2d
    i_restart           => gdp%gdrestart%i_restart
    dp_from_map_file    => gdp%gdrestart%dp_from_map_file
    kfuv_from_restart   => gdp%gdrestart%kfuv_from_restart
    rst_dp              => gdp%gdrestart%rst_dp
    restid              => gdp%gdrestart%restid
    morft               => gdp%gdmorpar%morft
    morft0              => gdp%gdmorpar%morft0
    bed                 => gdp%gdmorpar%bed
    mfg                 => gdp%gdparall%mfg
    mlg                 => gdp%gdparall%mlg
    nfg                 => gdp%gdparall%nfg
    nlg                 => gdp%gdparall%nlg
    mmaxgl              => gdp%gdparall%mmaxgl
    nmaxgl              => gdp%gdparall%nmaxgl    
    
    !
    ! dp_from_map_file=false: do not read depth from map file
    !
    call prop_get_logical(gdp%mdfile_ptr, '*', 'dp_from_map_file', dp_from_map_file)
    restid       = restid1
    nullify(ibuff)
    nullify(sbuff)
    error        = .false.
    error_string = ' '
    call noextspaces(restid    ,lrid      )
    !
    ! open NEFIS trim-<restid> file
    !
    dat_file = restid(1:lrid)//'.dat'
    def_file = restid(1:lrid)//'.def'
    
    if (inode == master) then
       ierror = crenef(fds, dat_file, def_file, ' ', 'r')
       if (ierror/= 0) then
          error = .true.
          goto 9999
       endif
    endif
    ex_nfs = .true.
    write(lundia, '(a)') 'Restarting from ' // trim(dat_file) // ' and ' // trim(def_file)

    !
    ! Now also reading KFU and KFV from restart file
    !
    kfuv_from_restart = .true.
    !
    ! initialize group index time dependent data
    !
    uindex (3,1) = 1 ! increment in time
    uindex (1,1) = 1
    uindex (2,1) = 1
    !
    ! initialize group index constant data
    !
    cuindex (3,1) = 1 ! increment in time
    cuindex (1,1) = 1
    cuindex (2,1) = 1
    !
    
    ! the master opens and reads the grid file 
    ! 
    if ( inode /= master ) goto 50 
    
    ierror = getelt(fds, 'map-const', 'DT', cuindex, 1, 4, dtms)

 

    dtm = dtms
    if (ierror/= 0) then
       ierror = neferr(0,error_string)
       call prterr(lundia    ,'P004'    , error_string)
       error = .true.
       goto 9999
    endif
    ierror = inqmxi(fds, 'map-series', max_index)
    if (ierror/= 0) then
       ierror = neferr(0,error_string)
       call prterr(lundia    ,'P004'    , error_string)
       error = .true.
       goto 9999
    endif
    !
    ! look for restart time on nefis map file
    !
    found = .false.
    do i = max_index,1,-1 ! assume last time on map file has highest probability of being the requested time step
       uindex (1,1) = i
       uindex (2,1) = i
       ierror = getelt(fds, 'map-info-series', 'ITMAPC', uindex, 1, 4, itmapc)
       if (ierror/= 0) then
          ierror = neferr(0,error_string)
          call prterr(lundia    ,'P004'    , error_string)
          error = .true.
          goto 9999
       endif
       t_restart = dtm*itmapc
       if (abs(tstart-t_restart) < 0.5_fp*dtm) then
          write(lundia, '(a,i5,a,e20.4)') 'using field ',i,' associated with time T = ',t_restart
          i_restart = i
          found     = .true.
          exit ! restart time found on map file
       end if
    enddo
    if (.not. found) then
       call prterr(lundia    ,'P004'    , &
            & 'Restart time not found on restart file ' // trim(dat_file))
       error = .true.
       goto 9999
    else
       uindex (1,1) = i_restart
       uindex (2,1) = i_restart
       !
       ! The following parameters use a nmaxus*mmax*kmax*1 buffer:
       ! S1, DPS, U1, V1, UMNLDF, VMNLDF
       !
       allocate(sbuff(nmaxgl, mmaxgl, kmax, 1), stat = ier1)
       allocate(s1_g(nmaxgl,mmaxgl),stat = ier2)
       !
       ! S1
       !
       ierror = getelt( fds , 'map-series', 'S1', uindex, 1, mmaxgl*nmaxgl*4, sbuff )
       if (ierror/= 0) then
          ierror = neferr(0,error_string)
          call prterr(lundia    ,'P004'    , error_string)
          error = .true.
          goto 9999
       endif
       s1_g(1:nmaxgl,1:mmaxgl) = sbuff(1:nmaxgl,1:mmaxgl,1,1)
       !
       ! Only read depth when dp_from_map_file=true
       !
       if (dp_from_map_file) then
          !
          ! DPS
          !
          ierror = getelt( fds , 'map-sed-series', 'DPS', uindex, 1, mmaxgl*nmaxgl*4, sbuff )
          if (ierror/= 0) then
             write(lundia, '(a)') 'No bed level data on restart file available:'
             write(lundia, '(a)') 'using bed level data as prescribed in master definition file.'
          else
             !
             ! The read DPS is placed in array DP
             ! The flag rst_dp is used to set DPSOPT=DP
             ! This ensures that the DP values are copied into DPS in subroutine caldps
             ! Differences may occur when DPU/DPV depend on (the original) DP
             !
             write(lundia, '(a)') 'Bed level data read from restart file.'
             rst_dp = .true.
             allocate(dp_g(nmaxgl,mmaxgl), stat = ier1)
             dp_g(1:nmaxgl,1:mmaxgl) = sbuff(1:nmaxgl,1:mmaxgl,1,1)
             !
             ! Read associated morphological time from map file.
             !
             ierror     = getelt(fds, 'map-infsed-serie', 'MORFT',  uindex, 1, 8, morft0)
             if (ierror/=0) morft0 = 0.0_hp
             morft = morft0
          endif
       endif
       !
       ! U1
       !
       ierror = getelt( fds , 'map-series', 'U1', uindex, 1, mmaxgl*nmaxgl*kmax*4, sbuff )
       if (ierror/= 0) then
          ierror = neferr(0,error_string)
          call prterr(lundia    ,'P004'    , error_string)
          error = .true.
          goto 9999
       endif
       allocate(u1_g(nmaxgl,mmaxgl,kmax), stat = ier1)
       u1_g(1:nmaxgl,1:mmaxgl,1:kmax) = sbuff(1:nmaxgl,1:mmaxgl,1:kmax,1)
       !
       ! V1
       !
       ierror = getelt( fds , 'map-series', 'V1', uindex, 1, mmaxgl*nmaxgl*kmax*4, sbuff )
       if (ierror/= 0) then
          ierror = neferr(0,error_string)
          call prterr(lundia    ,'P004'    , error_string)
          error = .true.
          goto 9999
       endif
       allocate(v1_g(nmaxgl,mmaxgl,kmax), stat = ier2)
       v1_g(1:nmaxgl,1:mmaxgl,1:kmax) = sbuff(1:nmaxgl,1:mmaxgl,1:kmax,1)
       !
       ! UMNLDF: filtered velocity U-component for subgrid viscosity model
       !
       ierror = getelt( fds , 'map-series', 'UMNLDF', uindex, 1, mmaxgl*nmaxgl*4, sbuff)
       if (ierror/= 0) then
          if (htur2d) then
             ierror = neferr(0,error_string)
             call prterr(lundia    ,'U190'    , error_string)
          endif
       else
          allocate(umnldf_g(nmaxgl,mmaxgl), stat = ier3)
          umnldf_g(1:nmaxgl,1:mmaxgl) = sbuff(1:nmaxgl,1:mmaxgl,1,1)
       endif
       !
       ! VMNLDF: filtered velocity V-component for subgrid viscosity model
       !
       ierror = getelt( fds , 'map-series', 'VMNLDF', uindex, 1, mmaxgl*nmaxgl*4, sbuff)
       if (ierror/= 0) then
          if (htur2d) then
             ierror = neferr(0,error_string)
             call prterr(lundia    ,'U190'    , error_string)
          endif
       else
          allocate(vmnldf_g(nmaxgl,mmaxgl), stat = ier4)
          vmnldf_g(1:nmaxgl,1:mmaxgl) = sbuff(1:nmaxgl,1:mmaxgl,1,1)
       endif
       !
       allocate(ibuff(nmaxgl, mmaxgl, 1, 1), stat = ier1)
       !
       ! KFU: current active/inactive status of U point
       !
       ierror = getelt( fds , 'map-series', 'KFU', uindex, 1, mmaxgl*nmaxgl*4, ibuff)
       if (ierror/= 0) then
          ierror = neferr(0,error_string)
          call prterr(lundia    ,'P004'    , error_string)
          error = .true.
          goto 9999
       endif
       allocate(kfu_g(nmaxgl,mmaxgl), stat = ier1)
       kfu_g=0
       kfu_g(1:nmaxgl,1:mmaxgl) = ibuff(1:nmaxgl,1:mmaxgl,1,1)
       !
       ! KFV: current active/inactive status of V point
       !
       ierror = getelt( fds , 'map-series', 'KFV', uindex, 1, mmaxgl*nmaxgl*4, ibuff)
       if (ierror/= 0) then
          ierror = neferr(0,error_string)
          call prterr(lundia    ,'P004'    , error_string)
          error = .true.
          goto 9999
       endif
       allocate(kfv_g(nmaxgl,mmaxgl), stat = ier2)
       kfv_g=0
       kfv_g(1:nmaxgl,1:mmaxgl) = ibuff(1:nmaxgl,1:mmaxgl,1,1)
       !
       ! Constituents sal, temp, constituents
       ! Use nmaxus*mmax*kmax*lstsci buffer
       !

       
       ierror = getelt(fds, 'map-const', 'LSTCI', cuindex, 1, 4, rst_lstci)       
       
       if (lstsci /= 0) then
          if (lstsci == rst_lstci) then
             deallocate(sbuff)
             allocate(sbuff(nmaxgl, mmaxgl, kmax, lstsci), stat = ier3)
             ierror = getelt( fds , 'map-series', 'R1', uindex, 1, mmaxgl*nmaxgl*kmax*rst_lstci*4, sbuff )
             if (ierror/= 0) then
                ierror = neferr(0,error_string)
                call prterr(lundia    ,'P004'    , error_string)
                error = .true.
                goto 9999
             endif
             allocate(r1_g(nmaxgl,mmaxgl, kmax, lstsci), stat = ier4)
             r1_g(1:nmaxgl,1:mmaxgl,1:kmax,1:lstsci) = sbuff(1:nmaxgl,1:mmaxgl,1:kmax,1:lstsci)
          else        
             write(lundia, *) 'No restart value used for Salinity, Temperature, a Constituent or Spiral Intensity'
          endif
       endif
       !
       ! Turbulence
       ! Use nmaxus*mmax*0:kmax*ltur buffer
       !
       ierror = getelt(fds, 'map-const', 'LTUR', cuindex, 1, 4, rst_ltur)
       if (ierror/= 0) then
          ierror = neferr(0,error_string)
          call prterr(lundia    ,'P004'    , error_string)
          error = .true.
          goto 9999
       endif
       lturi = 0
       if (ltur > 0) then
          if (ltur == rst_ltur) then
             deallocate(sbuff)
             allocate(sbuff(nmaxgl, mmaxgl, 0:kmax, ltur), stat = ier1)
             ierror = getelt( fds , 'map-series', 'RTUR1', uindex, 1, mmaxgl*nmaxgl*(kmax+1)*rst_ltur*4, sbuff )
             if (ierror/= 0) then
                ierror = neferr(0,error_string)
                call prterr(lundia    ,'P004'    , error_string)
                error = .true.
                goto 9999
             endif
             allocate(rtur1_g(nmaxgl,mmaxgl, 0:kmax, 1:ltur), stat = ier2)
             rtur1_g(1:nmaxgl,1:mmaxgl,0:kmax,1:ltur) = sbuff(1:nmaxgl,1:mmaxgl,0:kmax,1:ltur)
          else        
             write(lundia, *) 'Turbulence model is not compatible with previous simulation, default initialisation will be used'
          endif
       endif

    endif

    !    end of master part
    ! 
    ! scatter information to all nodes     
50 continue    
    !
    ! first scatter ltur1, r1 information to all domains!
    !
    call dfbroadc ( rst_lstci, 1, dfint, gdp )    
    call dfbroadc ( rst_ltur, 1, dfint, gdp )   
       
    if ( inode /= master ) then 
       allocate (s1_g(nmaxgl,mmaxgl),stat=ier1) 
       if (dp_from_map_file) allocate (dp_g(nmaxgl,mmaxgl))
       allocate (u1_g(nmaxgl,mmaxgl,kmax), stat = ier2) 
       allocate (v1_g(nmaxgl,mmaxgl,kmax), stat = ier3) 
       allocate (umnldf_g(nmaxgl,mmaxgl), stat = ier4) 
       allocate (vmnldf_g(nmaxgl,mmaxgl), stat = ier5)
       allocate (kfu_g(nmaxgl,mmaxgl), stat = ier6) 
       allocate (kfv_g(nmaxgl,mmaxgl), stat = ier7)
       if (lstsci > 0 .and. lstsci == rst_lstci) allocate (r1_g(nmaxgl,mmaxgl,kmax,1:lstsci), stat = ier8)
       if (ltur > 0 .and. ltur == rst_ltur)  allocate (rtur1_g(nmaxgl,mmaxgl,0:kmax,1:ltur), stat = ier9)              
        
    endif 
    ! 
    ! scatter arrays s1 etc to all nodes. Note: the broadc must use 'dfreal'
    ! since the arrays are single precision! Otherwise, intractable memory errors will occur. 
    ! 
    call dfsync(gdp)
      
    call dfbroadc ( s1_g, nmaxgl*mmaxgl, dfreal, gdp )   
    if (dp_from_map_file) call dfbroadc ( dp_g, nmaxgl*mmaxgl, dfreal, gdp ) 
    call dfbroadc ( u1_g, nmaxgl*mmaxgl*kmax, dfreal, gdp )
    call dfbroadc ( v1_g, nmaxgl*mmaxgl*kmax, dfreal, gdp )
    call dfbroadc ( umnldf_g, nmaxgl*mmaxgl, dfreal, gdp )
    call dfbroadc ( vmnldf_g, nmaxgl*mmaxgl, dfreal, gdp )
    call dfbroadc ( kfu_g, nmaxgl*mmaxgl, dfint, gdp )
    call dfbroadc ( kfv_g, nmaxgl*mmaxgl, dfint, gdp )
    if (lstsci > 0 .and. lstsci  == rst_lstci) call dfbroadc ( r1_g, nmaxgl*mmaxgl*kmax*lstsci, dfreal, gdp )
    if (ltur > 0  .and. ltur == rst_ltur )  call dfbroadc ( rtur1_g, nmaxgl*mmaxgl*(kmax+1)*ltur, dfreal, gdp )    
    !
    !
    ! 
    ! put copies of parts of s1 etc for each subdomain 
    ! 
    call dfsync ( gdp ) 
    do j = mfg, mlg 
       do i = nfg, nlg 
          s1(i-nfg+1,j-mfg+1) = s1_g(i,j) 
          u1(i-nfg+1,j-mfg+1,1:kmax) = u1_g(i,j,1:kmax)
          v1(i-nfg+1,j-mfg+1,1:kmax) = v1_g(i,j,1:kmax)           
          umnldf(i-nfg+1,j-mfg+1) = umnldf_g(i,j)
          vmnldf(i-nfg+1,j-mfg+1) = vmnldf_g(i,j)
          kfu(i-nfg+1,j-mfg+1) = kfu_g(i,j)
          kfv(i-nfg+1,j-mfg+1) = kfv_g(i,j)
       enddo 
    enddo 
        
    if (dp_from_map_file) then
       do j = mfg, mlg 
          do i = nfg, nlg 
             dp(i-nfg+1,j-mfg+1) = dp_g(i,j)          
          enddo
       enddo    
    endif

    if (lstsci > 0 .and. lstsci  == rst_lstci) then
       do j = mfg, mlg 
          do i = nfg, nlg 
             r1(i-nfg+1,j-mfg+1,1:kmax,1:lstsci) = r1_g(i,j,1:kmax,1:lstsci)          
          enddo
       enddo    
    endif 
       
    if (ltur > 0  .and. ltur == rst_ltur ) then
       do j = mfg, mlg 
          do i = nfg, nlg 
             rtur1(i-nfg+1,j-mfg+1,0:kmax,1:ltur) = rtur1_g(i,j,0:kmax,1:ltur)          
          enddo
       enddo   
    endif       

    deallocate(u1_g, v1_g, stat = ier2)
    deallocate(s1_g, stat = ier1)
    deallocate(umnldf_g, vmnldf_g, stat = ier3)
    deallocate(kfu_g, kfv_g, stat=ier4)
    if (dp_from_map_file) deallocate(dp_g, stat = ier1)
    if (lstsci > 0 .and. lstsci  == rst_lstci) deallocate(r1_g, stat = ier2)
    if (ltur > 0  .and. ltur == rst_ltur ) deallocate(rtur1_g, stat = ier3)

                
9999 continue

    
    if (inode == master) then
      if (associated(sbuff)) deallocate (sbuff)
      if (associated(ibuff)) deallocate (ibuff)
      ierror = clsnef(fds) 
    endif
    
    call dfsync(gdp)
      
end subroutine flow_nefis_restart
