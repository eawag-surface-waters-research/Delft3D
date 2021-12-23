!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2021.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! $Id$
! $HeadURL$

 !> Initializes the entire current model (geometry, boundaries, initial state)
 !! @return Error status: error (/=0) or not (0)
 integer function flow_modelinit() result(iresult)                     ! initialise flowmodel
 use timers
 use m_flowgeom,    only: jaFlowNetChanged, ndx, lnx, kfs, ndxi
 use waq,           only: reset_waq
 use m_flow,        only: zws, zws0, kmx, jasecflow, lnkx
 use m_flowtimes
 use m_wind, only: numlatsg
 use network_data,  only: netstat, NETSTAT_CELLS_DIRTY
 use gridoperations, only: make1D2Dinternalnetlinks
 use m_partitioninfo
 use m_timer
 use m_flowtimes
 use unstruc_model ! , only: md_ident, md_restartfile,  writeMDUFilepointer, md_foufile, md_flowgeomfile, md_snapshotdir, md_numthreads
 use unstruc_files, only: mdia
 use unstruc_netcdf
 use MessageHandling
 use m_flowparameters, only: jawave, jatrt, jacali, jacreep, jatransportmodule, jamd1dfile, flowWithoutWaves
 use dfm_error
 use m_fm_wq_processes, only: jawaqproc
 use m_vegetation
 use m_hydrology, only: jadhyd, init_hydrology
 use m_integralstats
 use m_xbeach_data, only: instat, newstatbc, bccreated
 use m_oned_functions
 use unstruc_display, only : ntek, jaGUI
 use m_alloc
 use m_bedform
 use m_fm_update_crosssections, only: fm_update_mor_width_area, fm_update_mor_width_mean_bedlevel
 use unstruc_netcdf_map_class
 use unstruc_caching
 use m_monitoring_crosssections, only: ncrs, fill_geometry_arrays_crs

 !use m_mormerge
 !
 ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
 ! Activate the following line (See also statements below)
 !use ifcore
 !
 implicit none

 integer              :: jw, istat, L, ierr
 integer, external    :: flow_flowinit
 integer, external    :: init_openmp
 !
 ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
 ! Activate the following 3 lines, See also statements below
 !INTEGER*4 OLD_FPE_FLAGS, NEW_FPE_FLAGS
 !NEW_FPE_FLAGS = FPE_M_TRAP_OVF + FPE_M_TRAP_DIV0 + FPE_M_TRAP_INV
 !OLD_FPE_FLAGS = FOR_SET_FPE (NEW_FPE_FLAGS)
 !

 iresult = DFM_GENERICERROR

 call datum2(rundat2)
 L = len_trim(rundat2)

 IF (ti_waq > 0d0) then
    call makedir(getoutputdir('waq'))  ! No problem if it exists already.
 end if

  call timstrt('Basic init', handle_extra(1)) ! Basic steps

 md_snapshotdir =  trim(getoutputdir())                  ! plot output to outputdir
 ! Make sure output dir for plot files exists
 if (len_trim(md_snapshotdir) > 0) then
    call makedir(md_snapshotdir) ! No problem if it exists already.
 end if

 if ( jatimer.eq.1 ) then
    call initimer()
 end if

 call unc_set_ncformat(md_ncformat)

 call reset_unstruc_netcdf_map_class()

 call resetflow()

 call reset_waq()

 call timstop(handle_extra(1)) ! End basic steps

! JRE
 call timstrt('Xbeach input init', handle_extra(2)) ! Wave input
 if (jawave == 4) then
    bccreated = .false.       ! for reinit
    call xbeach_wave_input()  ! will set swave and lwave
 endif
 call timstop(handle_extra(2)) ! End wave input


 call timstrt('Make internal links      ', handle_extra(3)) ! Internal links
 if (md_jamake1d2dlinks == 1) then
    ierr = make1D2Dinternalnetlinks()
     if (ierr /= DFM_NOERR) then
      call mess(LEVEL_WARN,'Error, failed to create 1D2D links.')
      goto 1234
    end if
 end if
 call timstop(handle_extra(3)) ! End internal links

 ! TODO: unc_wri_map_header

 call timstrt('Flow geometry       ', handle_extra(4)) ! Flow geometry
 call mess(LEVEL_INFO,'Initializing flow model geometry...')
 if ( jampi.eq.0 ) then
    call flow_geominit(0)                                ! initialise flow geometry based upon present network, time independent
                                                         ! make directional wave grid

    call mess(LEVEL_INFO,'Done initializing flow model geometry.')

    if (ndx == 0) then
      call mess(LEVEL_WARN,'ndx == 0, please check MDU-file')
      iresult = DFM_MODELNOTINITIALIZED
      goto 1234
    end if
 else
    call flow_geominit(1)  ! first phase only

    if ( Ndx.gt.0 ) then
       call mess(LEVEL_INFO,'Start partitioning model...')
       if ( jatimer.eq.1 ) call starttimer(IPARTINIT)

       call partition_init_1D2D(md_ident, iresult)   ! 1D & 2D (hence the name, thanks to Herman for pointing this out)

       if ( jatimer.eq.1 ) call stoptimer(IPARTINIT)
       call mess(LEVEL_INFO,'Done partitioning model.')

       if ( iresult.eq.0 ) then
          call update_geom(1)     ! update geometry in ghost area

          call flow_geominit(2)   ! second phase
          call update_geom(2)     ! update geometry in ghost area

          call disable_invalid_ghostcells_with_wu()  ! disable ghost cells that are not being synchronised by setting wu's to zero

          call mess(LEVEL_INFO,'Done initializing flow model geometry.')
       else
         call mess(LEVEL_WARN,'Error in 2D partitioning initialization.')
         goto 1234
       end if
    else
       call mess(LEVEL_WARN,'No network, please check MDU-file')
       iresult = DFM_MODELNOTINITIALIZED
       goto 1234
    end if
 end if
 call timstop(handle_extra(4)) ! End flow geometry


 if( kmx > 0 .and. jasecflow > 0) then         ! An error announcement (or warning, with correction to jasecflow to 0)
    jasecflow = 0
    call mess(LEVEL_WARN,'Warning: Secondary Flow is not applicable in 3D computation !!')
    call mess(LEVEL_WARN,'         Secondary flow is turned off')
 endif

 call timstrt('Bobsongullies       ', handle_extra(5)) ! bobsongullies
 call setbobsongullies()
 call timstop(handle_extra(5)) ! End bobsongullies

 if (javeg > 0) then
    ! NOTE: AvD: hardcoded for now: if vegetation is on, maintain max shear stresses for Peter and Jasper.
    is_numndvals = 3
 end if

 ! 3D: flow_allocflow will set kmxn, kmxL and kmxc arrays
 call timstrt('Flow allocate arrays          ', handle_extra(37)) ! alloc flow
 call flow_allocflow()                               ! allocate   flow arrays
 call timstop(handle_extra(37)) ! end alloc flow
 !
 if (jawave > 0 .and. .not. flowWithoutWaves) then
    call alloc9basicwavearrays()
 endif
 if (jawave > 2 .or. (jased > 0 .and. stm_included)) then
    call flow_waveinit()
 endif
 ! Construct a default griddim struct for D3D subroutines, i.e. sedmor or trachytopes
 call timstrt('Flow grid           ', handle_extra(7)) ! Flow griddim
 if ( len_trim(md_sedfile) > 0 .or. jatrt == 1) then
    call D3Dflow_dimensioninit()
 endif
 call timstop(handle_extra(7)) ! End flow griddim

 call timstrt('Bed forms init (1)  ', handle_extra(8)) ! Bed forms
 if ((jased > 0 .and. stm_included) .or. bfm_included .or. jatrt > 0 ) then
    call flow_bedforminit(1)        ! bedforms stage 1: datastructure init
 endif
 call timstop(handle_extra(8)) ! End bed forms

 !! flow1d -> dflowfm initialization
 call timstrt('1D roughness        ', handle_extra(9)) ! 1d roughness
 call set_1d_roughnesses()
 call timstop(handle_extra(9)) ! End 1d roughness

 ! need number of fractions for allocation of sed array
  call timstrt('Sedimentation Morphology init', handle_extra(10)) ! sedmor
 if ( len_trim(md_sedfile) > 0 ) then
      call flow_sedmorinit ()
 endif
 call timstop(handle_extra(10)) ! End sedmor

 call timstrt('Bed forms init (2)  ', handle_extra(11)) ! bedform
 if ((jased > 0 .and. stm_included) .or. bfm_included ) then
    call flow_bedforminit(2)        ! bedforms  stage 2: parameter read and process
 endif
 call timstop(handle_extra(11)) ! End bedform

 call timstrt('Vertical administration', handle_extra(12)) ! vertical administration
 if (jampi == 1) then
!   update vertical administration
    call update_vertadmin()

    !3D: partition_init needs kmxn and kmxL arrays for 3D send- and ghostlists
    if ( jatimer.eq.1 ) call starttimer(IPARTINIT)
    call partition_init_3D(iresult)
    if ( jatimer.eq.1 ) call stoptimer(IPARTINIT)

    if (iresult /= DFM_NOERR) then
      call mess(LEVEL_WARN,'Error in 3D partitioning initialization.')
      goto 1234
    end if

 end if
 call timstop(handle_extra(12)) ! vertical administration

 ierr = init_openmp(md_numthreads, jampi)
 
 call timstrt('Net link tree 0     ', handle_extra(13)) ! netlink tree 0
 if ((jatrt == 1) .or. (jacali == 1)) then
     call netlink_tree(0)
 endif
 call timstop(handle_extra(13)) ! end netlink tree

 call timstrt('Initialise trachytopes', handle_extra(14)) ! flow trachy init
 if (jatrt == 1) then
    call flow_trachyinit ()                          ! initialise the trachytopes module
 end if
 call timstop(handle_extra(14)) ! end flow trachy init

 call timstrt('Initialise Calibration', handle_extra(15)) ! calibration init
 if (jacali == 1) then
     call calibration_init()                          ! initialise the calibration memory structures and read .cld and .cll files
 end if
 call timstop(handle_extra(15)) ! end calibration init

 call timstrt('Net link tree 1     ', handle_extra(16)) ! netlink tree 1
 if ((jatrt == 1) .or. (jacali == 1)) then
     call netlink_tree(1)
 endif
 call timstop(handle_extra(16)) ! netlink tree 1

 !! flow1d -> dflowfm update
 call timstrt('Save 1d             ', handle_extra(17)) ! save 1d
 if (stm_included) then
    call save_1d_nrd_vars_in_stm()
 end if
 call timstop(handle_extra(17)) ! end save 1d

! initialize waq and add to tracer administration
 call timstrt('WAQ processes init  ', handle_extra(18)) ! waq processes init
 if ( len_trim(md_subfile) > 0 ) then
    call fm_wq_processes_ini_sub()
 end if
 call timstop(handle_extra(18)) ! end waq processes init

 call timstrt('Transport init      ', handle_extra(19)) ! transport module
 call ini_transport()
 call timstop(handle_extra(19)) ! end transport module

! initialize part
 call timstrt('Part init           ', handle_extra(20)) ! part init
 call ini_part(1, md_partfile, md_partrelfile, md_partjatracer, md_partstarttime, md_parttimestep, md_part3Dtype)
 call timstop(handle_extra(20)) ! end part init

 call timstrt('Observations init   ', handle_extra(21)) ! observations init
 call flow_obsinit()                                 ! initialise stations and cross sections on flow grid + structure his (1st call required for call to flow_trachy_update)
 if (ncrs > 0) then
    call fill_geometry_arrays_crs()
 end if
 call timstop(handle_extra(21)) ! end observations init

 call timstrt('Flow init           ', handle_extra(23)) ! flow init
 iresult = flow_flowinit()                           ! initialise flow arrays and time dependent params for a given user time
 if (iresult /= DFM_NOERR) then
    goto 1234
 end if
 call timstop(handle_extra(23)) ! end flow init

 if (jadhyd == 1) then
    call init_hydrology()                          ! initialise the hydrology module (after flow_flowinit())
 end if

 if (numlatsg > 0) then
    call init_lateral_his()
    call fill_geometry_arrays_lateral()
 end if

 ! initialize waq and add to tracer administration
 call timstrt('WAQ processes init  ', handle_extra(18)) ! waq processes init
 if (ti_waqproc /= 0d0) then
    if ( jawaqproc .eq. 1 ) then
       call fm_wq_processes_ini_proc()
       jawaqproc = 2
       call fm_wq_processes_step(ti_waqproc,tstart_user)
    endif
 endif
 call timstop(handle_extra(18)) ! end waq processes init

 call timstrt('MBA init            ', handle_extra(24)) ! MBA init
 if (ti_mba > 0) then
    call mba_init()
 endif
 call timstop(handle_extra(24)) ! end MBA init

 call timstrt('Update MOR width    ', handle_extra(25)) ! update MOR width and mean bed level
 if (stm_included) then
     call fm_update_mor_width_area()
     call fm_update_mor_width_mean_bedlevel()
 endif
 call timstop(handle_extra(25)) ! end update MOR width

 call timstrt('Dredging init       ', handle_extra(26)) ! dredging init
 if ( len_trim(md_dredgefile) > 0 .and. stm_included) then
    call flow_dredgeinit()          ! dredging and dumping. Moved here because julrefdate needed
 endif
 call timstop(handle_extra(26)) ! end dredging init

 call timstrt('Xbeach init         ', handle_extra(27)) ! Xbeach init
 if (jawave .eq. 4) then
    call xbeach_wave_init()

    if ( trim(instat).eq.'stat' .or. trim(instat)=='stat_table') then   ! for stationary solver: initialize with stationary field
       !call xbeach_stationary()
       call xbeach_solve_wave_stationary(iresult)
       newstatbc   = 0
       if ( jaGUI.eq.1 ) then                                          ! this part is for online visualisation
          if (ntek > 0) then
             if (mod(int(dnt_user),ntek) .eq. 0) then
                call wave_makeplotvars()                                ! Potentially only at ntek interval
             end if
          endif
       endif
    end if
 end if
 call timstop(handle_extra(27)) ! end Xbeach init

 call timstrt('Observations init 2 ', handle_extra(28)) ! observations init 2
 call flow_obsinit()                                 ! initialise stations and cross sections on flow grid + structure his (2nd time required to fill values in observation stations)
 call timstop(handle_extra(28)) ! end observations init 2

 call timstrt('Structure parameters', handle_extra(29)) ! structure parameters
 call structure_parameters()                         ! initialize structure values, after flow_flowinit() so that initial water levels and discharges are already set.
 call timstop(handle_extra(29)) ! end structure parameters

 call timstrt('Trachy update       ', handle_extra(30)) ! trachy update
 if (jatrt == 1) then
    call flow_trachyupdate()                         ! Perform a trachy update step to correctly set initial field quantities
 endif                                               ! Generally flow_trachyupdate() is called from flow_setexternalforcings()
 call timstop(handle_extra(30)) ! end trachy update

 call timstrt('Set friction values for MOR        ', handle_extra(31)) ! set fcru mor
 if ((jased>0) .and. stm_included) then
    if (jamd1dfile == 0) then
       call set_frcu_mor(1)        !otherwise frcu_mor is set in getprof_1d()
    endif
    call set_frcu_mor(2)
 endif
 call timstop(handle_extra(31)) ! end set fcru mor

 call flow_initimestep(1, iresult)                   ! 1 also sets zws0
 
 jaFlowNetChanged = 0


 ! Initialise Fourier Analysis
 call timstrt('Fourier init        ', handle_extra(33)) ! Fourier init
 if (len_trim(md_foufile)>0) then
    call flow_fourierinit()
 endif
 call timstop(handle_extra(33)) ! end Fourier init

 
 call timstrt('MDU file pointer    ', handle_extra(34)) ! writeMDUFilepointer
 call mess(LEVEL_INFO, '** Model initialization was successful **')
 call mess(LEVEL_INFO, '* Active Model definition:')! Print model settings in diagnostics file.
 call writeMDUFilepointer(mdia, .true., istat)

 call mess(LEVEL_INFO, '**')
 call timstop(handle_extra(34)) ! end writeMDUFilepointer

 call timstrt('Flowgeom            ', handle_extra(35)) ! write flowgeom ugrid
 if (len_trim(md_flowgeomfile) > 0) then             ! Save initial flow geometry to file.
    if (md_unc_conv == UNC_CONV_UGRID) then
       call unc_write_net_flowgeom_ugrid(trim(md_flowgeomfile)) ! UGRID
    else
       call unc_write_net_flowgeom(trim(md_flowgeomfile)) ! CFOLD
    end if
 end if
 call timstop(handle_extra(35)) ! end write flowgeom ugrid

 ! store the grid-based information in the cache file
 call timstrt('Remainder           ', handle_extra(36)) ! remainder
 call storeCachingFile(md_ident, md_usecaching)

 call timstop(handle_extra(36)) ! End remainder
 call writesomeinitialoutput()

 iresult = DFM_NOERR

 
 return
1234 continue
!  BEGIN DEBUG
   !call dum_makesal()
   !call dum_makeflowfield()
!  END DEBUG


end function flow_modelinit
