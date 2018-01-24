!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2018.                                
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

! $Id: xbeach_netcdf.f90 54191 2018-01-22 18:57:53Z dam_ar $
! $HeadURL: https://repos.deltares.nl/repos/ds/trunk/additional/unstruc/src/xbeach_netcdf.f90 $
module m_xbeach_netcdf
!! xbeach time-averaged spatial output
!! to do for the future: add flexibility to add variables using mnemonics
   use io_ugrid
   use netcdf
   implicit none
   
   type t_unc_wavids
   !
   ! Toplevel
   !
   integer            :: ncid = 0 !< NetCDF data set id (typically NetCDF file pointer)
   type(t_ug_mesh) :: meshids1d
   type(t_ug_mesh) :: meshids2d
   type(t_ug_mesh) :: meshids3d

   !
   ! Dimensions
   !
   integer :: id_timedim = -1 !< Time dimension (the only nf90_unlimited in file).
   !
   ! Data variables
   !
   integer :: id_flowelemba(3)     = -1 !< Variable ID for flow node bottom area (on 1D, 2D, 3D grid parts resp.).
   integer :: id_flowelembl(3)     = -1 !< Variable ID for flow node bed level (on 1D, 2D, 3D grid parts resp.).
   integer :: id_netnodez(3)       = -1 !< Variable ID for net node bed level. TODO: AvD: UNST-1318: consider removing here.
   integer :: id_time      = -1 !< Variable ID for 

   !
   ! Other
   !
   integer :: idx_curtime  = 0  !< Index of current time (typically of latest snapshot being written).
end type t_unc_wavids

contains

subroutine xbeach_write_stats(tim)
   use m_flowparameters, only: jawave, jaavgwavquant, eps10
   use m_flowtimes, only: ti_wav, ti_wavs, ti_wave, tstop_user, time_wav   
   use precision_basics
   
   implicit none
   
   double precision, intent(in)      :: tim
   integer                           :: ierr
   
   ierr = 1
   if ((jawave.eq.4) .and. (ti_wav > 0) .and. (jaavgwavquant .eq. 1)) then
      if (comparereal(tim, time_wav, eps10) >= 0) then
         call unc_write_wav(tim)
         call xbeach_clearaverages()
         if (ti_wav > 0) then
             time_wav = max(ti_wavs + (floor((tim-ti_wavs)/ti_wav)+1)*ti_wav,ti_wavs)
         else
             time_wav = tstop_user
         endif
         if (comparereal(time_wav, ti_wave, eps10) == 1) then
             time_wav = tstop_user
         endif
      endif
   end if
   
   ierr = 0
   
1234 continue
   return
end subroutine

subroutine unc_write_wav(tim)
    use m_flow
    use m_flowtimes
    use unstruc_netcdf
    use unstruc_model
    use unstruc_files , only: defaultFilename
    implicit none

    double precision, intent(in) :: tim

    type(t_unc_wavids), save :: wavids
    integer           , save :: iwavfile = 0
    integer                  :: ierr
    character(len=256)       :: filnam

    if (iwavfile /= 0 .and. it_wav == 0) then
    !if ( md_mapformat.eq.IFORMAT_NETCDF .or. md_mapformat == IFORMAT_UGRID) then   !   NetCDF output
       !if (wavids%ncid /= 0 .and. ((md_unc_conv == UNC_CONV_UGRID .and. wavids%idx_curtime == 0) .or. (md_unc_conv == UNC_CONV_CFOLD .and. it_wav == 0))) then
          ierr = unc_close(iwavfile)
          iwavfile = 0
       !end if
    end if

    if (iwavfile == 0) then
        filnam = defaultFilename('avgwavquant')
        ierr = unc_create(filnam , 0, iwavfile)
        if (ierr /= nf90_noerr) then
            call mess(LEVEL_WARN, 'Could not create file containing time-averaged wave output.')
            iwavfile = 0
        end if
    endif

    if (iwavfile .ne. 0) then
        call unc_write_wav_filepointer(iwavfile,tim)
        !if (md_unc_conv == UNC_CONV_UGRID) then
           !call unc_write_wav_filepointer_ugrid(wavids,tim)
        !else
           !call unc_write_map_filepointer(wavids%ncid,tim)
        !endif
    endif

    ierr = nf90_sync(iwavfile) ! Flush file

end subroutine unc_write_wav

!> Writes time-averaged spatial wave output to an already opened netCDF dataset.
subroutine unc_write_wav_filepointer(imapfile, tim,  jaseparate)
    use m_flow
    use m_flowtimes
    use m_flowgeom
    use m_sferic
    use network_data
    use unstruc_netcdf
	 use m_xbeach_avgoutput

    implicit none

    integer,           intent(in) :: imapfile
    real(kind=hp),     intent(in) :: tim

    integer                       :: idims(2)
    logical, save                 :: firststep  = .true.
    
    integer, save :: ierr, ndim, &
                     id_flowelemdim, &
                     id_flowlinkdim, &
                     id_timedim,     &
                     id_time, &
                     id_H_mean, id_H_var, id_H_min, id_H_max, &
                     id_E_mean, id_E_var, id_E_min, id_E_max, &
                     id_R_mean, id_R_var, id_R_min, id_R_max, &
                     id_D_mean, id_D_var, id_D_min, id_D_max, &
                     id_Fx_mean, id_Fx_var, id_Fx_min, id_Fx_max, &
                     id_Fy_mean, id_Fy_var, id_Fy_min, id_Fy_max, &                     
                     id_DR_mean, id_DR_var, id_DR_min, id_DR_max, &
                     id_s1_mean, id_s1_var, id_s1_min, id_s1_max, &
                     id_u_mean, id_u_var, id_u_min, id_u_max, &
                     id_v_mean, id_v_var, id_v_min, id_v_max, &
                     id_cwav_mean, id_cwav_var, id_cwav_min, id_cwav_max, &
                     id_cgwav_mean, id_cgwav_var, id_cgwav_min, id_cgwav_max, &
                     id_urms_mean, id_urms_var, id_urms_min, id_urms_max, &
                     id_ust_mean, id_ust_var, id_ust_min, id_ust_max, &
                     id_vst_mean, id_vst_var, id_vst_min, id_vst_max, &
                     id_thetamean_mean, id_thetamean_var, id_thetamean_min, id_thetamean_max, &
                     id_sigmwav_mean, id_sigmwav_var, id_sigmwav_min, id_sigmwav_max

    integer                :: itim, k
    integer,optional       :: jaseparate
    
    double precision, allocatable    :: temp(:)
    allocate(temp(1:lnx), stat=ierr)

    ! Use nr of dimensions in netCDF file a quick check whether vardefs were written
    ! before in previous calls.
    ndim = 0
    ierr = nf90_inquire(imapfile, nDimensions=ndim)

    ! Only write net and flow geometry data the first time, or for a separate map file.
    if (ndim == 0) then
       call unc_write_flowgeom_filepointer(imapfile) ! UNC_CONV_CFOLD ! Write time-independent flow geometry data
       
       ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim)
       ierr = nf90_inq_dimid(imapfile, 'nFlowLink', id_flowlinkdim)
       
       ! Time
       ierr = nf90_def_dim(imapfile, 'time', nf90_unlimited, id_timedim)
       call check_error(ierr, 'def time dim')
       ierr = nf90_def_var(imapfile, 'time', nf90_double, id_timedim,  id_time)
       ierr = nf90_put_att(imapfile, id_time,  'units'        , 'seconds since '//refdat(1:4)//'-'//refdat(5:6)//'-'//refdat(7:8)//' 00:00:00')
       ierr = nf90_put_att(imapfile, id_time,  'standard_name', 'time') 
              
       ! Shortcut 1
       idims(1) = id_flowelemdim 
       idims(2) = id_timedim 
       
       ! Flow data on centres
       call definencvar(imapfile,id_H_mean  ,nf90_double,idims,2, 'H_mean'  , 'mean rms wave height', 'm', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_H_var   ,nf90_double,idims,2, 'H_var'  , 'variance rms wave height', 'm2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_H_min   ,nf90_double,idims,2, 'H_min'  , 'min rms wave height', 'm', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_H_max   ,nf90_double,idims,2, 'H_max'  , 'max rms wave height', 'm', 'FlowElem_xcc FlowElem_ycc')
       
       call definencvar(imapfile,id_E_mean  ,nf90_double,idims,2, 'E_mean'  , 'mean bulk wave energy', 'J m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_E_var   ,nf90_double,idims,2, 'E_var'  , 'variance bulk wave energy', 'J2 m-4', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_E_min   ,nf90_double,idims,2, 'E_min'  , 'min bulk wave energy', 'J m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_E_max   ,nf90_double,idims,2, 'E_max'  , 'max bulk wave energy', 'J m-2', 'FlowElem_xcc FlowElem_ycc')
       
       call definencvar(imapfile,id_R_mean  ,nf90_double,idims,2, 'R_mean'  , 'mean roller energy', 'J m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_R_var   ,nf90_double,idims,2, 'R_var'  , 'variance roller energy', 'J2 m-4', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_R_min   ,nf90_double,idims,2, 'R_min'  , 'min roller energy', 'J m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_R_max   ,nf90_double,idims,2, 'R_max'  , 'max roller energy', 'J m-2', 'FlowElem_xcc FlowElem_ycc')
              
       call definencvar(imapfile,id_D_mean  ,nf90_double,idims,2, 'D_mean'  , 'mean wave breaking dissipation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_D_var   ,nf90_double,idims,2, 'D_var'  , 'variance wave breaking dissipation', 'W2 m-4', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_D_min   ,nf90_double,idims,2, 'D_min'  , 'min wave breaking dissipation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_D_max   ,nf90_double,idims,2, 'D_max'  , 'max wave breaking dissipation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
              
       call definencvar(imapfile,id_DR_mean  ,nf90_double,idims,2, 'DR_mean'  , 'mean roller energy dissipation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_DR_var   ,nf90_double,idims,2, 'DR_var'  , 'variance roller energy dissipation', 'W2 m-4', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_DR_min   ,nf90_double,idims,2, 'DR_min'  , 'min roller energy dissipation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_DR_max   ,nf90_double,idims,2, 'DR_max'  , 'max roller energy dissipation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
       
       call definencvar(imapfile,id_cwav_mean  ,nf90_double,idims,2, 'cwav_mean'  , 'mean wave phase velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_cwav_var   ,nf90_double,idims,2, 'cwav_var'  , 'variance wave phase velocity', 'm2 s-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_cwav_min   ,nf90_double,idims,2, 'cwav_min'  , 'min wave phase velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_cwav_max   ,nf90_double,idims,2, 'cwav_max'  , 'max wave phase velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
              
       call definencvar(imapfile,id_cgwav_mean  ,nf90_double,idims,2, 'cgwav_mean'  , 'mean wave group velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_cgwav_var   ,nf90_double,idims,2, 'cgwav_var'  , 'variance wave group velocity', 'm2 s-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_cgwav_min   ,nf90_double,idims,2, 'cgwav_min'  , 'min wave group velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_cgwav_max   ,nf90_double,idims,2, 'cgwav_max'  , 'max wave group velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')

       call definencvar(imapfile,id_s1_mean  ,nf90_double,idims,2, 's1_mean'  , 'mean water level on present timestep', 'm', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_s1_var   ,nf90_double,idims,2, 's1_var'  , 'variance water level on present timestep', 'm2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_s1_min   ,nf90_double,idims,2, 's1_min'  , 'min water level on present timestep', 'm', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_s1_max   ,nf90_double,idims,2, 's1_max'  , 'max water level on present timestep', 'm', 'FlowElem_xcc FlowElem_ycc')
       
       call definencvar(imapfile,id_sigmwav_mean  ,nf90_double,idims,2, 'sigmwav_mean'  , 'mean of mean frequency', 'rad s-1', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_sigmwav_var   ,nf90_double,idims,2, 'sigmwav_var'  , 'variance mean frequency', 'rad2 s-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_sigmwav_min   ,nf90_double,idims,2, 'sigmwav_min'  , 'min mean frequency', 'rad s-1', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_sigmwav_max   ,nf90_double,idims,2, 'sigmwav_max'  , 'max mean frequency', 'rad s-1', 'FlowElem_xcc FlowElem_ycc')
       
       call definencvar(imapfile,id_thetamean_mean  ,nf90_double,idims,2, 'thetamean_mean'  , 'mean of mean wave angle', 'rad', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_thetamean_var   ,nf90_double,idims,2, 'thetamean_var'  , 'variance mean wave angle', 'rad2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_thetamean_min   ,nf90_double,idims,2, 'thetamean_min'  , 'min mean wave angle', 'rad', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_thetamean_max   ,nf90_double,idims,2, 'thetamean_max'  , 'max mean wave angle', 'rad', 'FlowElem_xcc FlowElem_ycc')
       
       ! These go over links

       ! Fx
       ierr = nf90_def_var(imapfile, 'Fx_mean' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_Fx_mean)
       ierr = nf90_put_att(imapfile, id_Fx_mean,'standard_name', 'sea_surface_wave_force_east_mean')
       ierr = nf90_put_att(imapfile, id_Fx_mean,'units'        , 'N m-2')
       ierr = nf90_put_att(imapfile, id_Fx_mean,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_Fx_mean,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'Fx_var' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_Fx_var)
       ierr = nf90_put_att(imapfile, id_Fx_var,'standard_name', 'sea_surface_wave_force_east_var')
       ierr = nf90_put_att(imapfile, id_Fx_var,'units'        , 'N2 m-4')
       ierr = nf90_put_att(imapfile, id_Fx_var,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_Fx_var,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'Fx_min' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_Fx_min)
       ierr = nf90_put_att(imapfile, id_Fx_min,'standard_name', 'sea_surface_wave_force_east_min')
       ierr = nf90_put_att(imapfile, id_Fx_min,'units'        , 'N m-2')
       ierr = nf90_put_att(imapfile, id_Fx_min,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_Fx_min,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'Fx_max' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_Fx_max)
       ierr = nf90_put_att(imapfile, id_Fx_max,'standard_name', 'sea_surface_wave_force_east_max')
       ierr = nf90_put_att(imapfile, id_Fx_max,'units'        , 'N m-2')
       ierr = nf90_put_att(imapfile, id_Fx_max,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_Fx_max,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ! Fy
       ierr = nf90_def_var(imapfile, 'Fy_mean' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_Fy_mean)
       ierr = nf90_put_att(imapfile, id_Fy_mean,'standard_name', 'sea_surface_wave_force_north_mean')
       ierr = nf90_put_att(imapfile, id_Fy_mean,'units'        , 'N m-2')
       ierr = nf90_put_att(imapfile, id_Fy_mean,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_Fy_mean,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'Fy_var' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_Fy_var)
       ierr = nf90_put_att(imapfile, id_Fy_var,'standard_name', 'sea_surface_wave_force_north_var')
       ierr = nf90_put_att(imapfile, id_Fy_var,'units'        , 'N2 m-4')
       ierr = nf90_put_att(imapfile, id_Fy_var,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_Fy_var,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'Fy_min' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_Fy_min)
       ierr = nf90_put_att(imapfile, id_Fy_min,'standard_name', 'sea_surface_wave_force_north_min')
       ierr = nf90_put_att(imapfile, id_Fy_min,'units'        , 'N m-2')
       ierr = nf90_put_att(imapfile, id_Fy_min,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_Fy_min,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'Fy_max' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_Fy_max)
       ierr = nf90_put_att(imapfile, id_Fy_max,'standard_name', 'sea_surface_wave_force_north_max')
       ierr = nf90_put_att(imapfile, id_Fy_max,'units'        , 'N m-2')
       ierr = nf90_put_att(imapfile, id_Fy_max,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_Fy_max,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ! Ustokes
       ierr = nf90_def_var(imapfile, 'ust_mean' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_ust_mean)
       ierr = nf90_put_att(imapfile, id_ust_mean,'standard_name', 'sea_surface_Stokes_drift_east_mean')
       ierr = nf90_put_att(imapfile, id_ust_mean,'units'        , 'm s-1')
       ierr = nf90_put_att(imapfile, id_ust_mean,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_ust_mean,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'ust_var' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_ust_var)
       ierr = nf90_put_att(imapfile, id_ust_var,'standard_name', 'sea_surface_Stokes_drift_east_var')
       ierr = nf90_put_att(imapfile, id_ust_var,'units'        , 'm2 s-2')
       ierr = nf90_put_att(imapfile, id_ust_var,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_ust_var,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'ust_min' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_ust_min)
       ierr = nf90_put_att(imapfile, id_ust_min,'standard_name', 'sea_surface_Stokes_drift_east_min')
       ierr = nf90_put_att(imapfile, id_ust_min,'units'        , 'm s-1')
       ierr = nf90_put_att(imapfile, id_ust_min,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_ust_min,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'ust_max' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_ust_max)
       ierr = nf90_put_att(imapfile, id_ust_max,'standard_name', 'sea_surface_wave_force_east_max')
       ierr = nf90_put_att(imapfile, id_ust_max,'units'        , 'm s-1')
       ierr = nf90_put_att(imapfile, id_ust_max,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_ust_max,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'vst_mean' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_vst_mean)
       ierr = nf90_put_att(imapfile, id_vst_mean,'standard_name', 'sea_surface_Stokes_drift_north_mean')
       ierr = nf90_put_att(imapfile, id_vst_mean,'units'        , 'm s-1')
       ierr = nf90_put_att(imapfile, id_vst_mean,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_vst_mean,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'vst_var' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_vst_var)
       ierr = nf90_put_att(imapfile, id_vst_var,'standard_name', 'sea_surface_Stokes_drift_north_var')
       ierr = nf90_put_att(imapfile, id_vst_var,'units'        , 'm2 s-2')
       ierr = nf90_put_att(imapfile, id_vst_var,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_vst_var,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'vst_min' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_vst_min)
       ierr = nf90_put_att(imapfile, id_vst_min,'standard_name', 'sea_surface_Stokes_drift_north_min')
       ierr = nf90_put_att(imapfile, id_vst_min,'units'        , 'm s-1')
       ierr = nf90_put_att(imapfile, id_vst_min,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_vst_min,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'vst_max' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_vst_max)
       ierr = nf90_put_att(imapfile, id_vst_max,'standard_name', 'sea_surface_wave_force_east_max')
       ierr = nf90_put_att(imapfile, id_vst_max,'units'        , 'm s-1')
       ierr = nf90_put_att(imapfile, id_vst_max,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_vst_max,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ! Orbital u
       ierr = nf90_def_var(imapfile, 'urms_mean' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_urms_mean)
       ierr = nf90_put_att(imapfile, id_urms_mean,'standard_name', 'sea_surface_wave_orbital_velocity_mean')
       ierr = nf90_put_att(imapfile, id_urms_mean,'units'        , 'm s-1')
       ierr = nf90_put_att(imapfile, id_urms_mean,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_urms_mean,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'urms_var' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_urms_var)
       ierr = nf90_put_att(imapfile, id_urms_var,'standard_name', 'sea_surface_wave_orbital_velocity_var')
       ierr = nf90_put_att(imapfile, id_urms_var,'units'        , 'm2 s-2')
       ierr = nf90_put_att(imapfile, id_urms_var,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_urms_var,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'urms_min' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_urms_min)
       ierr = nf90_put_att(imapfile, id_urms_min,'standard_name', 'sea_surface_wave_orbital_velocity_min')
       ierr = nf90_put_att(imapfile, id_urms_min,'units'        , 'm s-1')
       ierr = nf90_put_att(imapfile, id_urms_min,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_urms_min,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'urms_max' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_urms_max)
       ierr = nf90_put_att(imapfile, id_urms_max,'standard_name', 'sea_surface_wave_orbital_velocity_max')
       ierr = nf90_put_att(imapfile, id_urms_max,'units'        , 'm s-1')
       ierr = nf90_put_att(imapfile, id_urms_max,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_urms_max,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ! Velocity: east
       ierr = nf90_def_var(imapfile, 'u_mean' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_u_mean)
       ierr = nf90_put_att(imapfile, id_u_mean,'standard_name', 'sea_water_speed_east_mean')
       ierr = nf90_put_att(imapfile, id_u_mean,'units'        , 'm s-1')
       ierr = nf90_put_att(imapfile, id_u_mean,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_u_mean,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'u_var' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_u_var)
       ierr = nf90_put_att(imapfile, id_u_var,'standard_name', 'sea_water_speed_east_var')
       ierr = nf90_put_att(imapfile, id_u_var,'units'        , 'm2 s-2')
       ierr = nf90_put_att(imapfile, id_u_var,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_u_var,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'u_min' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_u_min)
       ierr = nf90_put_att(imapfile, id_u_min,'standard_name', 'sea_water_speed_east_min')
       ierr = nf90_put_att(imapfile, id_u_min,'units'        , 'm s-1')
       ierr = nf90_put_att(imapfile, id_u_min,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_u_min,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'u_max' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_u_max)
       ierr = nf90_put_att(imapfile, id_u_max,'standard_name', 'sea_water_speed_east_max')
       ierr = nf90_put_att(imapfile, id_u_max,'units'        , 'm s-1')
       ierr = nf90_put_att(imapfile, id_u_max,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_u_max,'coordinates'  , 'FlowLink_xu FlowLink_yu')


       ! Velocity: north
       ierr = nf90_def_var(imapfile, 'v_mean' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_v_mean)
       ierr = nf90_put_att(imapfile, id_v_mean,'standard_name', 'sea_water_speed_north_mean')
       ierr = nf90_put_att(imapfile, id_v_mean,'units'        , 'm s-1')
       ierr = nf90_put_att(imapfile, id_v_mean,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_v_mean,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'v_var' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_v_var)
       ierr = nf90_put_att(imapfile, id_v_var,'standard_name', 'sea_water_speed_north_var')
       ierr = nf90_put_att(imapfile, id_v_var,'units'        , 'm2 s-2')
       ierr = nf90_put_att(imapfile, id_v_var,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_v_var,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'v_min' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_v_min)
       ierr = nf90_put_att(imapfile, id_v_min,'standard_name', 'sea_water_speed_north_min')
       ierr = nf90_put_att(imapfile, id_v_min,'units'        , 'm s-1')
       ierr = nf90_put_att(imapfile, id_v_min,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_v_min,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_def_var(imapfile, 'v_max' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_v_max)
       ierr = nf90_put_att(imapfile, id_v_max,'standard_name', 'sea_water_speed_north_max')
       ierr = nf90_put_att(imapfile, id_v_max,'units'        , 'm s-1')
       ierr = nf90_put_att(imapfile, id_v_max,'interfaces'   , 'FlowLink')
       ierr = nf90_put_att(imapfile, id_v_max,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ierr = nf90_enddef(imapfile)
       firststep = .false.
    endif   
    ! End of writing time-independent flow geometry data.

    ! -- Inquire id's belonging to map file ------------------------
    if (firststep .and. ndim>0) then 
       ! 
       ! 
       ! this step is necessary because if a snapshot_map.nc file is written
       ! in between two map file outputs the saved id's may have changed
       !
       firststep = .false. 
       !
       ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim)
       ierr = nf90_inq_dimid(imapfile, 'nFlowLink', id_flowlinkdim)
       !
       ! Time
       ierr = nf90_inq_dimid(imapfile, 'time', id_timedim)
       ierr = nf90_inq_varid(imapfile, 'time', id_time)       
       !    
       ! Size of latest timestep
       ! ierr = nf90_inq_varid(imapfile, 'timestep', id_timestep)

       ierr = nf90_inq_varid(imapfile, 'E_mean', id_E_mean)
       ierr = nf90_inq_varid(imapfile, 'E_var', id_E_var)
       ierr = nf90_inq_varid(imapfile, 'E_min', id_E_min)
       ierr = nf90_inq_varid(imapfile, 'E_max', id_E_max)

       ierr = nf90_inq_varid(imapfile, 'H_mean', id_H_mean)
       ierr = nf90_inq_varid(imapfile, 'H_var', id_H_var)
       ierr = nf90_inq_varid(imapfile, 'H_min', id_H_min)
       ierr = nf90_inq_varid(imapfile, 'H_max', id_H_max)

       ierr = nf90_inq_varid(imapfile, 'R_mean', id_R_mean)
       ierr = nf90_inq_varid(imapfile, 'R_var', id_R_var)
       ierr = nf90_inq_varid(imapfile, 'R_min', id_R_min)
       ierr = nf90_inq_varid(imapfile, 'R_max', id_R_max)

       ierr = nf90_inq_varid(imapfile, 'D_mean', id_D_mean)
       ierr = nf90_inq_varid(imapfile, 'D_var', id_D_var)
       ierr = nf90_inq_varid(imapfile, 'D_min', id_D_min)
       ierr = nf90_inq_varid(imapfile, 'D_max', id_D_max)

       ierr = nf90_inq_varid(imapfile, 'DR_mean', id_DR_mean)
       ierr = nf90_inq_varid(imapfile, 'DR_var', id_DR_var)
       ierr = nf90_inq_varid(imapfile, 'DR_min', id_DR_min)
       ierr = nf90_inq_varid(imapfile, 'DR_max', id_DR_max)

       ierr = nf90_inq_varid(imapfile, 'Fx_mean', id_Fx_mean)
       ierr = nf90_inq_varid(imapfile, 'Fx_var', id_Fx_var)
       ierr = nf90_inq_varid(imapfile, 'Fx_min', id_Fx_min)
       ierr = nf90_inq_varid(imapfile, 'Fx_max', id_Fx_max)

       ierr = nf90_inq_varid(imapfile, 'Fy_mean', id_Fy_mean)
       ierr = nf90_inq_varid(imapfile, 'Fy_var', id_Fy_var)
       ierr = nf90_inq_varid(imapfile, 'Fy_min', id_Fy_min)
       ierr = nf90_inq_varid(imapfile, 'Fy_max', id_Fy_max)

       ierr = nf90_inq_varid(imapfile, 'ust_mean', id_ust_mean)
       ierr = nf90_inq_varid(imapfile, 'ust_var', id_ust_var)
       ierr = nf90_inq_varid(imapfile, 'ust_min', id_ust_min)
       ierr = nf90_inq_varid(imapfile, 'ust_max', id_ust_max)

       ierr = nf90_inq_varid(imapfile, 'vst_mean', id_vst_mean)
       ierr = nf90_inq_varid(imapfile, 'vst_var', id_vst_var)
       ierr = nf90_inq_varid(imapfile, 'vst_min', id_vst_min)
       ierr = nf90_inq_varid(imapfile, 'vst_max', id_vst_max)

       ierr = nf90_inq_varid(imapfile, 'urms_mean', id_urms_mean)
       ierr = nf90_inq_varid(imapfile, 'urms_var', id_urms_var)
       ierr = nf90_inq_varid(imapfile, 'urms_min', id_urms_min)
       ierr = nf90_inq_varid(imapfile, 'urms_max', id_urms_max)

       ierr = nf90_inq_varid(imapfile, 'cwav_mean', id_cwav_mean)
       ierr = nf90_inq_varid(imapfile, 'cwav_var', id_cwav_var)
       ierr = nf90_inq_varid(imapfile, 'cwav_min', id_cwav_min)
       ierr = nf90_inq_varid(imapfile, 'cwav_max', id_cwav_max)

       ierr = nf90_inq_varid(imapfile, 'cgwav_mean', id_cgwav_mean)
       ierr = nf90_inq_varid(imapfile, 'cgwav_var', id_cgwav_var)
       ierr = nf90_inq_varid(imapfile, 'cgwav_min', id_cgwav_min)
       ierr = nf90_inq_varid(imapfile, 'cgwav_max', id_cgwav_max)

       ierr = nf90_inq_varid(imapfile, 'thetamean_mean', id_thetamean_mean)
       ierr = nf90_inq_varid(imapfile, 'thetamean_var', id_thetamean_var)
       ierr = nf90_inq_varid(imapfile, 'thetamean_min', id_thetamean_min)
       ierr = nf90_inq_varid(imapfile, 'thetamean_max', id_thetamean_max)

       ierr = nf90_inq_varid(imapfile, 'sigmwav_mean', id_sigmwav_mean)
       ierr = nf90_inq_varid(imapfile, 'sigmwav_var', id_sigmwav_var)
       ierr = nf90_inq_varid(imapfile, 'sigmwav_min', id_sigmwav_min)
       ierr = nf90_inq_varid(imapfile, 'sigmwav_max', id_sigmwav_max)

       ierr = nf90_inq_varid(imapfile, 's1_mean', id_s1_mean)
       ierr = nf90_inq_varid(imapfile, 's1_var', id_s1_var)
       ierr = nf90_inq_varid(imapfile, 's1_min', id_s1_min)
       ierr = nf90_inq_varid(imapfile, 's1_max', id_s1_max)

       ierr = nf90_inq_varid(imapfile, 'u_mean', id_u_mean)
       ierr = nf90_inq_varid(imapfile, 'u_var', id_u_var)
       ierr = nf90_inq_varid(imapfile, 'u_min', id_u_min)
       ierr = nf90_inq_varid(imapfile, 'u_max', id_u_max)

       ierr = nf90_inq_varid(imapfile, 'v_mean', id_v_mean)
       ierr = nf90_inq_varid(imapfile, 'v_var', id_v_var)
       ierr = nf90_inq_varid(imapfile, 'v_min', id_v_min)
       ierr = nf90_inq_varid(imapfile, 'v_max', id_v_max)


    end if    
    
    ! -- Start data writing (flow data) ------------------------
    it_wav   = it_wav+1
    itim     = it_wav ! Increment time dimension index  

    ! Time
    ierr = nf90_put_var(imapfile, id_time    , tim, (/  itim /))

    ! Data on flow nodes
    ierr = nf90_put_var(imapfile, id_E_mean, E_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_E_var, E_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_E_max, E_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_E_min, E_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))

    do k = 1, ndx   ! stack
       ierr = nf90_put_var(imapfile, id_H_mean, sqrt(H_varsquare(k:k)), (/ 1, itim /), (/ 1, 1 /)) 
    end do
    ierr = nf90_put_var(imapfile, id_H_var, H_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_H_max, H_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_H_min, H_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))

    ierr = nf90_put_var(imapfile, id_R_mean, R_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_R_var, R_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_R_max, R_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_R_min, R_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))

    ierr = nf90_put_var(imapfile, id_D_mean, D_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_D_var, D_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_D_max, D_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_D_min, D_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))

    ierr = nf90_put_var(imapfile, id_DR_mean, DR_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_DR_var, DR_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_DR_max, DR_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_DR_min, DR_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))

    ierr = nf90_put_var(imapfile, id_cwav_mean, cwav_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_cwav_var, cwav_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_cwav_max, cwav_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_cwav_min, cwav_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))

    ierr = nf90_put_var(imapfile, id_cgwav_mean, cgwav_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_cgwav_var, cgwav_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_cgwav_max, cgwav_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_cgwav_min, cgwav_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))

    do k = 1, ndx ! stack
        ierr = nf90_put_var(imapfile, id_thetamean_mean, &
                            mod(2.d0*pi + atan2(nint(thetamean_mean(k:k))/1d7, &
                            mod(thetamean_mean(k:k),1.d0)*1d1), 2.d0*pi), &
                            (/ 1, itim /), (/ 1, 1 /))
    end do
    ierr = nf90_put_var(imapfile, id_thetamean_var, thetamean_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_thetamean_max, thetamean_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_thetamean_min, thetamean_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))

    ierr = nf90_put_var(imapfile, id_sigmwav_mean, sigmwav_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_sigmwav_var, sigmwav_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_sigmwav_max, sigmwav_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_sigmwav_min, sigmwav_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
 
    ierr = nf90_put_var(imapfile, id_s1_mean, s1_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_s1_var, s1_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_s1_max, s1_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_s1_min, s1_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))

    ! Data on flow links
    ierr = nf90_put_var(imapfile, id_Fx_mean, Fx_mean(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 
    ierr = nf90_put_var(imapfile, id_Fx_var, Fx_var(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 
    ierr = nf90_put_var(imapfile, id_Fx_max, Fx_max(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 
    ierr = nf90_put_var(imapfile, id_Fx_min, Fx_min(1:lnx), (/ 1, itim /), (/ lnx, 1 /))

    ierr = nf90_put_var(imapfile, id_Fy_mean, Fy_mean(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 
    ierr = nf90_put_var(imapfile, id_Fy_var, Fy_var(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 
    ierr = nf90_put_var(imapfile, id_Fy_max, Fy_max(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 
    ierr = nf90_put_var(imapfile, id_Fy_min, Fy_min(1:lnx), (/ 1, itim /), (/ lnx, 1 /))

    ierr = nf90_put_var(imapfile, id_ust_mean, ust_mean(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 
    ierr = nf90_put_var(imapfile, id_ust_var, ust_var(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 
    ierr = nf90_put_var(imapfile, id_ust_max, ust_max(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 
    ierr = nf90_put_var(imapfile, id_ust_min, ust_min(1:lnx), (/ 1, itim /), (/ lnx, 1 /))

    ierr = nf90_put_var(imapfile, id_vst_mean, vst_mean(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 
    ierr = nf90_put_var(imapfile, id_vst_var,  vst_var(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 
    ierr = nf90_put_var(imapfile, id_vst_max,  vst_max(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 
    ierr = nf90_put_var(imapfile, id_vst_min,  vst_min(1:lnx), (/ 1, itim /), (/ lnx, 1 /))

    temp = sqrt(urms_varsquare)
    ierr = nf90_put_var(imapfile, id_urms_mean, temp, (/ 1, itim /), (/ lnx, 1 /))
    ierr = nf90_put_var(imapfile, id_urms_var, urms_var(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 
    ierr = nf90_put_var(imapfile, id_urms_max, urms_max(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 
    ierr = nf90_put_var(imapfile, id_urms_min, urms_min(1:lnx), (/ 1, itim /), (/ lnx, 1 /))

    ierr = nf90_put_var(imapfile, id_u_mean, u_mean(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 
    ierr = nf90_put_var(imapfile, id_u_var,  u_var(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 
    ierr = nf90_put_var(imapfile, id_u_max,  u_max(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 
    ierr = nf90_put_var(imapfile, id_u_min,  u_min(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 

    ierr = nf90_put_var(imapfile, id_v_mean, v_mean(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 
    ierr = nf90_put_var(imapfile, id_v_var,  v_var(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 
    ierr = nf90_put_var(imapfile, id_v_max,  v_max(1:lnx), (/ 1, itim /), (/ lnx, 1 /)) 
    ierr = nf90_put_var(imapfile, id_v_min,  v_min(1:lnxi), (/ 1, itim /), (/ lnx, 1 /)) 
 
end subroutine unc_write_wav_filepointer


!subroutine unc_write_wav_filepointer_ugrid(wavids, tim)
!   use unstruc_netcdf
!   implicit none
!   
!   type(t_unc_wavids), intent(inout) :: wavids   !< Set of file and variable ids for this map-type file.
!   real(kind=hp),      intent(in)    :: tim
!   
!   integer                       :: idims(2)
!   logical, dimension(2), save   :: firststep = .true.
!
!   integer, save                 :: ierr, ndim
!   
!   ndim = 0
!   ierr = nf90_inquire(wavids%ncid, nDimensions=ndim)
!
!   if (ndim == 0) then
!      ierr = ug_addglobalatts(wavids%ncid, ug_meta_fm)
!      call unc_write_wavgeom_filepointer_ugrid(wavids, 0)
!      
!   end if
!
!end subroutine unc_write_wav_filepointer_ugrid

!! Construct averages for netcdf output
!! (Re)allocation in flow_waveinit
subroutine xbeach_makeaverages(dt)
   use m_flow
   use m_flowgeom
   use m_flowtimes
   use m_xbeach_data
   use m_xbeach_avgoutput
   use m_alloc
   use m_sferic
   implicit none

   double precision, intent(in)              :: dt                         ! timestep
   double precision                          :: mult, fill
   integer                                   :: ierr, result1, result2

   double precision, allocatable                  :: tvar_sin(:)
   double precision, allocatable                  :: tvar_cos(:)
   double precision, allocatable                  :: oldmean(:), ux(:), uy(:)

   ierr = 1
   call realloc(tvar_sin, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call realloc(tvar_cos, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call realloc(oldmean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call realloc(ux, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
   call realloc(uy, lnx, stat=ierr, keepExisting = .false., fill = 0d0)

   mult = max(dt/ti_wav,0.d0)
   !multcum = multcum + mult
   !write(*,*) 'Multiplier: ', mult, ', cumulative proportion: ', multcum
   
   !! Data on flow nodes
   ! H
   oldmean = H_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   H_mean = H_mean + mult*H
   H_varcross = H_varcross/oldmean*H_mean + mult*2.d0*H*H_mean
   H_varsquare = H_varsquare + mult*(H)**2
   H_var = H_varsquare - H_varcross + H_mean**2
   H_max = max(H_max,H)
   H_min = min(H_min,H)

   ! E
   oldmean = E_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   E_mean = E_mean + mult*E
   E_varcross = E_varcross/oldmean*E_mean + mult*2.d0*E*E_mean
   E_varsquare = E_varsquare + mult*(E)**2
   E_var = E_varsquare - E_varcross + E_mean**2
   E_max = max(E_max,E)
   E_min = min(E_min,E)

   !R
   oldmean = R_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   R_mean = R_mean + mult*R
   R_varcross = R_varcross/oldmean*R_mean + mult*2.d0*R*R_mean
   R_varsquare = R_varsquare + mult*(R)**2
   R_var = R_varsquare - R_varcross + R_mean**2
   R_max = max(R_max,R)
   R_min = min(R_min,R)

   ! D
   oldmean = D_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   D_mean = D_mean + mult*D
   D_varcross = D_varcross/oldmean*D_mean + mult*2.d0*D*D_mean
   D_varsquare = D_varsquare + mult*(D)**2
   D_var = D_varsquare - D_varcross + D_mean**2
   D_max = max(D_max,D)
   D_min = min(D_min,D)

   ! DR
   oldmean = DR_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere

   !< DEBUG
   DR_mean = DR_mean + mult*DR
   DR_varcross = 1d-20 + DR_varcross/oldmean*DR_mean + mult*2.d0*DR*DR_mean
   DR_varsquare = DR_varsquare + mult*(DR)**2
   DR_var = DR_varsquare - DR_varcross + DR_mean**2
   DR_max = max(DR_max,DR)
   DR_min = min(DR_min,DR)
   !</ DEBUG

  ! cwav
   oldmean = cwav_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   cwav_mean = cwav_mean + mult*cwav
   cwav_varcross = cwav_varcross/oldmean*cwav_mean + mult*2.d0*cwav*cwav_mean
   cwav_varsquare = cwav_varsquare + mult*(cwav)**2
   cwav_var = cwav_varsquare - cwav_varcross + cwav_mean**2
   cwav_max = max(cwav_max,cwav)
   cwav_min = min(cwav_min,cwav)

  ! cgwav
   oldmean = cgwav_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   cgwav_mean = cgwav_mean + mult*cgwav
   cgwav_varcross = cgwav_varcross/oldmean*cgwav_mean + mult*2.d0*cgwav*cgwav_mean
   cgwav_varsquare = cgwav_varsquare + mult*(cgwav)**2
   cgwav_var = cgwav_varsquare - cgwav_varcross + cgwav_mean**2
   cgwav_max = max(cgwav_max,cgwav)
   cgwav_min = min(cgwav_min,cgwav)

  ! sigmwav
   oldmean = sigmwav_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   sigmwav_mean = sigmwav_mean + mult*sigmwav
   sigmwav_varcross = sigmwav_varcross/oldmean*sigmwav_mean + mult*2.d0*sigmwav*sigmwav_mean
   sigmwav_varsquare = sigmwav_varsquare + mult*(sigmwav)**2
   sigmwav_var = sigmwav_varsquare - sigmwav_varcross + sigmwav_mean**2
   sigmwav_max = max(sigmwav_max,sigmwav)
   sigmwav_min = min(sigmwav_min,sigmwav)

  ! thetamean: unsure whether all this is correct
   oldmean = thetamean_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   thetamean_sin = thetamean_sin + mult*sin(thetamean)
   thetamean_cos = thetamean_cos + mult*cos(thetamean)
   thetamean_mean = mod(atan2(thetamean_sin, thetamean_cos), 2d0*pi)
   thetamean_varcross = thetamean_varcross/oldmean*thetamean_mean + mult*2.d0*thetamean*thetamean_mean
   thetamean_varsquare = thetamean_varsquare + mult*(thetamean)**2
   thetamean_var = thetamean_varsquare - thetamean_varcross + thetamean_mean**2
   thetamean_max = max(thetamean_max,thetamean)
   thetamean_min = min(thetamean_min,thetamean)
   
   ! s1
   oldmean = s1_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   s1_mean = s1_mean + mult*s1
   s1_varcross = s1_varcross/oldmean*s1_mean + mult*2.d0*s1*s1_mean
   s1_varsquare = s1_varsquare + mult*(s1)**2
   s1_var = s1_varsquare - s1_varcross + s1_mean**2
   s1_max = max(s1_max,s1)
   s1_min = min(s1_min,s1)

!! Data on flow links
!! JRE Port to cell centres using ucx, ucy 
   call realloc(oldmean, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('oldmean  (lnx)', ierr, lnx)

   ! u: x-component
   oldmean = u_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   ux = u1*csu - v*snu
   u_mean = u_mean + mult*ux
   u_varcross = u_varcross/oldmean*u_mean + mult*2.d0*ux*u_mean
   u_varsquare = u_varsquare + mult*(ux)**2
   u_var = u_varsquare - u_varcross + u_mean**2
   u_max = max(u_max,ux)
   u_min = min(u_min,ux)

   ! v: y-component
   oldmean = v_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   uy = u1*snu + v*csu
   v_mean = v_mean + mult*uy
   v_varcross = v_varcross/oldmean*v_mean + mult*2.d0*uy*v_mean
   v_varsquare = v_varsquare + mult*(uy)**2
   v_var = v_varsquare - v_varcross + v_mean**2
   v_max = max(v_max,uy)
   v_min = min(v_min,uy)

   ! Fx: y-component
   oldmean = Fx_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   Fx_mean = Fx_mean + mult*Fx
   Fx_varcross = Fx_varcross/oldmean*Fx_mean + mult*2.d0*Fx*Fx_mean
   Fx_varsquare = Fx_varsquare + mult*(Fx)**2
   Fx_var = Fx_varsquare - Fx_varcross + Fx_mean**2
   Fx_max = max(Fx_max,Fx)
   Fx_min = min(Fx_min,Fx)

   ! Fy
   oldmean = Fy_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   Fy_mean = Fy_mean + mult*Fy
   Fy_varcross = Fy_varcross/oldmean*Fy_mean + mult*2.d0*Fy*Fy_mean
   Fy_varsquare = Fy_varsquare + mult*(Fy)**2
   Fy_var = Fy_varsquare - Fy_varcross + Fy_mean**2
   Fy_max = max(Fy_max,Fy)
   Fy_min = min(Fy_min,Fy)

   ! ust
   oldmean = ust_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   ux = csu*ust - snu*vst
   ust_mean = ust_mean + mult*ux
   ust_varcross = ust_varcross/oldmean*ust_mean + mult*2.d0*ux*ust_mean
   ust_varsquare = ust_varsquare + mult*(ux)**2
   ust_var = ust_varsquare - ust_varcross + ust_mean**2
   ust_max = max(ust_max,ux)
   ust_min = min(ust_min,ux)

   ! vst
   oldmean = vst_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   uy = snu*ust + csu*uy
   vst_mean = vst_mean + mult*uy
   vst_varcross = vst_varcross/oldmean*vst_mean + mult*2.d0*uy*vst_mean
   vst_varsquare = vst_varsquare + mult*(uy)**2
   vst_var = vst_varsquare - vst_varcross + vst_mean**2
   vst_max = max(vst_max,uy)
   vst_min = min(vst_min,uy)

   ! urms
   oldmean = urms_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   urms_mean = urms_mean + mult*urms
   urms_varcross = urms_varcross/oldmean*urms_mean + mult*2.d0*urms*urms_mean
   urms_varsquare = urms_varsquare + mult*(urms)**2
   urms_var = urms_varsquare - urms_varcross + urms_mean**2
   urms_max = max(urms_max,urms)
   urms_min = min(urms_min,urms)
     
   ierr = 0
1234 continue
   deallocate(ux, uy, tvar_sin,tvar_cos,oldmean, stat=ierr)
   return

end subroutine xbeach_makeaverages

subroutine xbeach_clearaverages()
   use m_xbeach_avgoutput
   implicit none

   integer               :: ierr

   ierr = 1
   !multcum = 0d0
   H_mean = 0d0; H_var  = 0d0; H_min  = huge(0d0); H_max  = -1d0*huge(0d0); H_varcross = 0d0; H_varsquare = 0d0
   E_mean = 0d0; E_var  = 0d0; E_min  = huge(0d0); E_max  = -1d0*huge(0d0); E_varcross = 0d0; E_varsquare = 0d0
   R_mean = 0d0; R_var  = 0d0; R_min  = huge(0d0); R_max  = -1d0*huge(0d0); R_varcross = 0d0; R_varsquare = 0d0
   D_mean = 0d0; D_var  = 0d0; D_min  = huge(0d0); D_max  = -1d0*huge(0d0); D_varcross = 0d0; D_varsquare = 0d0
   DR_mean = 0d0; DR_var  = 0d0; DR_min  = huge(0d0); DR_max  = -1d0*huge(0d0); DR_varcross = 0d0; DR_varsquare = 0d0
   ust_mean = 0d0; ust_var  = 0d0; ust_min  = huge(0d0); ust_max  = -1d0*huge(0d0); ust_varcross = 0d0; ust_varsquare = 0d0
   urms_mean = 0d0; urms_var  = 0d0; urms_min  = huge(0d0); urms_max  = -1d0*huge(0d0); urms_varcross = 0d0; urms_varsquare = 0d0
   thetamean_mean = 0d0; thetamean_var  = 0d0; thetamean_min  = huge(0d0); thetamean_max  = -1d0*huge(0d0); thetamean_varcross = 0d0; 
   thetamean_varsquare = 0d0; thetamean_sin = 0d0; thetamean_cos = 0d0
   cwav_mean = 0d0; cwav_var  = 0d0; cwav_min  = huge(0d0); cwav_max  = -1d0*huge(0d0); cwav_varcross = 0d0; cwav_varsquare = 0d0
   cgwav_mean = 0d0; cgwav_var  = 0d0; cgwav_min  = huge(0d0); cgwav_max  = -1d0*huge(0d0); cgwav_varcross = 0d0; cgwav_varsquare = 0d0
   Fx_mean = 0d0; Fx_var  = 0d0; Fx_min  = huge(0d0); Fx_max  = -1d0*huge(0d0); Fx_varcross = 0d0; Fx_varsquare = 0d0
   Fy_mean = 0d0; Fy_var  = 0d0; Fy_min  = huge(0d0); Fy_max  = -1d0*huge(0d0); Fy_varcross = 0d0; Fy_varsquare = 0d0
   s1_mean = 0d0; s1_var  = 0d0; s1_min  = huge(0d0); s1_max  = -1d0*huge(0d0); s1_varcross = 0d0; s1_varsquare = 0d0
   u_mean = 0d0; u_var  = 0d0; u_min  = huge(0d0); u_max  = -1d0*huge(0d0); u_varcross = 0d0; u_varsquare = 0d0
   v_mean = 0d0; v_var  = 0d0; v_min  = huge(0d0);v_max  = -1d0*huge(0d0); v_varcross = 0d0; v_varsquare = 0d0
   sigmwav_mean = 0d0; sigmwav_var  = 0d0; sigmwav_min  = huge(0d0); sigmwav_max  = -1d0*huge(0d0); sigmwav_varcross = 0d0; sigmwav_varsquare = 0d0

   ierr = 0
1234 continue
   return

end subroutine xbeach_clearaverages

end module m_xbeach_netcdf
