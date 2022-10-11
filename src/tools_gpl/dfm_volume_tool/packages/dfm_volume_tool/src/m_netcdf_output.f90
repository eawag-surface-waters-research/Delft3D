!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2022.                                
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


!> Module for writing storage table information in the DFM_VOLUME tool
module m_netcdf_output
   
   use netcdf
private

public nc_create
public nc_write

contains
   
!> Create the Netcdf file with name NCFILE
!> and fill in the common fields.
function nc_create(ncfile) result (ioutput)
   use dfm_volume_tool_version_module
   implicit none
   character(len=*), intent(in) :: ncfile   !< name of the new netcdf-file to be created
   integer                      :: ioutput     !< Netcdf ID given to this file

   character*8  :: cdate
   character*10 :: ctime
   character*5  :: czone
   integer :: ierr
   integer :: oldfillmode
   ierr = nf90_noerr

   ierr = nf90_create(trim(ncfile), 0, ioutput)
   if (ierr .ne. 0) call nc_error(ierr, 'nc_create')
   ierr = nf90_set_fill(ioutput,NF90_NOFILL,oldfillmode)
   if (ierr .ne. 0) call nc_error(ierr, 'nc_create')
   ierr = nf90_put_att(ioutput, nf90_global, 'institution', 'Deltares')                 !<-----RL666----------------- Aanpassen aan sobek 
   if (ierr .ne. 0) call nc_error(ierr, 'nc_create')
   ierr = nf90_put_att(ioutput, nf90_global, 'references' , 'http://www.deltares.nl')
   if (ierr .ne. 0) call nc_error(ierr, 'nc_create')
   ierr = nf90_put_att(ioutput, nf90_global, 'source'     , trim(dfm_volume_tool_version_full))
   if (ierr .ne. 0) call nc_error(ierr, 'nc_create')

   call date_and_time(cdate, ctime, czone)
   ierr = nf90_put_att(ioutput, nf90_global,  'history', &
       'Created on '//cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)// &
       ',  DFM Volume Tool')
   if (ierr .ne. 0) call nc_error(ierr, 'nc_create')

!   ierr = nf90_put_att(ioutput, nf90_global,  'simulation_start_date', 'Startdate: '//RefDate(1:4)//'-'//RefDate(6:7)//'-'//RefDate(9:10))
!   if (ierr .ne. 0) call nc_error(ierr, 'nc_create')

   ierr = nf90_put_att(ioutput, nf90_global,  'Conventions', 'CF-1.7')
   if (ierr .ne. 0) call nc_error(ierr, 'nc_create')
   ! ierr = nf90_put_att(ioutput, nf90_global,  'featureType', 'timeSeries')
   ! if (ierr .ne. 0) call nc_error(ierr, 'nc_create')

end function nc_create

!> Write the volumes and surfaces to the Netcdf file
subroutine nc_write(ioutput, ids, levels, volume, surface, numlevels, numids)

   use MessageHandling

   integer,                            intent(in)  :: ioutput           !< Handle to the output file.
   character(len=IdLen), dimension(:), intent(in)  :: ids               !< Ids of the entries in the volume table.
   double precision, dimension(:),     intent(in)  :: levels            !< Levels w.r.t. the bedlevel.
   double precision, dimension(:,:),   intent(in)  :: volume            !< Volumes. 
   double precision, dimension(:,:),   intent(in)  :: surface           !< Wet surface area.
   integer,                            intent(in)  :: numlevels         !< Number of levels in the volume table
   integer,                            intent(in)  :: numids            !< Number of volume tables (per Id)

   integer :: dimid_levels, dimid_ids, dimid_chars
   integer :: varid_id, varid_levels, varid_volume, varid_surface

   ierr = nf90_def_dim(ioutput, 'levels', numlevels, dimid_levels)    ! ... if not, make this an unlimited dimension
   if (ierr .ne. 0) call nc_error(ierr, 'nc_prepare')
   
   ierr = nf90_def_dim(ioutput, 'id', numids, dimid_ids)
   if (ierr .ne. 0) call nc_error(ierr, 'nc_prepare')
   ierr = nf90_def_dim(ioutput, 'id_charlen', IdLen, dimid_chars)
   if (ierr .ne. 0) call nc_error(ierr, 'nc_prepare')
   
   ! Create 1D-array of location ID's
   varid_id          = SetupVariable(ioutput,'id',          nf90_char,   (/ dimid_chars,dimid_ids /),  '', 'Id',       '-')     
   ierr = nf90_put_att(ioutput, varid_id, 'cf_role', 'timeseries_id')
   varid_levels      = SetupVariable(ioutput,'levels',      nf90_double, (/ dimid_levels/),            '', 'Levels',    'm AD') 
   varid_volume      = SetupVariable(ioutput,'volume',      nf90_double, (/ dimid_levels, dimid_ids/), '', 'Volume',    'm3') 
   varid_surface     = SetupVariable(ioutput,'surface',     nf90_double, (/ dimid_levels, dimid_ids/), '', 'Surface',   'm2') 
   ierr = nf90_put_att(ioutput, varid_volume, 'coordinates', 'id levels')   ! if present, standard_name should obey CF-convention 
   if (ierr .ne. 0) call nc_error(ierr, 'nc_prepare')
   ierr = nf90_put_att(ioutput, varid_surface, 'coordinates', 'id levels')   ! if present, standard_name should obey CF-convention 
   if (ierr .ne. 0) call nc_error(ierr, 'nc_prepare')
   ierr = nf90_put_att(ioutput, varid_levels, 'positive', 'up')
   if (ierr .ne. 0) call nc_error(ierr, 'nc_prepare')
   
   ierr = nf90_enddef(ioutput)

   if (ierr .ne. 0) call nc_error(ierr, 'nc_prepare')

   ierr = nf90_put_var(ioutput, varid_id, ids(1:numids), start=(/1,1/), count = (/IdLen,numids/))
   if (ierr .ne. 0) call nc_error(ierr, 'nc_prepare')
   ierr = nf90_put_var(ioutput, varid_levels    , levels, start=(/1/)  , count = (/numlevels/))
   if (ierr .ne. 0) call nc_error(ierr, 'nc_prepare')
   ierr = nf90_put_var(ioutput, varid_volume    , volume, start=(/1,1/), count = (/numlevels, numids/))
   if (ierr .ne. 0) call nc_error(ierr, 'nc_prepare')
   ierr = nf90_put_var(ioutput, varid_surface   , surface, start=(/1,1/), count = (/numlevels, numids/))
   if (ierr .ne. 0) call nc_error(ierr, 'nc_prepare')
   
   ierr = nf90_close(ioutput)
   if (ierr .ne. 0) call nc_error(ierr, 'nc_prepare')

end subroutine nc_write

!> Define the variable with VAR_NAME, including the attributes.
function SetupVariable(ioutput,var_name,var_type,dimensions,std_name,lng_name,units,      & 
   fillDble, fillInt) result (varid)
implicit none
integer, intent(in)               :: ioutput         !< ID of the netcdf file, already opened for writing
character(len=*), intent(in)      :: var_name     !< Variable name in the file
integer,          intent(in)      :: var_type     !< NetCDF variable type (as defined in the netCDF API)
character(len=*), intent(in)      :: std_name     !< Standard name attribute
character(len=*), intent(in)      :: lng_name     !< Long name attribute
character(len=*), intent(in)      :: units        !< Units attribute
double precision, intent(in) , optional  :: fillDble  !< Default value representing missing: Double
integer,          intent(in) , optional  :: fillInt   !< Default value representing missing: Int
integer, dimension(:), intent(in) :: dimensions   !< Vector with the number of levels in each dimension
integer                           :: ierr
integer                           :: varid
ierr = nf90_def_var(ioutput, trim(var_name), var_type, dimensions, varid)
if (ierr .ne. 0) call nc_error(ierr, 'SetupVariable')

if (len_trim(std_name) > 0) then
ierr = nf90_put_att(ioutput, varid, 'standard_name', trim(std_name))   ! if present, standard_name should obey CF-convention 
if (ierr .ne. 0) call nc_error(ierr, 'SetupVariable')
endif

ierr = nf90_put_att(ioutput, varid, 'long_name', trim(lng_name))
if (ierr .ne. 0) call nc_error(ierr, 'SetupVariable')
ierr = nf90_put_att(ioutput, varid, 'units',trim(units))
if (ierr .ne. 0) call nc_error(ierr, 'SetupVariable')

select case (var_type)
case (nf90_double)
if (present(fillDble)) ierr = nf90_put_att(ioutput, varid, '_FillValue', fillDble)
case (nf90_int)
if (present(fillInt)) ierr = nf90_put_att(ioutput, varid, '_FillValue', fillInt)
end select
if (ierr .ne. 0) call nc_error(ierr, 'HisSetupVariable')
end function SetupVariable

!> In case of an error generate an error message
subroutine nc_error(ierr, procName)

   use messageHandling
   
   integer, intent(in)           :: ierr
   character(len=*), intent(in)  :: procName

   character(len=5)              :: cNetCDF_ErrCode = '(   )'

   write(cNetCDF_ErrCode(2:4), '(i3)') ierr
   msgbuf = nf90_strerror(ierr)
   msgbuf = 'NETCDF-ERROR in '//procName//', Error Code: '//cNetCDF_ErrCode//', '//trim(msgbuf)
   call fatal_flush()  

end subroutine nc_error


end module m_netcdf_output
   