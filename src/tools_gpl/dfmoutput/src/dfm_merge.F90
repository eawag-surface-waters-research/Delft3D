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

! $Id: dfm_merge.F90 52271 2017-09-04 07:52:04Z klecz_ml $
! $HeadURL: https://repos.deltares.nl/repos/ds/branches/dflowfm/20161017_dflowfm_codecleanup/tools_gpl/dfmoutput/src/dfm_merge.F90 $

module netcdf_utils
use netcdf
implicit none

! Copied from official NetCDF: typeSizes.f90
integer, parameter ::   OneByteInt = selected_int_kind(2), &
                        TwoByteInt = selected_int_kind(4), &
                       FourByteInt = selected_int_kind(9), &
                      EightByteInt = selected_int_kind(18)
integer, parameter ::                                          &
                      FourByteReal = selected_real_kind(P =  6, R =  37), &
                     EightByteReal = selected_real_kind(P = 13, R = 307)

!> Compatibility function: returns the fill settings for a variable in a netCDF-3 file. 
interface ncu_inq_var_fill
   module procedure ncu_inq_var_fill_int4
   module procedure ncu_inq_var_fill_real8
end interface ncu_inq_var_fill

contains

!function ncu


!> Copy all attributes from a variable or dataset into another variable/dataset.
!! Returns:
!     nf90_noerr if all okay, otherwise an error code
!!
!! Note: The variable in the output file must already exist.
function ncu_copy_atts( ncidin, ncidout, varidin, varidout ) result(ierr)
   integer, intent(in)            :: ncidin   !< ID of the input NetCDF file
   integer, intent(in)            :: ncidout  !< ID of the output NetCDF file
   integer, intent(in)            :: varidin  !< ID of the variable in the input file, or NF90_GLOBAL for global attributes.
   integer, intent(in)            :: varidout !< ID of the variable in the output file, or NF90_GLOBAL for global attributes.

   integer                        :: ierr
   integer                        :: i

   character(len=nf90_max_name)   :: attname
   integer                        :: natts

   ierr = -1

   ierr = nf90_inquire_variable( ncidin, varidin, nAtts=natts )
   if ( ierr == nf90_enotvar ) then
      ierr = nf90_inquire( ncidin, nAttributes=natts )
   endif
   if ( ierr /= nf90_noerr ) then
      return
   endif

   do i = 1,natts
      ierr = nf90_inq_attname( ncidin, varidin, i, attname )
      if ( ierr /= nf90_noerr ) then
         return
      endif

      ierr = nf90_copy_att( ncidin, varidin, attname, ncidout, varidout )
      if ( ierr /= nf90_noerr ) then
         return
      endif
   enddo

   ierr = nf90_noerr
end function ncu_copy_atts

!> Compatibility function: returns the fill settings for a variable in a netCDF-3 file. 
function ncu_inq_var_fill_int4( ncid, varid, no_fill, fill_value) result(ierr)
   integer,                   intent(in)  :: ncid        !< ID of the NetCDF dataset
   integer,                   intent(in)  :: varid       !< ID of the variable in the data set
   integer,                   intent(out) :: no_fill     !< An integer that will always get 1 (for forward compatibility).
   integer(kind=FourByteInt), intent(out) :: fill_value  !< This will get the fill value for this variable.

   integer :: ierr ! Error status, nf90_noerr = if successful.

   no_fill = 1

   ierr = nf90_get_att(ncid, varid, '_FillValue', fill_value)
   if (ierr /= nf90_noerr) then
      fill_value = nf90_fill_int
      ierr = nf90_noerr
   end if
end function ncu_inq_var_fill_int4


!> Compatibility function: returns the fill settings for a variable in a netCDF-3 file. 
function ncu_inq_var_fill_real8( ncid, varid, no_fill, fill_value) result(ierr)
   integer,                   intent(in)  :: ncid        !< ID of the NetCDF dataset
   integer,                   intent(in)  :: varid       !< ID of the variable in the data set
   integer,                   intent(out) :: no_fill     !< An integer that will always get 1 (for forward compatibility).
   real(kind=EightByteReal),  intent(out) :: fill_value  !< This will get the fill value for this variable.

   integer :: ierr ! Error status, nf90_noerr = if successful.

   no_fill = 1

   ierr = nf90_get_att(ncid, varid, '_FillValue', fill_value)
   if (ierr /= nf90_noerr) then
      fill_value = nf90_fill_int
      ierr = nf90_noerr
   end if
end function ncu_inq_var_fill_real8

end module netcdf_utils

   
module dfm_merge
use netcdf
use netcdf_utils
use dfm_params
implicit none

! TODO: re-use the definitions below from original source: unstruc_netcdf
! The following location codes generalize for 1D/2D/3D models.
integer, parameter :: UNC_LOC_CN = 1  !< Data location: corner point.
integer, parameter :: UNC_LOC_S  = 2  !< Data location: pressure point.
integer, parameter :: UNC_LOC_SN = 102  !< TEMP: Data location: netelem (not always identical to flowelem). (UNST-1256)
integer, parameter :: UNC_LOC_U  = 3  !< Data location: horizontal velocity point.
integer, parameter :: UNC_LOC_L  = 13  !< Data location: net link. TODO: AvD: phase out
integer, parameter :: UNC_LOC_W  = 6  !< Data location: vertical velocity point.
integer, parameter :: UNC_LOC_SBND=7  !< Data location: boundary waterlevel points

contains

!> Merges multiple D-Flow FM map files into a single file.
!! Typically this routine should be used on output of a parallel run,
!! i.e., on files <modelname>_000x_map.nc
function dfm_merge_mapfiles(infiles, nfiles, outfile, force) result(ierr)
   use m_alloc
   use string_module
   implicit none

   character(len=MAXNAMELEN), intent(inout) :: infiles(:) !< Input files names, will be sorted if not sorted already.
   integer,                   intent(in)    :: nfiles     !< Number of input files.
   character(len=MAXNAMELEN), intent(inout) :: outfile    !< Output file name. When empty, the name is derived from the input file names.
   logical,                   intent(in)    :: force      !< Whether to disallow interactive user prompting (for file overwrite)
   integer                                  :: ierr       !< Result status (0 = success)

   integer, dimension(nfiles+1) :: ncids, id_timedim, id_facedim, id_edgedim, id_laydim, id_wdim, id_nodedim, &
                                   id_netedgedim, id_netfacedim, id_netfacemaxnodesdim, id_time, id_timestep, id_bnddim !< dim and var ids, maintained for all input files + 1 output file.
   integer, allocatable :: dimids(:,:) !< (nfiles+1:NF90_MAX_DIMS) Used for storing any remaining vectormax dimension IDs
   integer, dimension(nfiles+1), target :: ndx, lnx, ndxg, lnxg, kmx, numk, numl, nump, numkg, numlg, netfacemaxnodes, nt, ndxbnd !< counters, maintained for all input files + 1 output file.
   integer, dimension(:), pointer :: item_counts !< Generalized count pointer, will point to ndx, lnx, numl, or numk during var data reading + writing.
   integer:: noutfile !< array index/position of output file ids, by default the last, i.e., nfiles + 1.
   integer, allocatable, target  :: face_domain(:), facebnd_domain(:), face_globnr(:), edge_domain(:), node_domain(:), netedge_domain(:) !< Global face/edge/node numbers and their domain number.
   integer,              pointer :: item_domain(:)
   integer, allocatable :: ln(:,:) !< Flow links
   integer, allocatable :: edgenodes(:,:) !< Net links
   integer, allocatable :: netfacenodes(:,:) !< Net cell - to - net node mapping
   integer, allocatable :: netfacenodesl(:,:) !< Net cell - to - net node mapping for local usage (per cell)
   integer, allocatable :: face_c2g(:) !< Concatenated index - to - global index mapping.
   integer, allocatable :: node_g2c(:), edge_g2c(:), face_g2c(:), netedge_g2c(:) !< Global index - to - concatenated index mapping.
!netface_g2c(:) 
   integer :: id_facedomain, id_faceglobnr, id_edgefaces, id_netfacenodes, id_edgenodes
   integer :: ierri
   integer :: maxlen, nlen, plen, mlen, ii, id, iv, it, ip, ik, is, ie, nvarsel, ntsel, nvars, ndims, nvardims, vartype, kmx1,i
   integer :: netfacemaxnodesg, ndxc, lnxc, numkc, numlc,ndx_bndc
   integer :: nfaceglob,    nfaceglob0,    nfacecount
   integer :: nedgeglob,    nedgeglob0,    nedgecount
   integer :: nnodeglob,    nnodeglob0,    nnodecount
   integer :: nbndglob,     nbndcount
!   integer :: nnetfaceglob, nnetfaceglob0, nnetfacecount, numpg
   integer :: nnetedgeglob, nnetedgeglob0, nnetedgecount
   integer :: nitemglob, nitemglob0, nitemcount, maxitems
   integer :: nkmxglob
   
   integer :: idom, n1, n2, n3, k1, k2
   integer :: tmpdimids(NF90_MAX_VAR_DIMS)
   double precision, allocatable, target  :: itmpvar1D(:) !< array buffer for a single global variable slice, size: (kmx1, max(ndx(noutfile),lnx(noutfile)))
   double precision, allocatable, target  :: itmpvar2D(:,:) !< array buffer for a single global variable slice, size: (kmx1, max(ndx(noutfile),lnx(noutfile)))
   double precision,              pointer :: itmpvarptr(:,:,:)
   double precision, allocatable, target  :: tmpvar1D(:) !< array buffer for a single global variable slice, size: (kmx1, max(ndx(noutfile),lnx(noutfile)))
   double precision, allocatable, target  :: tmpvar2D(:,:) !< array buffer for a single global variable slice, size: (kmx1, max(ndx(noutfile),lnx(noutfile)))
   double precision, allocatable, target  :: tmpvar3D(:,:,:) !< array buffer for a single global variable slice, size: (kmx1, max(ndx(noutfile),lnx(noutfile)))
   double precision,              pointer :: tmpvarptr(:,:,:)
   character(len=4) :: fmtstr
   character(len=4096) :: tmpstr1, tmpstr2
   integer,                      allocatable :: varids(:,:)        !< Variable IDs for the selected variables in the input files. Support having different variables in differnt input files.
   integer,                      allocatable :: varids_out(:)      !< Variable IDs for the selected variables in the output file. Will typically not be equal to the var IDs in the input files, as we may not be copying *all* vars from input files.
   character(len=NF90_MAX_NAME), allocatable :: var_names(:)       !< Names of the selected variables.
   integer,                      allocatable :: var_types(:)       !< Data types of the selected variables.
   integer,                      allocatable :: var_dimids(:,:)    !< Dimension ids for selected variables, should be filled later, starting at the end.
   integer,                      allocatable :: var_timdimpos(:)   !< Position in var_dimids(1:4,iv) of time dimension (-1 if not timedep)
   integer,                      allocatable :: var_spacedimpos(:) !< Position in var_dimids(1:4,iv) of space dimension (-1 if not timedep)
   integer,                      allocatable :: var_laydimpos(:)   !< Position in var_dimids(1:4,iv) of layer dimension (-1 if not timedep)
   integer,                      allocatable :: var_kxdimpos(:)    !< Position in var_dimids(1:4,iv) of vectormax dimension (-1 if not timedep)
   integer,                      allocatable :: var_ndims(:)       !< Actual number of dimensions.
   integer,                      allocatable :: var_loctype(:)     !< Spatial location type for each var (face/node/etc.)
   integer,                      allocatable :: var_wdimpos(:)     !< Position in var_dimids(1:4,iv) of layer interface dimension (-1 if not timedep)
   integer,                      allocatable :: file_ndims(:)      !< Nr. dimensions in every input file
   integer,                      allocatable :: dimids_uses(:)     !< Nr. vectormax-like dimensions that are used
   integer :: ivarcandidate, ifirstdim, ilastdim
   integer, parameter :: MAX_VAR_DIMS = 4 !< Max nr of dimensions for a single var. Support: (vectormax, layers, space, time).
   integer, dimension(MAX_VAR_DIMS) :: start_idx   !< Start index array for calling nf90_get_var(..., start=...)
   integer, dimension(MAX_VAR_DIMS) :: count_read  !< Data size array for calling nf90_get_var(..., count=...)
   integer, dimension(MAX_VAR_DIMS) :: count_write !< Data size array for calling nf90_put_var(..., count=...)
   integer :: nofill, ifill_value !< For inquiring fill values

   integer :: id_npartdim !< Dimension ID for the partitions counter
   integer :: id_part_face_start, id_part_edge_start, id_part_node_start, id_part_facebnd_start !< Var IDs for storing start index for each partition in the merged global arrays.
   integer :: id_part_face_count, id_part_edge_count, id_part_node_count, id_part_facebnd_count !< Var IDs for storing count of unique items for each partition in the merged global arrays
   integer :: Lrst_m, ifile, max_nvars
   integer :: isBndLink = 0, id_infile
   
   character(len=NF90_MAX_NAME) :: varname, dimname
   integer,                      allocatable :: itimsel(:)
   double precision,             allocatable :: times(:)
   double precision,             allocatable :: timestep(:)
   logical :: is_timedep, isfound, needshift, exist
   character(len=1) :: answer
   character*8  :: cdate
   character*10 :: ctime
   character*5  :: czone

   if (nfiles <= 1) then
      write (*,'(a)') 'Error: mapmerge: At least two input files required.'
      ierr = 12
   else
      if (verbose_mode) then
         write (*,'(a,i0,a)') 'Info: mapmerge: Starting merge of ', nfiles, ' files...'
      end if
   end if

   noutfile = nfiles+1
   ifile      = 1    ! Initially use the first input file as the reference file for merging 
   nvars      = 0
   max_nvars  = 0
   ncids      = -1
   id_timedim = -1
   id_facedim = -1
   id_edgedim = -1
   id_time    = -1
   id_timestep= -1
   id_laydim  = -1
   id_wdim    = -1
   id_bnddim  = -1
   ndx        =  0
   lnx        =  0
   kmx        =  0
   nt         =  0
   ndxbnd     =  0

   !! 0a. Open input files
   call dfm_order_by_partition(infiles, nfiles)

   ierr = nf90_noerr
   do ii=1,nfiles
      ierri = nf90_open(infiles(ii), NF90_NOWRITE, ncids(ii))
      if (ierri /= nf90_noerr) then
         write (*,'(a)') 'Error: mapmerge: could not open file `'//trim(infiles(ii))//'''.'
         ierr = ierri
         ncids(ii) = -1
      end if
   end do
   if (ierr /= nf90_noerr .and. .not. verbose_mode) then
      goto 888
   end if

   ! Detect too new file format: mapmerge is almost UGRID-ready, but not quite yet.
   tmpstr1 = ''
   ierr = nf90_get_att(ncids(ifile), nf90_global, 'Conventions', tmpstr1)
   if (ierr == nf90_noerr) then
      if (index(tmpstr1(1:80), 'UGRID') > 0) then
         write (*,'(a)') 'Warning: mapmerge: true UGRID-format not supported yet. Detected in `'//trim(infiles(ifile))//'''. Trying anyway.'
      end if
   end if


   
   !! 0b. Open output file
   if (len_trim(outfile) == 0) then
      nlen = len_trim(infiles(1))
      n3 = index(infiles(1)(1:nlen), '.', .true.) - 1  ! pos of '.nc'
      if (n3 < 0) then
         n3 = nlen
      end if
      Lrst_m = index(infiles(1), '_rst.nc')
      if (Lrst_m > 0) then ! If they are _rst files
         n1 = index(infiles(1)(1:Lrst_m), '_0000_', .true.) 
         outfile = infiles(1)(1:n1) //'merged_'// infiles(1)(n1+6:Lrst_m)//'rst.nc'
      else
      n2  = index(infiles(1)(1:n3), '_', .true.) - 1  ! pos of '_map'
      n1  = index(infiles(1)(1:n2), '_', .true.) - 1  ! pos of '_0000'
      if (n1 >= 0) then      ! mdident_0000_typ.nc --> mdident_merged_typ.nc
         outfile = infiles(1)(1:n1) // '_merged_'//infiles(1)(n2+2:n3)//'.nc'
      else if (n2 >= 0) then ! mdident_typ.nc      --> mdident_merged_typ.nc
         outfile = infiles(1)(1:n2) // '_merged_'//infiles(1)(n2+2:n3)//'.nc'
      else                   ! mdident.nc          --> mdident_merged_map.nc
         outfile = infiles(1)(1:n3) // '_merged_map.nc'
      end if
      endif
   end if
   ! Check for possible overwrite of outfile
   inquire(file=trim(outfile), exist=exist)
   if (exist .and. .not. force) then
      write (*, '(a)', advance='no') 'mapmerge: overwrite `'//trim(outfile)//'''? (Y/N) '
      answer = ''
      read (*,'(a)') answer
      if (.not. strcmpi(answer, 'Y')) then
         goto 888
      end if
   end if


   ierr = nf90_create(outfile, ior(NF90_CLOBBER, NF90_64BIT_OFFSET), ncids(noutfile))
   if (ierr /= nf90_noerr) then
      write (*,'(a)') 'Error: mapmerge: could not open file for writing: `'//trim(outfile)//'''.'
      if (.not. verbose_mode) goto 888
   end if


   maxlen = 16
   if (nfiles > 0) then
      maxlen = min(maxlen, maxval(len_trim(infiles(1:nfiles))))
   end if

   !! 1a. Scan for flownode (face), flow link (edge) and time dimensions in input files
   ! Find the input file 'ifile', which has the most variables. This file will be the reference file where later we scan variables.
   allocate(file_ndims(nfiles))
   do ii = 1, nfiles
      ierr = nf90_inquire(ncids(ii), nVariables = nvars )
      if ( ierr /= nf90_noerr ) then
          write (*,'(a)') 'Error: mapmerge: no variables found in file `'//trim(infiles(ii))//'''.'
          if (.not. verbose_mode) goto 888
      else
          ierr = nf90_inquire(ncids(ii), nDimensions = file_ndims(ii) )
          if ( ierr /= nf90_noerr ) then
              write (*,'(a)') 'Error: mapmerge: no dimension found in file `'//trim(infiles(ii))//'''.'
              if (.not. verbose_mode) goto 888
          endif
      endif
      if (nvars > max_nvars) then
         max_nvars = nvars
         ifile = ii   ! Set 'ifile' the one that has the most variables
      endif
   enddo
   nDims = file_ndims(ifile) ! nDims is equal to Nr. dimensions in ifile
   
   allocate(dimids(nDims, nfiles+1))
   dimids = -999 ! missing
   do ii=1,nfiles
      if (ncids(ii) <= 0) then
         if (verbose_mode) then
            write (*,'(a)') 'Warning: mapmerge: Skipping scan of file `'//trim(infiles(ii))//'''.'
         end if
         cycle
      end if

      do id=1,file_ndims(ii)
         ierr = nf90_inquire_dimension(ncids(ii), id, name = dimname, len = nlen) ! NetCDF-F90 allows us to assume that the dim IDs are 1:ndims
         if (ierr /= nf90_noerr) then
            write (*,'(a,i0,a)') 'Error: mapmerge: unable to read dimension information from file `'//trim(infiles(ii))//''' for #', id,'.'
            if (.not. verbose_mode) goto 888
         end if

         if (trim(dimname) == 'nFlowElem') then
         !! Flow nodes (face) dimension
            id_facedim(ii) = id
            ndx(ii)        = nlen

         else if (trim(dimname) == 'nFlowLink') then
         !! Flow links (edge) dimension
            id_edgedim(ii) = id
            lnx(ii)        = nlen

         else if (trim(dimname) == 'laydim') then ! TODO: AvD: also wdim?
            id_laydim(ii) = id
            kmx(ii)       = nlen
         else if (trim(dimname) == 'wdim') then
            id_wdim(ii) = id

      !! Now some Net* related dimensions (in addition to Flow*).

         else if (trim(dimname) == 'nNetElem') then
         !! Net cells (again face) dimension
            id_netfacedim(ii) = id
            nump(ii)          = nlen


         else if (trim(dimname) == 'nNetElemMaxNode') then ! TODO: AvD: now we detect nNetElemMaxNode, but should be not change to nFlowElemMaxNode, now that facedim is the overall counter and netfacedim is hardly used anymore?
            dimids(id, ii) = id ! Store this now, because later it is just a vectormax dim, so should be available in dim filter
            id_netfacemaxnodesdim(ii) = id
            netfacemaxnodes(ii)       = nlen

         else if (trim(dimname) == 'nNetNode') then
         !! Net nodes (node) dimension
            id_nodedim(ii) = id
            numk(ii)       = nlen

         else if (trim(dimname) == 'nNetLink') then
         !! Net links (again edge) dimension
            id_netedgedim(ii) = id
            numl(ii)          = nlen

         else if (trim(dimname) == 'time') then
         !! Time dimension
            id_timedim(ii) = id
            nt(ii)         = nlen
         else if (trim(dimname) == 'nFlowElemBnd') then
         !! Flow nodes (face) boundary points dimension
            id_bnddim(ii) = id
            ndxbnd(ii)   = nlen
            if (verbose_mode) then
               write (*,'(a)') 'Info: mapmerge: find dimension of boundary waterlevel points in file `'//trim(infiles(ii))//'''.'
            endif
         else
            ! No special dimension, so probably just some vectormax-type dimension that
            ! we may need later for some variables, so store it.
            dimids(id, ii) = id ! Only stored to filter on non-missing values in def_dim loop later
         end if
      end do ! id
   end do ! ii

   
   !! 1b. Scan for variables in the file which has the most dimension (and variables).
   if (verbose_mode) then
      write (*,'(a)') 'Info: mapmerge: Scan for variables in file `'//trim(infiles(ifile))//'''.'
   endif
   ierr = nf90_inquire(ncids(ifile), nVariables = nvars )
   if ( ierr /= nf90_noerr ) then
      write (*,'(a)') 'Error: mapmerge: no variables found in file `'//trim(infiles(ifile))//'''.'
      if (.not. verbose_mode) goto 888
   endif

   if (verbose_mode) then
      write (*,'(a)') '## Selecting variables to include in merge:'
   end if

   allocate(varids(nfiles, nvars));
   allocate(varids_out(nvars))
   allocate(var_names(nvars))
   allocate(var_types(nvars))
   allocate(var_dimids(MAX_VAR_DIMS,nvars))
   allocate(var_timdimpos(nvars));   var_timdimpos   = -1
   allocate(var_spacedimpos(nvars)); var_spacedimpos = -1
   allocate(var_laydimpos(nvars));   var_laydimpos   = -1
   allocate(var_kxdimpos(nvars));    var_kxdimpos    = -1
   allocate(var_wdimpos(nvars));     var_wdimpos     = -1
   allocate(var_ndims(nvars));       var_ndims       =  0
   allocate(var_loctype(nvars));     var_loctype     =  0
   allocate(dimids_uses(nDims));     dimids_uses     =  0
   nvarsel = 0
   do iv = 1,nvars
      ierr = nf90_inquire_variable(ncids(ifile), iv, name=varname, xtype=vartype, ndims=nvardims, dimids=tmpdimids)

      if (nvardims == 1) then
         if (tmpdimids(1) == id_timedim(ifile) .and. trim(varname) == 'time') then
            id_time(ifile) = iv ! TODO: AvD: do this for all ii files, not only for 'ifile'
            if (verbose_mode) then
               write (*,'(a)') 'Found time variable in file `'//trim(infiles(ifile))//''': `'//trim(varname)//'''.'
            end if

            cycle ! This was time, continue searching for remaining data variables.
         elseif (tmpdimids(1) == id_timedim(ifile) .and. trim(varname) == 'timestep') then
            id_timestep(ifile) = iv ! TODO: AvD: do this for all ii files, not only for 'ifile'
            cycle
         end if
      end if


      ! It was not time, see if this is a geometry or data variable on flow nodes/links/net nodes:
      ivarcandidate = nvarsel+1
      isfound = .true.
      ilastdim = MAX_VAR_DIMS
      ifirstdim = ilastdim+1 ! dummy start: no dims detected yet.
      do id=nvardims,1,-1
         if (tmpdimids(id) == id_timedim(ifile)) then
            ifirstdim = ifirstdim-1
            var_timdimpos(ivarcandidate) = ifirstdim
         else if (tmpdimids(id) == id_facedim(ifile)) then
            tmpdimids(id) = id_facedim(ifile) ! replace netfacedim by (flow)facedim
            ifirstdim = ifirstdim-1
            var_loctype(ivarcandidate) = UNC_LOC_S
            var_spacedimpos(ivarcandidate) = ifirstdim
         else if (tmpdimids(id) == id_netfacedim(ifile)) then
            tmpdimids(id) = id_netfacedim(ifile) 
            ifirstdim = ifirstdim-1
            var_loctype(ivarcandidate) = UNC_LOC_SN       ! UNST-1256: original UNST_LOC_S caused NetElemNode variable in merged file to be on nFlowElem dimension, whereas input is on nNetElem dimension.
            var_spacedimpos(ivarcandidate) = ifirstdim
         else if (tmpdimids(id) == id_edgedim(ifile)) then
            ifirstdim = ifirstdim-1
            var_loctype(ivarcandidate) = UNC_LOC_U
            var_spacedimpos(ivarcandidate) = ifirstdim
         else if (tmpdimids(id) == id_nodedim(ifile)) then
            ifirstdim = ifirstdim-1
            var_loctype(ivarcandidate) = UNC_LOC_CN
            var_spacedimpos(ivarcandidate) = ifirstdim
         else if (tmpdimids(id) == id_netedgedim(ifile)) then
            ifirstdim = ifirstdim-1
            var_loctype(ivarcandidate) = UNC_LOC_L
            var_spacedimpos(ivarcandidate) = ifirstdim
         else if (tmpdimids(id) == id_laydim(ifile)) then
            ! TODO: do we really not need to set the S3D/W3D here?
            ifirstdim = ifirstdim-1
          !  var_loctype(ivarcandidate) = UNC_LOC_S3D
            var_laydimpos(ivarcandidate) = ifirstdim
         else if (tmpdimids(id) == id_wdim(ifile)) then
            ifirstdim = ifirstdim-1
            !var_loctype(ivarcandidate) = UNC_LOC_W
            var_wdimpos(ivarcandidate) = ifirstdim
         else if (tmpdimids(id) == id_bnddim(ifile)) then
            tmpdimids(id) = id_bnddim(ifile) 
            ifirstdim = ifirstdim-1
            var_loctype(ivarcandidate) = UNC_LOC_SBND
            var_spacedimpos(ivarcandidate) = ifirstdim
         else
            if (var_kxdimpos(ivarcandidate) == -1) then
               ifirstdim = ifirstdim-1
               var_kxdimpos(ivarcandidate) = ifirstdim
               ! count how many times this dimension is used
               id_infile = tmpdimids(id)
               dimids_uses(id_infile) = dimids_uses(id_infile) + 1
            else
               if (verbose_mode) then
                  write (*,'(a)')           'Error: mapmerge: detected more than one vectormax dimension for `'//trim(varname)//''':'
                  write (*,'(a,i0,a,i0,a)') '       current: ', id, ', other: ', var_kxdimpos(ivarcandidate), '. Skipping this variable.'
               end if
               isfound = .false.
               exit ! Stop scanning any remaining dimensions for this var.
            end if
         end if
         var_dimids(ifirstdim,ivarcandidate) = tmpdimids(id) ! for debugging only: temp store the dimids from file #1
      end do ! id

      ! We can only merge a variable across multiple domains *if* it is defined on space locations (face/edge/node)
      ! *or* if it is a special time-related variable. All others impossible to merge.
      if (var_spacedimpos(ivarcandidate) <= 0 .and. var_timdimpos(ivarcandidate) <= 0 .and. nvardims .ne. 0) then
         if (verbose_mode) then
            write (*,'(a)')           'Error: mapmerge: detected that variable `'//trim(varname)//''': is not defined on space' &
               //'locations, and is not a speical time-related variable. Skipping this variable.'
         end if
         ! Make decrement -1 to all dimensions of the skipped variable, i.e. they are used one time less
         ! dimids_uses(id_infile) needs to be decremented -1 here for the kx dim
         do id=nvardims,1,-1
            id_infile = tmpdimids(id)
            dimids_uses(id_infile) = dimids_uses(id_infile) - 1
         enddo
         isfound = .false.
      end if
   
      if (isfound) then
         nvarsel = nvarsel + 1 ! === ivarcandidate
         ! NOTE: Use variable ID from file #1 and assume that all input files contain the same variables, and as such the same consecutive variable IDs.
         varids(ifile, nvarsel) = iv
         var_names(nvarsel) = varname
         var_types(nvarsel) = vartype
         ! NOTE: all dimension positions were already stored, actual dimension ids not,
         ! because those are only needed for output file, will be done later.
         var_ndims(nvarsel) = ilastdim-ifirstdim+1
         if (verbose_mode) then
            tmpstr1 = ''
            nlen=0
            do id=ifirstdim,ilastdim
               ierr = nf90_inquire_dimension(ncids(ifile), var_dimids(id,nvarsel), name=tmpstr2)
               mlen = len_trim(tmpstr2)
               tmpstr1 = tmpstr1(1:nlen)//', '//tmpstr2(1:mlen)
               nlen = nlen + 2 + mlen
            end do
            write (*,'(a,i3,a,a)') ' * ', nvarsel, ': ', trim(varname)//'('//tmpstr1(3:nlen)//')'
         end if
         ! Set IDs of current variable to other input files, if those files does not have this variable, then set ID to -1
         do ii = 1, nfiles
            if (ii .ne. ifile) then
               ierr = nf90_inq_varid(ncids(ii), varname, varids(ii, nvarsel))
               if (ierr .ne. nf90_noerr) then
                  if (verbose_mode) then
                     write (*,'(a)') 'Info: mapmerge: file `'//trim(infiles(ii))//'''does not have variable `'//trim(varname)//'''.'
                  endif
                  ierr = 0
                  varids(ii,nvarsel) = -1
               endif
            endif
         enddo
      end if
   end do ! iv

   if (verbose_mode) then
      if (nvarsel == 0) then
         write (*,'(a)') 'Error: mapmerge: no variables found in file `'//infiles(ifile)//'''.'
         if (.not. verbose_mode) goto 888
      end if
   end if

   !! 2a. Write top level attributes to file as a copy from input file.
   ierr = ncu_copy_atts(ncids(ifile), ncids(noutfile), nf90_global, nf90_global)

   ! We're making history here
   tmpstr1 = ''
   ierr = nf90_get_att(ncids(ifile), nf90_global, 'history', tmpstr1)
   if (ierr /= nf90_noerr) then
      mlen = 0
   else
      mlen = min(len(tmpstr1), len_trim(tmpstr1)+1)
      tmpstr1(mlen:mlen) = char(10) ! Prepare for our extra history line below by adding newline now already.
   end if
   
   call get_command (tmpstr2, nlen, ierr)
   if (ierr < 0) then ! command did not fit into string, abbreviate it.
      tmpstr2(len(tmpstr2)-2:len(tmpstr2)) = '...'
   end if

   call date_and_time(cdate, ctime, czone)
   ierr = nf90_put_att(ncids(noutfile), nf90_global, 'history', &
      tmpstr1(1:mlen)// &
      cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)//': '// &
      tmpstr2(1:nlen))

   ! Don't set 'date_created', hopefully it was in the original input files, and then we copy it and leave it at that source value.
   ierr = nf90_put_att(ncids(noutfile), nf90_global,  'date_modified', cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5))


   !! 2b. Define time dim&var with attributes in outputfile as a copy from input file.
   ierr = nf90_def_dim(ncids(noutfile), 'time', nf90_unlimited, id_timedim(noutfile))
   ierr = nf90_def_var(ncids(noutfile), 'time', nf90_double,    (/ id_timedim(noutfile) /), id_time   (noutfile))
   ierr = ncu_copy_atts(ncids(ifile), ncids(noutfile), id_time(ifile), id_time(noutfile))

   ierr = nf90_def_var(ncids(noutfile), 'timestep', nf90_double,    (/ id_timedim(noutfile) /), id_timestep(noutfile))
   ierr = ncu_copy_atts(ncids(ifile), ncids(noutfile), id_timestep(ifile), id_timestep(noutfile))

   !! 3. Construct merged flow geometry (using proper cellmasks, global numbers, etc.
   !! 3a. Count dimensions (removing partition overlap) for merged flow nodes (faces) and flow links (edges).
   nfacecount    = 0 !< running total of ndx(ii) while iterating the files
   nedgecount    = 0 !< running total of lnx(ii) while iterating the files
   nnodecount    = 0 !< running total of numk(ii) while iterating the files
   !nnetfacecount = 0 !< running total of nump(ii) while iterating the files
   nnetedgecount = 0 !< running total of numl(ii) while iterating the files
   nfaceglob     = 0 !< total number of flow nodes (faces) without duplicates
   nedgeglob     = 0 !< total number of flow links (edges) without duplicates
   nnodeglob     = 0 !< total number of net nodes (nodes) without duplicates
   !nnetfaceglob  = 0 !< total number of net cells (faces) without duplicates
   nnetedgeglob  = 0 !< total number of net links (edges) without duplicates
   nbndglob      = 0 !< total number of boundary waterlevel points without duplicates
   nbndcount     = 0 !< total number of boundary waterlevel points

   ndxc = sum(ndx(1:nfiles))
   call realloc(face_domain, ndxc, keepExisting=.false., fill=-1)
   call realloc(face_c2g,    ndxc, keepExisting=.false.)
   call realloc(face_g2c,    ndxc, keepExisting=.false.)

   lnxc = sum(lnx(1:nfiles))
   call realloc(edge_domain, lnxc, keepExisting=.false., fill=-1)
   call realloc(ln, (/ 2, lnxc /), keepExisting=.false.)
   call realloc(edge_g2c,    lnxc, keepExisting=.false.)

   netfacemaxnodesg = maxval(netfacemaxnodes(1:nfiles))
   call realloc(netfacenodes, (/ netfacemaxnodesg, sum(nump(1:nfiles)) /), keepExisting=.false., fill=-1)

   numkc = sum(numk(1:nfiles))
   call realloc(node_domain, numkc, keepExisting=.false., fill=huge(1))
   call realloc(node_g2c,    numkc, keepExisting=.false.)

   numlc = sum(numl(1:nfiles))
   call realloc(netedge_domain, numlc, keepExisting=.false., fill=-1)
   call realloc(edgenodes, (/ 2, numlc /), keepExisting=.false.)
   call realloc(netedge_g2c,    numlc, keepExisting=.false.)
   
   ndx_bndc = sum(ndxbnd(1:nfiles))
   call realloc(facebnd_domain, ndx_bndc, keepExisting=.false., fill =-1)

   if (verbose_mode) then
      write (*,'(a)') '## Scanning input files for dimensions...'

      write (fmtstr, '(a,i0)') 'a', maxlen
      write (*,'('//trim(fmtstr)//',a3,4(a13),a8)') 'File', &
         ' : ', '   flow nodes', ' | flow links', ' |  net nodes', ' |  net links', ' | times'
   end if
   do ii=1,nfiles
      !! 3a.1: handle flow nodes (faces)
      nfaceglob0 = nfaceglob
      face_domain(nfacecount+1:nfacecount+ndx(ii)) = ii-1 ! Just set default domain if FlowElemDomain not present.
      ierr = nf90_inq_varid(ncids(ii), 'FlowElemDomain', id_facedomain)
      if (ierr == nf90_noerr) then
         ierr = nf90_get_var(ncids(ii), id_facedomain, face_domain(nfacecount+1:nfacecount+ndx(ii)), count=(/ ndx(ii) /))
         if (ierr /= nf90_noerr) then
            write (*,'(a)') 'Error: mapmerge: could not retrieve FlowElemDomain from `'//trim(infiles(ii))//'''. '
            if (.not. verbose_mode) goto 888
         end if
      else
         ! no problem if FlowElemDomain is missing: just include all elems (default ii-1 was set above).
      end if

      if (ierr == nf90_noerr) then
         ierr = nf90_inq_varid(ncids(ii), 'FlowElemGlobalNr', id_faceglobnr)
      end if
      if (ierr == nf90_noerr) then
         ierr = nf90_get_var(ncids(ii), id_faceglobnr, face_c2g(nfacecount+1:nfacecount+ndx(ii)), count=(/ ndx(ii) /))
         if (ierr /= nf90_noerr) then
            write (*,'(a)') 'Error: mapmerge: could not retrieve FlowElemGlobalNr from `'//trim(infiles(ii))//'''. '
            if (.not. verbose_mode) goto 888
         end if
      else
         ! no problem if FlowElemGlobalNr is missing: just include all elems: global nr is equal to 'concat-index'.
         do ip=1,ndx(ii)
            face_c2g(nfacecount+ip) = nfacecount+ip
         end do
      end if

      ! Count the actual unique flow nodes (to get rid of partition overlap)
      do ip=1,ndx(ii)
         if (face_domain(nfacecount+ip) == ii-1) then
            nfaceglob = nfaceglob+1
            face_g2c(nfaceglob) = nfacecount + ip
         end if
      end do
      ndxg(ii)   = nfaceglob-nfaceglob0

      !! 3a.2: handle flow links (edges)
      nedgeglob0 = nedgeglob
      ierr = nf90_inq_varid(ncids(ii), 'FlowLink', id_edgefaces)
      if (ierr == nf90_noerr) then
         ierr = nf90_get_var(ncids(ii), id_edgefaces, ln(:,nedgecount+1:nedgecount+lnx(ii)), count=(/ 2,lnx(ii) /))
      end if
      if (ierr /= nf90_noerr) then
         write (*,'(a)') 'Error: mapmerge: could not retrieve FlowLink from `'//trim(infiles(ii))//'''. '
         if (.not. verbose_mode) goto 888
      end if

      ! Count the actual unique flow links (to get rid of partition overlap, and also of duplicate boundary links)
      do ip=1,lnx(ii)
         ! For each link, take 2nd point (such that boundary links will be uniquely
         ! owned by the domain who owns the internal flow node of that link.)
         n1 = ln(1,nedgecount+ip) ! Could be a boundary point
         n2 = ln(2,nedgecount+ip)
         if (n1 > ndx(ii)) then
            idom = ii - 1 ! Boundary mirrornodes are always owned by the domain itself.
            isBndLink = 1 ! Mark the boundary link
         else
            idom = face_domain(nfacecount+n1)
         end if

         idom = min(idom, face_domain(nfacecount+n2))

         if (isBndLink == 1 .and. face_domain(nfacecount+n2) .ne. ii-1) then
         ! If this boundary link connects an interior flownode which does not belong to the current subdomain
            idom = - 999
         endif
         edge_domain(nedgecount+ip) = idom
         if (idom == ii-1) then
            nedgeglob = nedgeglob+1
            edge_g2c(nedgeglob) = nedgecount + ip
         end if
         isBndLink = 0
      end do
      lnxg(ii)   = nedgeglob-nedgeglob0

      !! 3a.3: handle net nodes (nodes)
      nnodeglob0 = nnodeglob
      ierr = nf90_inq_varid(ncids(ii), 'NetElemNode', id_netfacenodes)
      if (ierr == nf90_noerr) then
!        ierr = nf90_get_var(ncids(ii), id_netfacenodes, netfacenodes(:,nnetfacecount+1:nnetfacecount+nump(ii)), count=(/ netfacemaxnodes(ii), nump(ii) /))
         ierr = ncu_inq_var_fill(ncids(ii), id_netfacenodes, nofill, ifill_value)
         call realloc (netfacenodesl, (/ netfacemaxnodes(ii), nump(ii) /), keepExisting = .false.)
         ierr = nf90_get_var(ncids(ii), id_netfacenodes, netfacenodesl(:,:), count=(/ netfacemaxnodes(ii), nump(ii) /))
         do ip=1,nump(ii)
           netfacenodes(1:netfacemaxnodes(ii),nfacecount+ip) = netfacenodesl(1:netfacemaxnodes(ii),ip)
         end do
      end if
      if (ierr /= nf90_noerr) then
         write (*,'(a)') 'Error: mapmerge: could not retrieve NetElemNode from `'//trim(infiles(ii))//'''. '
         if (.not. verbose_mode) goto 888
      end if

      ! Identify the node domain based on the already processed net cells
      do ip=1,nump(ii)
         do ik=1,netfacemaxnodesg
            k1 = netfacenodes(ik, nfacecount+ip)
            if (k1 == -1 .or. k1 == ifill_value) then
               exit
            end if
            ! Owner of the node will be the lowest domain number of any of the cells surrounding that node.
            node_domain(nnodecount+k1) = min(node_domain(nnodecount+k1), face_domain(nfacecount+ip))
         end do
      end do
      !numpg(ii)   = nnetfaceglob-nnetfaceglob0

      ! Count the actual unique nodes (to get rid of partition overlap)
      do ip=1,numk(ii)
         idom = node_domain(nnodecount+ip)

         if (idom == ii-1) then ! Second check is to identify orphan nodes (in case no 1D netcells were available on file)
            nnodeglob = nnodeglob+1 ! TODO: orphan nodes are not handled correctly yet for 1D channel strings (duplicates?)
            node_g2c(nnodeglob) = nnodecount + ip
         end if
      end do
      numkg(ii)   = nnodeglob-nnodeglob0

      !! 3a.4: handle net edges (also edges)
      nnetedgeglob0 = nnetedgeglob
      ierr = nf90_inq_varid(ncids(ii), 'NetLink', id_edgenodes)
      if (ierr == nf90_noerr) then
         ierr = nf90_get_var(ncids(ii), id_edgenodes, edgenodes(:,nnetedgecount+1:nnetedgecount+numl(ii)), count=(/ 2, numl(ii) /))
      end if
      if (ierr /= nf90_noerr) then
         write (*,'(a)') 'Error: mapmerge: could not retrieve NetLink from `'//trim(infiles(ii))//'''. '
         if (.not. verbose_mode) goto 888
      end if

      ! Identify the net link node domain based on the already processed net nodes
      do ip=1,numl(ii)
         k1 = edgenodes(1,nnetedgecount+ip)
         k2 = edgenodes(2,nnetedgecount+ip)
         ! NOTE: AvD: by taking the MAX below, I believe this also guarantees that one and
         !            the same domain owns both the net link and the associated flow link.
         idom = max(node_domain(nnodecount+k1), node_domain(nnodecount+k2))

         netedge_domain(nnetedgecount+ip) = idom

         if (idom == ii-1) then
            nnetedgeglob = nnetedgeglob+1
            netedge_g2c(nnetedgeglob) = nnetedgecount + ip
         end if
      end do
      numlg(ii)   = nnetedgeglob-nnetedgeglob0
      
      !! 3a.5: handle boundary waterlevel points
      if (ndxbnd(ii) > 0) then
         facebnd_domain(nbndcount+1:nbndcount+ndxbnd(ii)) = ii-1
         nbndglob = nbndglob + ndxbnd(ii)
      else if (verbose_mode) then
            write (*,'(a)') 'Info: mapmerge: no waterlevel boundary in `'//trim(infiles(ii))//'''. '
      endif
      
      ! Intentional: all of these need to be done at very last instant:
      nedgecount    = nedgecount    + lnx(ii)
      nfacecount    = nfacecount    + ndx(ii)
      nnodecount    = nnodecount    + numk(ii)
      nnetedgecount = nnetedgecount + numl(ii)
      !nnetfacecount = nnetfacecount + nump(ii)
      nbndcount     = nbndcount     + ndxbnd(ii)

      if (verbose_mode) then
         nlen = len_trim(infiles(ii))
         plen = 3*max(0,sign(1,nlen-maxlen-1))

         write (fmtstr, '(a,i0)') 'a', maxlen
         write (*,'('//trim(fmtstr)//',a3,i7,3(i13),i8)') repeat('.', plen)//infiles(ii)(max(1,nlen-maxlen+1)+plen:nlen)//repeat(' ', max(0,maxlen-nlen-plen)), &
            ' : ', ndx(ii), lnx(ii), numk(ii), numl(ii), nt(ii)
         write (*,'('//trim(fmtstr)//',a3,4(i13))')    repeat(' ', maxlen-7)//'Removed', &
            ' : ', (ndxg(ii)-ndx(ii)), (lnxg(ii)-lnx(ii)), (numkg(ii)-numk(ii)), (numlg(ii)-numl(ii))
      end if
   end do ! ii
   
   nkmxglob = kmx(1)
   do ii = 2,nfiles
      if (kmx(ii) .ne. nkmxglob) then
         write(*, '(a)') 'Error: Different layers are found in subdomains.'
         if (.not. verbose_mode) goto 888
         if (kmx(ii) > nkmxglob) then
            nkmxglob = kmx(ii)
         endif
      endif
   enddo
   
   ndx (noutfile) = nfaceglob
   lnx (noutfile) = nedgeglob
   numk(noutfile) = nnodeglob
   numl(noutfile) = nnetedgeglob
   kmx (noutfile) = nkmxglob
   ndxbnd(noutfile) = nbndglob

   if (verbose_mode) then
      nlen = len_trim(outfile)
      plen = 3*max(0,sign(1,nlen-maxlen-1))

      write (fmtstr, '(a,i0)') 'a', maxlen
      write (*,'('//trim(fmtstr)//',a3,4(i13))')    repeat(' ', maxlen-7)//'Net sum', &
         ' : ', sum(ndxg(1:nfiles)), sum(lnxg(1:nfiles)), sum(numkg(1:nfiles)), sum(numlg(1:nfiles))
      write (*,'('//trim(fmtstr)//',a3,4(i13))')    repeat(' ', maxlen-11)//'Check count', &
         ' : ', ndx(noutfile), lnx(noutfile), numk(noutfile), numl(noutfile)
   end if

   !! 3b. dimensions for merged flow nodes (faces) and flow links (edges), same for net nodes + links.
   ierr = nf90_def_dim(ncids(noutfile), 'nNetElem', ndx(noutfile), id_netfacedim(noutfile)) ! UNST-1256: temp fix, pending proper UGRID support in UNST-1134.
   ierr = nf90_def_dim(ncids(noutfile), 'nFlowElem', ndx(noutfile), id_facedim(noutfile))
   ierr = nf90_def_dim(ncids(noutfile), 'nFlowLink', lnx(noutfile), id_edgedim(noutfile))
   if (kmx(noutfile) > 0) then
      ierr = nf90_def_dim(ncids(noutfile), 'laydim', kmx(noutfile), id_laydim(noutfile))
      ierr = nf90_def_dim(ncids(noutfile), 'wdim', kmx(noutfile)+1, id_wdim(noutfile))
   end if
   ierr = nf90_def_dim(ncids(noutfile), 'nNetNode', numk(noutfile), id_nodedim(noutfile))
   ierr = nf90_def_dim(ncids(noutfile), 'nNetLink', numl(noutfile), id_netedgedim(noutfile))
   ierr = nf90_def_dim(ncids(noutfile), 'nFlowElemBnd', ndxbnd(noutfile), id_bnddim(noutfile))! TODO: Add if ndxbnd > 0

   !! 4. Define all variables (grid + data), including any remaining dimensions
   !! 4a. Simply copy all remaining dimensions (probably vectormax-like) to output file.
   do id=1,ndims
      if (dimids(id,ifile) > 0) then ! For now, just copy the vectormax dimids (if any) from file #1 to output file. Assume same length in all files.
         ierr = nf90_inquire_dimension(ncids(ifile), dimids(id,ifile), name = dimname, len = nlen)
         if (dimids_uses(id) <= 0) then
            write (*,'(a)') 'Info: mapmerge: Dimension `'//trim(dimname)//''' is not merged because no merged variable uses it. '
            cycle
         endif
         if (ierr == nf90_noerr) then
            ierr = nf90_def_dim(ncids(noutfile), trim(dimname), nlen, dimids(id, noutfile))
         end if
         if (ierr /= nf90_noerr) then
            write (*,'(a,i0,a,i0)') 'Error: mapmerge: Could not copy dimension #', dimids(id,ifile), &
                                    ' from `'//trim(infiles(ifile))//''' into output file `'//trim(outfile)//'''. Error: ', ierr
            if (.not. verbose_mode) goto 888
         end if
      end if
   end do

   !! 4b. Define all variables, based on previously detected dimension information.
   ! var_dimids = -1 ! NOTE: can't do this, as we still need the vectormax dimensions that are stored in here.
   do iv=1,nvarsel
!      ierr = nf90_inquire_var(ncids(1), varids(iv)
!      ndims = 2
      !dimids(1) = id_facedim(noutfile)
      !dimids(2) = id_timedim(noutfile)

      ip = var_timdimpos(iv)
      if (ip /= -1) then
         var_dimids(ip,iv) = id_timedim(noutfile)
      end if
      ip = var_spacedimpos(iv)
      if (ip /= -1) then
         if (var_loctype(iv) == UNC_LOC_S) then
            var_dimids(ip,iv) = id_facedim(noutfile)
         else if (var_loctype(iv) == UNC_LOC_SN) then
            var_dimids(ip,iv) = id_netfacedim(noutfile)
         else if (var_loctype(iv) == UNC_LOC_U) then
            var_dimids(ip,iv) = id_edgedim(noutfile)
         else if (var_loctype(iv) == UNC_LOC_CN) then
            var_dimids(ip,iv) = id_nodedim(noutfile)
         else if (var_loctype(iv) == UNC_LOC_L) then
            var_dimids(ip,iv) = id_netedgedim(noutfile)
         else if (var_loctype(iv) == UNC_LOC_SBND) then
            var_dimids(ip,iv) = id_bnddim(noutfile)
         else
            write (*,'(a,i0,a)') 'Error: mapmerge: Unknown location type ', var_loctype(iv), ' for `'//trim(var_names(iv))//'''.'
         end if
      end if
      ip = var_laydimpos(iv)
      if (ip /= -1) then
         var_dimids(ip,iv) = id_laydim(noutfile)
      end if
      ip = var_wdimpos(iv)
      if (ip /= -1) then
         var_dimids(ip,iv) = id_wdim(noutfile)
      end if
      ip = var_kxdimpos(iv)
      if (ip /= -1) then
         var_dimids(ip,iv) = dimids(var_dimids(ip,iv), noutfile) ! this is necessary because in outfile dim IDs will be in different order, even if *all* dim ids have been copied
         ! Dim ID for this var in outfile === based on *pos* in dimids(:) for *this* noutfile.
      end if

      ierr = nf90_def_var(ncids(noutfile), var_names(iv), var_types(iv), var_dimids(4-var_ndims(iv)+1:4,iv), varids_out(iv))

      if (ierr /= nf90_noerr) then
         write (*,'(a)') 'Error: mapmerge: could not create output variable `'//trim(var_names(iv))//'''. '
         write (*,'(a)')        'Details : '//trim(nf90_strerror(ierr))
         if (.not. verbose_mode) goto 888
         varids_out(iv) = -1
         cycle
      end if

      ierr = ncu_copy_atts(ncids(ifile), ncids(noutfile), varids(ifile, iv), varids_out(iv))
   end do
   

   ierr = nf90_put_att(ncids(noutfile), nf90_global, 'NumPartitionsInFile', nfiles)
   ierr = nf90_def_dim(ncids(noutfile), 'nPartitions', nfiles, id_npartdim)
   ierr = nf90_def_var(ncids(noutfile), 'partitions_face_start', nf90_int, (/ id_npartdim /), id_part_face_start)
   ierr = nf90_def_var(ncids(noutfile), 'partitions_edge_start', nf90_int, (/ id_npartdim /), id_part_edge_start)
   ierr = nf90_def_var(ncids(noutfile), 'partitions_node_start', nf90_int, (/ id_npartdim /), id_part_node_start)
   ierr = nf90_put_att(ncids(noutfile), id_part_face_start, 'long_name', 'start index in global data arrays for face data for all partititions')
   ierr = nf90_put_att(ncids(noutfile), id_part_edge_start, 'long_name', 'start index in global data arrays for edge data for all partititions')
   ierr = nf90_put_att(ncids(noutfile), id_part_node_start, 'long_name', 'start index in global data arrays for node data for all partititions')
   ! NOTE: AvD: intentionally not adding netedge here, as I want to phase it out.
   ierr = nf90_def_var(ncids(noutfile), 'partitions_face_count', nf90_int, (/ id_npartdim /), id_part_face_count)
   ierr = nf90_def_var(ncids(noutfile), 'partitions_edge_count', nf90_int, (/ id_npartdim /), id_part_edge_count)
   ierr = nf90_def_var(ncids(noutfile), 'partitions_node_count', nf90_int, (/ id_npartdim /), id_part_node_count)
   ierr = nf90_put_att(ncids(noutfile), id_part_face_count, 'long_name', 'per-partition count in global data arrays for face data')
   ierr = nf90_put_att(ncids(noutfile), id_part_edge_count, 'long_name', 'per-partition count in global data arrays for edge data')
   ierr = nf90_put_att(ncids(noutfile), id_part_node_count, 'long_name', 'per-partition count in global data arrays for node data')
   if (nbndglob>0) then
      ierr = nf90_def_var(ncids(noutfile), 'partitions_facebnd_count', nf90_int, (/ id_npartdim /), id_part_facebnd_count)
      ierr = nf90_put_att(ncids(noutfile), id_part_facebnd_count, 'long_name', 'per-partition count in global data arrays for boundary points data')
      ierr = nf90_def_var(ncids(noutfile), 'partitions_facebnd_start', nf90_int, (/ id_npartdim /), id_part_facebnd_start)
      ierr = nf90_put_att(ncids(noutfile), id_part_facebnd_start, 'long_name', 'start index in global data arrays for boundary points for all partititions')
   endif
   ! 4. Write new flow geom to output file
   ! 5. Write all timedep fields from all files into output file

   ierr = nf90_enddef(ncids(noutfile))

   ! For now: do all times.
   ntsel = nt(ifile)
   nt(noutfile) = ntsel
   allocate(itimsel(ntsel))
   itimsel = (/ (it, it=1,ntsel) /)
   allocate(times(ntsel), timestep(ntsel))
   do it=1,ntsel
      ierr = nf90_get_var(ncids(ifile), id_time(ifile), times(it), start = (/ itimsel(it) /)) ! count=1
      ierr = nf90_get_var(ncids(ifile), id_timestep(ifile), timestep(it), start = (/ itimsel(it) /)) ! count=1
   end do

   ierr = nf90_put_var(ncids(noutfile), id_time(noutfile), times, count = (/ ntsel /))
   ierr = nf90_put_var(ncids(noutfile), id_timestep(noutfile), timestep, count = (/ ntsel /))

   if (verbose_mode) then
      write (*,'(a)') '## Writing merged variables to output file...'
   end if

   ! 1D tmp array: take largest of all topological position counts:
   maxitems = max(nedgecount, nfacecount, nnodecount, nnetedgecount, nbndcount) 
   call realloc( tmpvar1D, maxitems, keepExisting=.false.)
   call realloc(itmpvar1D, maxitems, keepExisting=.false.)
   ! 2D/3D done in loop below

   do iv=1,nvarsel
      if (verbose_mode) then
         write (tmpstr1, '(a,i0,a,i0,a)') 'Var #', iv, ' of ', nvarsel, ': '
      end if
      if (var_ndims(iv) == 0) then  ! For instance, 'Mesh2D'
         cycle
      else if (var_spacedimpos(iv) == -1 .and. var_timdimpos(iv) == -1 .and. var_kxdimpos(iv) /= -1) then
         ! Some unknown non-space and non-time dimension: impossible to merge in a generic way. Skip it.
         write (*,'(a)') 'Warning: mapmerge: cannot merge vars with non-space/time dimensions: `'//trim(var_names(iv))//'''. Skipping.'
         cycle
         ! TODO: AvD: read + write timestep variable and similar here.
      end if

      is = MAX_VAR_DIMS-var_ndims(iv)+1
      ie = MAX_VAR_DIMS
      start_idx  (is:ie) = 1 ! For all relevant dimensions for this var: start indices to be set below
      count_read (is:ie) = 1 ! For all relevant dimensions for this var: read  counts to be set below
      count_write(is:ie) = 1 ! For all relevant dimensions for this var: write counts to be set below

      ! 5a.1 Optional kx/vectormax is the same for all files, for all times:
      if (var_kxdimpos(iv) /= -1) then
         id = var_dimids(var_kxdimpos(iv), iv) ! Dim ID for this kx dim in outfile
         ierr = nf90_inquire_dimension(ncids(noutfile), id, len = nlen)
         if (ierr /= nf90_noerr) then
            write (*,'(a,i0,a,a,a)') 'Error: mapmerge: Could not inquire vectormax dimension #', id, ' for variable `', trim(var_names(iv)), '''.'
            cycle ! iv
         end if
         count_read (var_kxdimpos(iv)) = nlen
         count_write(var_kxdimpos(iv)) = nlen
      end if

      ! 5a.2 Optional kmx/layer dim is assumed to be the same for all files, for all times:
      if (var_laydimpos(iv) /= -1) then
         count_read (var_laydimpos(iv)) = kmx(noutfile)
         count_write(var_laydimpos(iv)) = kmx(noutfile)
         ! TODO: AvD: UNST-993: support w-position quantities: difference between kmx and kmx1 !!
         ! TODO: AvD: future work: what if kmx varies between input files?
      end if
      if (var_wdimpos(iv) /= -1) then
         count_read (var_wdimpos(iv)) = kmx(noutfile) +1
         count_write(var_wdimpos(iv)) = kmx(noutfile) +1
      end if

      ! 5a.3 Prepare for space dimension, by pointers to proper face/edge/node/netedge variables
      if ( var_spacedimpos(iv) /= -1) then
      select case (var_loctype(iv))
      case (UNC_LOC_S)
         item_counts => ndx
         item_domain => face_domain
      case (UNC_LOC_SN)
         item_counts => ndx
         item_domain => face_domain
      case (UNC_LOC_U)
         item_counts => lnx
         item_domain => edge_domain
      case (UNC_LOC_CN)
         item_counts => numk
         item_domain => node_domain
      case (UNC_LOC_L)
         item_counts => numl
         item_domain => netedge_domain
      case (UNC_LOC_SBND)
         item_counts => ndxbnd
         item_domain => facebnd_domain
      case default
         ! TODO: Handle non-space dependent vars
         if (var_ndims(iv) > 0) then
            write (*,'(a)') 'Warning: mapmerge: cannot write space-independent vars: `'//trim(var_names(iv))//'''. Skipping.'
         else
            if (verbose_mode) then
               call progress(tmpstr1, 100) ! generate the progress bar.
            end if
         end if

         cycle
      end select
      count_write(var_spacedimpos(iv)) = item_counts(noutfile)
      endif
      
      ! NOTE: AvD: below we assume that order is kx, kmx, ndx, nt, so not as generic anymore as the var_*dimpos analysis would allow.
      ! Allocate the proper memory space for nf90_get_var without risk of stack overflows in the netcdf lib

      if (var_types(iv) /= nf90_double .and. var_types(iv) /= nf90_int .and. var_types(iv) /= nf90_short) then
         write (*,'(a,i0,a)') 'Error: mapmerge: encountered unsupported data type ', var_types(iv), ' for variable `'//trim(var_names(iv))//'''.'
         if (.not. verbose_mode) goto 888
      end if

      if (var_kxdimpos(iv) == -1 .and. var_laydimpos(iv) == -1  .and. var_wdimpos(iv) == -1) then ! 1D array with no layers and no vectormax (possibly time-dep)
         ! Already allocated at max(lnx, ndx, numk, numl), no risk of stack overflow
         if (var_types(iv) == nf90_double) then
            tmpvarptr(1:1,1:1,1:maxitems)  =>  tmpvar1D(:)
         else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
            itmpvarptr(1:1,1:1,1:maxitems) => itmpvar1D(:)
         end if

      else if (var_kxdimpos(iv) /= -1) then
         if (var_laydimpos(iv) /= -1) then   ! Both a vectormax AND a laydim
            call realloc(tmpvar3D, (/  count_read(var_kxdimpos(iv)), count_read(var_laydimpos(iv)), maxitems /), keepExisting=.false.)
            ! use maxitems instead of items_count(noutfile) to try and have as few reallocs as possible.
            tmpvarptr => tmpvar3D
         else                                ! Only a vectormax dim
            if (var_types(iv) == nf90_double) then
               call realloc( tmpvar2D, (/  count_read(var_kxdimpos(iv)), maxitems /), keepExisting=.false.)
               tmpvarptr(1:count_read(var_kxdimpos(iv)),1:1,1:maxitems)  =>  tmpvar2D(:,:)
            else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
               call realloc(itmpvar2D, (/  count_read(var_kxdimpos(iv)), maxitems /), keepExisting=.false.)
               itmpvarptr(1:count_read(var_kxdimpos(iv)),1:1,1:maxitems) => itmpvar2D(:,:)
            end if
         end if
      else if (var_laydimpos(iv) /= -1) then ! Only a laydim
         if (var_types(iv) == nf90_double) then
            call    realloc(tmpvar2D, (/  kmx(noutfile), maxitems /), keepExisting=.false.)
            tmpvarptr(1:1,1:kmx(noutfile),1:maxitems)  =>  tmpvar2D(:,:)
         else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
            call    realloc(itmpvar2D, (/  kmx(noutfile), maxitems /), keepExisting=.false.)
            itmpvarptr(1:1,1:kmx(noutfile),1:maxitems) => itmpvar2D(:,:)
         end if
      else if (var_wdimpos(iv) /= -1) then
         if (var_types(iv) == nf90_double) then
            call    realloc(tmpvar2D, (/  kmx(noutfile)+1, maxitems /), keepExisting=.false.)
            tmpvarptr(1:1,1:kmx(noutfile)+1,1:maxitems)  =>  tmpvar2D(:,:)
         else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
            call    realloc(itmpvar2D, (/  kmx(noutfile)+1, maxitems /), keepExisting=.false.)
            itmpvarptr(1:1,1:kmx(noutfile)+1,1:maxitems) => itmpvar2D(:,:)
         end if
      end if

      do it=1,ntsel
         if (verbose_mode) then
            call progress(tmpstr1, ceiling((it-1)*100.0/ntsel)) ! generate the progress bar.
         end if

         ! 5a.4 Time dimension: Which timestep to read from input files
         if (var_timdimpos(iv) /= -1) then
            start_idx(var_timdimpos(iv)) = itimsel(it)
         end if

         nitemglob  = 0
         nitemcount = 0
         do ii=1,nfiles
            nitemglob0 = nitemglob

            if (var_spacedimpos(iv) /= -1) then
               if (item_counts(ii) == 0) then
                  cycle
               else
                  count_read(var_spacedimpos(iv)) = item_counts(ii) ! How many flow face/edges/nodes to read from file #ii
               endif
            end if

            ! Do the actual reading
            if (var_kxdimpos(iv) == -1 .and. var_laydimpos(iv) == -1  .and. var_wdimpos(iv) == -1) then ! 1D array with no layers and no vectormax (possibly time-dep)
               if (var_types(iv) == nf90_double) then
                  ierr = nf90_get_var(ncids(ii), varids(ii,iv), tmpvar1D(    nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
               else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
                  ierr = nf90_get_var(ncids(ii), varids(ii,iv), itmpvar1D(    nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
               end if
            else if (var_kxdimpos(iv) /= -1 .neqv. var_laydimpos(iv) /= -1) then ! Either a vectormax OR a laydim
               if (var_types(iv) == nf90_double) then
                  ierr = nf90_get_var(ncids(ii), varids(ii,iv),  tmpvar2D(  :,nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
               else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
                  ierr = nf90_get_var(ncids(ii), varids(ii,iv), itmpvar2D(  :,nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
               end if
             else if (var_kxdimpos(iv) /= -1 .neqv. var_wdimpos(iv) /= -1) then ! Either a vectormax OR a wdim
               if (var_types(iv) == nf90_double) then
                  ierr = nf90_get_var(ncids(ii), varids(ii,iv),  tmpvar2D(  :,nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
               else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
                  ierr = nf90_get_var(ncids(ii), varids(ii,iv), itmpvar2D(  :,nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
               end if
            else ! Both a vectormax AND a laydim
               if (var_types(iv) == nf90_double) then
                  ierr = nf90_get_var(ncids(ii), varids(ii,iv), tmpvar3D(:,:,nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
               end if
            end if
            if (ierr /= nf90_noerr) then
               write (*,'(a,i0,a)') 'Error: mapmerge: could not read `'//trim(var_names(iv))//''' from file `'//trim(infiles(ii))//''' (it=', itimsel(it), ').'
               if (.not. verbose_mode) goto 888
            end if

            ! Now shift all items (in space) that really belong to *current* domain ii to the left,
            ! such that global item (face/edge/node) nrs form one increasing range in tmpvar.
            needshift = .false. ! The get_var above started at the right place, so no shifting needed yet.
            if (var_types(iv) == nf90_double) then ! TODO: AvD: try to remove this ugly code-copy for just different types
               do ip=1,item_counts(ii)
                  if (item_domain(nitemcount+ip) == ii-1) then
                     nitemglob = nitemglob+1
                     if (needshift) then
                        tmpvarptr(:,:,nitemglob) = tmpvarptr(:,:,nitemglob0+ip)
                     end if
                  else
                     needshift = .true. ! From now on, all points from this var/file need one or more shifts to the left.
                  end if
               end do
            else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
               do ip=1,item_counts(ii)
                  if (item_domain(nitemcount+ip) == ii-1) then
                     nitemglob = nitemglob+1
                     if (needshift) then
                        itmpvarptr(:,:,nitemglob) = itmpvarptr(:,:,nitemglob0+ip)
                     end if
                  else
                     needshift = .true. ! From now on, all points from this var/file need one or more shifts to the left.
                  end if
               end do
            end if
            nitemcount = nitemcount + item_counts(ii)
         end do ! ii

         if (item_counts(noutfile) /= nitemglob) then
            write (*,'(a,i0,a,i0,a)') 'Error: mapmerge: accumulated ', nitemglob, ' items, but expected ', item_counts(noutfile), ', for `'//var_names(iv)//'''.'
            if (.not. verbose_mode) goto 888
         end if
         !! tmpvar is now filled with 1 var, 1 time, across all domains, without overlap, so write it now:
         if (var_kxdimpos(iv) == -1 .and. var_laydimpos(iv) == -1 .and. var_wdimpos(iv) == -1) then ! 1D array with no layers and no vectormax (possibly time-dep)
            if (var_types(iv) == nf90_double) then
               ierr = nf90_put_var(ncids(noutfile), varids_out(iv), tmpvar1D, count = count_write(is:ie), start = start_idx(is:ie))
            else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
               ierr = nf90_put_var(ncids(noutfile), varids_out(iv), itmpvar1D, count = count_write(is:ie), start = start_idx(is:ie))
            end if
         else if (var_kxdimpos(iv) /= -1 .neqv. var_laydimpos(iv) /= -1) then ! Either a vectormax OR a laydim
            if (var_types(iv) == nf90_double) then
               ierr = nf90_put_var(ncids(noutfile), varids_out(iv), tmpvar2D, count = count_write(is:ie), start = start_idx(is:ie))
            else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
               ierr = nf90_put_var(ncids(noutfile), varids_out(iv), itmpvar2D, count = count_write(is:ie), start = start_idx(is:ie))
            end if
         else if (var_kxdimpos(iv) /= -1 .neqv. var_wdimpos(iv) /= -1) then ! Either a vectormax OR a laydim
            if (var_types(iv) == nf90_double) then
               ierr = nf90_put_var(ncids(noutfile), varids_out(iv), tmpvar2D, count = count_write(is:ie), start = start_idx(is:ie))
            else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
               ierr = nf90_put_var(ncids(noutfile), varids_out(iv), itmpvar2D, count = count_write(is:ie), start = start_idx(is:ie))
            end if
         else ! Both a vectormax AND a laydim
            if (var_types(iv) == nf90_double) then
               ierr = nf90_put_var(ncids(noutfile), varids_out(iv), tmpvar3D, count = count_write(is:ie), start = start_idx(is:ie))
            end if
         end if
         !if (kmx(noutfile) > 0) then
         !   ierr = nf90_put_var(ncids(noutfile), varids_out(iv), tmpvar, count = (/ kmx(noutfile), ndx(noutfile), 1 /), start = (/ 1, 1, it /))
         !else
         !   ierr = nf90_put_var(ncids(noutfile), varids_out(iv), tmpvar, count = (/ ndx(noutfile), 1 /), start = (/ 1, it /))
         !end if
         !if (ierr /= nf90_noerr) then
         !   write (*,'(a,i0,a)') 'Error: mapmerge: could not write merged variable `'//trim(var_names(iv))//''' into output file `'//trim(outfile)//''' (itime=', it, ').'
         !   if (.not. verbose_mode) goto 888
         !end if

         ! Check: if this was time-independent variable, first iteration-step was enough for reading+writing.
         if (var_timdimpos(iv) == -1) then
            exit ! it
         end if

      end do ! it
      if (verbose_mode) then
         call progress(tmpstr1, 100) ! generate the progress bar.
      end if

   end do ! iv

   
   ! 6. Write some useful meta info on all merged domains into the output file

   nfacecount = 0
   nedgecount = 0
   nnodecount = 0   

   do ii=1,nfiles
      ierr = nf90_put_var(ncids(noutfile), id_part_face_start, nfacecount+1, start=(/ ii /))
      ierr = nf90_put_var(ncids(noutfile), id_part_edge_start, nedgecount+1, start=(/ ii /))
      ierr = nf90_put_var(ncids(noutfile), id_part_node_start, nnodecount+1, start=(/ ii /))

      nfacecount = nfacecount + ndxg (ii)
      nedgecount = nedgecount + lnxg (ii)
      nnodecount = nnodecount + numkg(ii)  
   end do
   ierr = nf90_put_var(ncids(noutfile), id_part_face_count, ndxg (1:nfiles))
   ierr = nf90_put_var(ncids(noutfile), id_part_edge_count, lnxg (1:nfiles))
   ierr = nf90_put_var(ncids(noutfile), id_part_node_count, numkg(1:nfiles))
   
   if (nbndglob>0) then
      nbndcount  = 0
      do ii=1,nfiles
         ierr = nf90_put_var(ncids(noutfile), id_part_facebnd_start, nbndcount+1, start=(/ ii /))
         nbndcount = nbndcount  + ndxbnd(ii)
      enddo
      ierr = nf90_put_var(ncids(noutfile), id_part_facebnd_count, ndxbnd(1:nfiles))
   endif


   ! Success:
   ierr = 0

   !! Cleanup:
888 continue
   do ii=1,nfiles
      ierr = nf90_close(ncids(ii))
   end do
   ierr = nf90_close(ncids(noutfile))
   
   if (allocated(varids))          deallocate(varids)
   if (allocated(varids_out))      deallocate(varids_out)
   if (allocated(var_names))       deallocate(var_names)
   if (allocated(var_types))       deallocate(var_types)
   if (allocated(var_dimids))      deallocate(var_dimids)
   if (allocated(var_timdimpos))   deallocate(var_timdimpos)
   if (allocated(var_spacedimpos)) deallocate(var_spacedimpos)
   if (allocated(var_laydimpos))   deallocate(var_laydimpos)
   if (allocated(var_kxdimpos))    deallocate(var_kxdimpos)
   if (allocated(var_ndims))       deallocate(var_ndims)
   if (allocated(var_loctype))     deallocate(var_loctype)
   if (allocated(itmpvar1D))       deallocate(itmpvar1D)
   if (allocated(itmpvar2D))       deallocate(itmpvar2D)
   if (allocated(tmpvar1D))        deallocate(tmpvar1D)
   if (allocated(tmpvar2D))        deallocate(tmpvar2D)
   if (allocated(tmpvar3D))        deallocate(tmpvar3D)
end function dfm_merge_mapfiles

!> Orders a filename list by increasing partition number.
!! Sorting is done in place.
subroutine dfm_order_by_partition(files, nfiles)
   use m_alloc
   implicit none

   character(len=MAXNAMELEN), intent(inout) :: files(:)   !< List files names, will be replaced by the sorted list.
   integer,                   intent(in)    :: nfiles     !< Number of input files.

   integer,                   allocatable :: idom(:)
   character(len=MAXNAMELEN), allocatable :: filesorig(:)
   integer :: ii, ierr, nlen

   allocate(idom(nfiles))
   allocate(filesorig(nfiles))
   
   do ii=1,nfiles
      filesorig(ii) = files(ii)
      nlen = len_trim(files(ii))
      ! modelname_xxxx_map.nc'
      !           0  76  3  0
      if (files(ii)(max(1,nlen-6):nlen) == '_map.nc') then
         read (files(ii)(max(1,nlen-10):nlen-7), '(i4.4)', iostat=ierr) idom(ii)
      else
         ierr = 1
      end if
      if (ierr /= 0) then
         idom(ii) = ii-1
      end if
   end do

   do ii=1,nfiles
      if (idom(ii)+1 > nfiles) then
         write (*,'(a,i0,a,i0,a)') 'Error: mapmerge: found domain number ', idom(ii), ', which exceeds the number of files: ', nfiles, '.'
         exit ! We can't really recover from this.
      end if

      files(idom(ii)+1) = filesorig(ii)
   end do
   
   deallocate(idom, filesorig)

end subroutine dfm_order_by_partition

   
subroutine progress(prefix, j)
  implicit none
  character(len=*),intent(in)  :: prefix
  integer(kind=4),intent(in)   :: j
  integer(kind=4)              :: k
  character(len=27)            ::bar

  bar = "???% |                    |"
  write(unit=bar(1:3),fmt="(i3)") j
  do k=1, ceiling(j/5.0)
    bar(6+k:6+k)="*"
  enddo
  ! print the progress bar.
  if (j==100) then
     write(unit=6,fmt="(a1,a16,a27)") char(13), prefix, bar
  else
     write(unit=6,fmt="(a1,a16,a27)", advance='no') char(13), prefix, bar
  end if

  flush(6)
  return
end subroutine progress
end module dfm_merge
