!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2019.                                
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

module unstruc_ini
!! Some basic routines for reading an INI file.
!! Most work is done in startup and model modules.

use unstruc_messages
use properties
use string_module, only: str_lower, strcmpi

implicit none
private ! Prevent used modules from being exported

   type(tree_data), pointer, public :: ini_ptr !< Unstruc ini settings in tree_data

public :: readIniFile, init1dField, initInitialFields, spaceInit1dField, &
          get_req_string, get_req_integer, get_req_integers, get_req_double

contains

!> Loads initial program settings file through inifiles. 
subroutine readIniFile(filename, istat)
    character(*),      intent(in)  :: filename
    integer, optional, intent(out) :: istat

    istat = 0 ! Success

    call tree_create(trim(filename), ini_ptr)
    call prop_file('ini',trim(filename),ini_ptr,istat)
    if (istat /= 0) then
        call mess(LEVEL_ERROR, 'ini file '''//trim(filename)//''' not found. Code: ', istat)
    endif
end subroutine readIniFile

!> Reads the value for a string variable from a propery tree.
!! When not found, and error is logged and program stops. 
subroutine get_req_string(prop_ptr, chapter, key, value)
    type(tree_data), pointer, intent(in) :: prop_ptr
    character(*) ,intent (in)  :: chapter
    character(*) ,intent (in)  :: key
    character(*) ,intent (out) :: value

    logical :: success

    call prop_get_string(prop_ptr, chapter, key, value, success)
    if (.not. success) then
      call err('ERROR READING INI-FILE, RESTORE CORRECT FILE OR CALL Deltares.',&
               'NO VALUE FOUND FOR: ',key)
    endif
end subroutine get_req_string

!> Reads the value for an integer variable from a propery tree.
!! When not found, and error is logged and program stops. 
subroutine get_req_integer(prop_ptr, chapter, key, value)
    type(tree_data), pointer, intent(in) :: prop_ptr
    character(*) ,intent (in)  :: chapter
    character(*) ,intent (in)  :: key
    integer      ,intent (out) :: value

    logical :: success

    call prop_get_integer(prop_ptr, chapter, key, value, success)
    if (.not. success) then
      call err('ERROR READING INI-FILE, RESTORE CORRECT FILE OR CALL Deltares.',&
               'NO VALUE FOUND FOR: ',key)
    endif
end subroutine get_req_integer


!> Reads the value for an integer-list variable from a propery tree.
!! When not found, and error is logged and program stops. 
subroutine get_req_integers(prop_ptr, chapter, key, value, valuelength)
    type(tree_data), pointer, intent(in) :: prop_ptr
    character(*) ,intent (in)  :: chapter
    character(*) ,intent (in)  :: key
    integer,dimension(*),intent (out) :: value
    integer, intent (in) :: valuelength

    logical :: success

    call prop_get_integers(prop_ptr, chapter, key, value, valuelength, success)
    if (.not. success) then
      call err('ERROR READING INI-FILE, RESTORE CORRECT FILE OR CALL Deltares.\n'//&
               'NO VALUE FOUND FOR: '//key)
    endif
end subroutine get_req_integers

!> Reads the value for a double precision variable from a propery tree.
!! When not found, and error is logged and program stops. 
subroutine get_req_double(prop_ptr, chapter, key, value)
    type(tree_data), pointer, intent(in) :: prop_ptr
    character(*)    ,intent (in)  :: chapter
    character(*)    ,intent (in)  :: key
    double precision,intent (out) :: value

    logical :: success

    call prop_get_double(prop_ptr, chapter, key, value, success)
    if (.not. success) then
      call err('ERROR READING INI-FILE, RESTORE CORRECT FILE OR CALL Deltares.',&
               'NO VALUE FOUND FOR: ',key)
    endif
end subroutine get_req_double


!> Reads and initializes an initial field file. 
subroutine initInitialFields(filename)
   use tree_data_types
   use tree_structures
   use messageHandling
   use unstruc_files, only: resolvePath
   use system_utils
   use m_ec_interpolationsettings
   use m_flow
   use m_flowgeom
   use m_wind
   use m_missing
   use timespace
   use unstruc_boundaries, only: prepare_lateral_mask
   use m_flowexternalforcings, only: transformcoef
   use network_data
   use m_alloc
   implicit none
   character(len=*), intent(in   ) :: filename            !< name of initial field file
   type(tree_data),  pointer       :: inifield_ptr        !< tree of inifield-file's [Initial] or [Parameter] blocks
   type(tree_data),  pointer       :: node_ptr            
   integer                         :: istat               
   integer, parameter              :: ini_key_len   = 32  
   integer, parameter              :: ini_value_len = 256 
   character(len=ini_key_len)      :: groupname           
   character(len=ini_value_len)    :: quantity
   character(len=ini_value_len)    :: dataFile            
   character(len=ini_value_len)    :: dataFileType        
   character(len=ini_value_len)    :: interpolationMethod 
   character(len=ini_value_len)    :: locationType        
   character(len=ini_value_len)    :: averagingType        
   integer                         :: method               
   integer                         :: num_items_in_file   
   logical                         :: retVal
   character(len=ini_value_len)    :: fnam
   character(len=ini_value_len)    :: basedir
   integer                         :: i, ib, L, extrapolation, averagingNumMin, iprimpos, kc_size_store, mx, k1, k2, locType, aveType
   integer, allocatable            :: kcc(:), kc1D(:), kc2D(:)
   
   logical, external :: timespaceinitialfield_mpi
   
   
   call mess(LEVEL_INFO, 'Reading initial field file '''//trim(filename)//'''.')
   
   call tree_create(trim(filename), inifield_ptr)
   call prop_file('ini',trim(filename),inifield_ptr,istat) 
   
   call split_filename(filename, basedir, fnam)
   
   num_items_in_file = 0
   if (associated(inifield_ptr%child_nodes)) then
       num_items_in_file = size(inifield_ptr%child_nodes)
   endif
   
   
   !! Prepare for initial bedlevel, the following codes are copied from subroutine setbedlevelfromextfile
   ! ibedlevtyp determines from which source data location the bed levels are used to derive bobs and bl.
   ! These types need to be mapped to one of three possible primitive locations (center/edge/corner).
   select case (ibedlevtyp)
      case (1)       ! position = waterlevelpoint, cell centre
         iprimpos = 2 ; mx = max(numk, ndx)
      case (2)       ! position = velocitypoint, cellfacemid
         iprimpos = 1 ; mx = max(numk, lnx)
      case (3,4,5,6) ! position = netnode, cell corner
         iprimpos = 3 ; mx = numk
   end select
   
   kc_size_store = size(kc)
   allocate(kcc(mx),kc1d(mx),kc2d(mx)) ; kcc = 1; kc1D = 0 ; kc2D = 0
   call realloc(kc, mx, keepExisting = .false., fill = 0)
  
   do L = 1, numL1D
      if (kn(3,L) == 1 .or. kn(3,L) == 6) then ! TODO: AvD: why not also type 3/4/5/7?
          k1 = kn(1,L) ; k2 = kn(2,L)
          if (nmk(k1) > 1) kc1D(k1) = 1
          if (nmk(k2) > 1) kc1D(k2) = 1
      endif
   enddo
  
   if (iprimpos == 3) then
      do L = 1, numL
         if (kn(3,L) == 2) then
             k1 = kn(1,L) ; k2 = kn(2,L)
             kc2D(k1) = 1
             kc2D(k2) = 1
         endif
      enddo
   else if (iprimpos == 1) then
      kc2D(lnx1d+1:lnxi) = 1
   else if (iprimpos == 2) then
      kc2D(1:ndx2D) = 1
   endif
   
   ib = 0
   !! Now loop on each block
   do i=1,num_items_in_file
   
      node_ptr => inifield_ptr%child_nodes(i)%node_ptr
      groupname = tree_get_name(node_ptr)
      
      transformcoef = -999d0
      !! Step 1: Read each block
      if ((.not. strcmpi(groupname,'Initial')) .and. (.not.(strcmpi(groupname,'Parameter')))) then
         write(msgbuf, '(5a)') 'Unrecognized block in file ''', trim(filename), ''': [', trim(groupname), ']. Ignoring this block.'
         call warn_flush()
         cycle
      end if
      
      ! read quantity
      call prop_get_string(node_ptr, '', 'quantity', quantity, retVal)
      if (.not. retVal) then
         write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''quantity'' is missing. Ignoring this block.'
         call warn_flush()
         cycle
      end if
      if ((strcmpi(groupname, 'Initial') .and. (.not. strcmpi(quantity, 'bedlevel')) .and. (.not.strcmpi(quantity, 'waterlevel')) .and. (.not. strcmpi(quantity, 'waterdepth'))) &
         .or. (strcmpi(groupname, 'Parameter') .and. (.not. strcmpi(quantity, 'frictioncoefficient')))) then
         write(msgbuf, '(5a)') 'Wrong block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''quantity'' does not match (refer to User Manual). Ignoring this block.'
         call warn_flush()
         cycle
      end if
   
      ! read datafile
      call prop_get_string(node_ptr, '', 'dataFile', dataFile, retVal)
      if (retVal) then
         call resolvePath(dataFile, basedir, dataFile)
      else
         write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''dataFile'' is missing. Ignoring this block.'
         call warn_flush()
         cycle
      end if
   
      ! read dataFileType
      call prop_get_string(node_ptr, '', 'dataFileType ', dataFileType , retVal)
      if (.not. retVal) then
         write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''dataFileType'' is missing. Ignoring this block.'
         call warn_flush()
         cycle
      end if
      if (.not. strcmpi(dataFileType, 'arcinfo') .and. (.not.strcmpi(dataFileType, 'sample')) .and. (.not.strcmpi(dataFileType, '1dField')) .and. (.not. strcmpi(dataFileType, 'polygon'))) then
         write(msgbuf, '(5a)') 'Wrong block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''dataFileType'' does not match (refer to User Manual). Ignoring this block.'
         call warn_flush()
         cycle
      end if
      call fileTypeStringToInteger(dataFileType, filetype)
      
      ! if dataFileType is 1dField, then it is not necessary to read interpolationMethod, operand, averagingType,  
      ! averagingRelSize, averagingNumMin, averagingPercentile, locationType, extrapolationMethod, value
      if (.not. strcmpi(dataFileType, '1dField')) then 
         ! read interpolationMethod
         call prop_get_string(node_ptr, '', 'interpolationMethod ', interpolationMethod , retVal)
         if (.not. retVal) then
            write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''interpolationMethod'' is missing. Ignoring this block.'
            call warn_flush()
            cycle
         end if
         if (((.not. strcmpi(interpolationMethod, 'constant')).and. (.not. strcmpi(interpolationMethod, 'triangulation')) .and. (.not. strcmpi(interpolationMethod, 'averaging'))) &
            .or. (strcmpi(interpolationMethod,'constant') .and. (.not. strcmpi(dataFileType, 'polygon')))) then
            write(msgbuf, '(5a)') 'Wrong block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''interpolationMethod'' does not match (refer to User Manual). Ignoring this block.'
            call warn_flush()
            cycle
         end if
         call methodStringToInteger(interpolationMethod, method)
      
         ! read operand
         call prop_get_string(node_ptr, '', 'operand ', operand , retVal)
         if (.not. retVal) then
            operand = 'O'
            write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''operand'' is missing. Use the default value (= '''//trim(operand)//''').'
            call warn_flush()
         else
            if ((.not.strcmpi(operand, 'O')) .and. (.not.strcmpi(operand, 'A')) .and. (.not.strcmpi(operand, '+')) .and. (.not.strcmpi(operand, '*')) .and. (.not.strcmpi(operand, 'X')) .and. (.not.strcmpi(operand, 'N'))) then
            write(msgbuf, '(5a)') 'Wrong block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''operand'' does not match (refer to User Manual). Ignoring this block.'
            call warn_flush()
            cycle
            end if
         end if
         
         if (strcmpi(interpolationMethod,'averaging')) then
            ! read averagingType
            call prop_get_string(node_ptr, '', 'averagingType ', averagingType , retVal)
            if (.not. retVal) then
               averagingType = 'mean'
               write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''averagingType'' is missing. Use the default value (= '''// trim(averagingType)//''').'
               call warn_flush() 
            else
               if ((.not.strcmpi(averagingType, 'mean')) .and. (.not.strcmpi(averagingType, 'nearestN')) .and. (.not.strcmpi(averagingType, 'max')) .and. (.not.strcmpi(averagingType, 'min')) .and. (.not.strcmpi(averagingType, 'invDist')) .and. (.not.strcmpi(averagingType, 'minAbs'))) then
                  write(msgbuf, '(5a)') 'Wrong block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''averagingType'' does not match (refer to User Manual). Ignoring this block.'
                  call warn_flush()
                  cycle
               end if
            end if
            call averagingTypeStringToDouble(averagingType, transformcoef(4))
         
            ! read averagingRelSize
            call prop_get_double(node_ptr,'','averagingRelSize', transformcoef(5), retVal)
            if (.not. retVal) then
               transformcoef(5) = RCEL_DEFAULT
               write(msgbuf, '(5a,g8.3,a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''averagingRelSize'' is missing. Use the default value (=', transformcoef(5), ').'
               call warn_flush()
            end if
            
            ! read averagingNumMin
            call prop_get_integer(node_ptr,'','averagingNumMin', averagingNumMin, retVal)
            if (.not. retVal) then
               transformcoef(8) = 0d0
               write(msgbuf, '(5a,i2,a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''averagingNumMin'' is missing. Use the default value (=', int(transformcoef(8)), ').'
               call warn_flush()
            else
               transformcoef(8) = dble(averagingNumMin)
            end if
            
            ! read averagingPercentile
            call prop_get_double(node_ptr,'','averagingPercentile', transformcoef(7), retVal)
            if (.not. retVal) then
               transformcoef(7) = 0d0
               write(msgbuf, '(5a, g8.3, a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''averagingPercentile'' is missing. Use the default value (=', transformcoef(7), ').'
               call warn_flush()
            end if
         end if
         
         ! read locationType, only when quantity is waterlevel/bedlevel/waterdepth
         if (strcmpi(quantity,'waterlevel') .or. strcmpi(quantity,'bedlevel') .or. strcmpi(quantity,'waterdepth')) then
            call prop_get_string(node_ptr, '', 'locationType ', locationType , retVal)
            if (.not. retVal) then
               locType = ILATTP_ALL
               write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''locationType'' is missing. Use the default value (= all)'
               call warn_flush()
            else
               call str_lower(locationType)
               select case (trim(locationType))
                  case ('1d')
                     locType = ILATTP_1D
                  case ('2d')
                     locType = ILATTP_2D
                  case ('1d2d')
                     locType = ILATTP_ALL
                  case default
                     locType = ILATTP_ALL
               end select
            end if
         end if
         
         ! read extrapolationMethod
         call prop_get_integer(node_ptr,'','extrapolationMethod', extrapolation, retVal)
         if (.not. retVal) then
            extrapolation = 0
            write(msgbuf, '(5a, i2,a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''extrapolationMethod'' is missing. Use the default value (=', extrapolation, ').'
            call warn_flush()
         end if
         method = method + 100 * extrapolation
         
         ! read value
         if (strcmpi(dataFileType, 'polygon')) then
            call prop_get_double(node_ptr,'','value', transformcoef(1), retVal)
            if (.not. retVal) then
               write(msgbuf, '(5a)') 'Wrong block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''value'' is missing. Ignore this block.'
               call warn_flush()
               cycle
            end if
         end if
      end if
      
      
      ib = ib + 1

      !! Step 2: operation for each block
      if (strcmpi(dataFileType, '1dField')) then
         call init1dField(dataFile,filename, quantity)
      else
         if (strcmpi(quantity, 'waterlevel')) then
            call realloc(kcsini, ndx, keepExisting=.false.)
            call prepare_lateral_mask(kcsini, locType)
            
            success = timespaceinitialfield(xz, yz, s1, ndx, dataFile, filetype, method, operand, transformcoef, 2, kcsini) ! zie meteo module
         else if (strcmpi(quantity, 'bedlevel')) then          
            if (locType == ILATTP_1D) then
                call mess(LEVEL_INFO, 'Setting 1D bedlevel from file '''//trim(dataFile)//'''.')
                kc(1:mx) = kc1D
                success = timespaceinitialfield_mpi(xk, yk, zk, numk, dataFile, filetype, method, operand, transformcoef, 3, kc) ! zie meteo module
            else 
               if (locType == ILATTP_2D) then
                  call mess(LEVEL_INFO, 'Setting 2D bedlevel from file '''//trim(dataFile)//'''.')
                  kc(1:mx) = kc2D
               else if (locType == ILATTP_ALL)  then
                  call mess(LEVEL_INFO, 'Setting both 1d and 2D bedlevel from file '''//trim(dataFile)//'''.')
                  kc(1:mx) = kcc
               end if
               
               if (ibedlevtyp == 3) then
                  success = timespaceinitialfield_mpi(xk, yk, zk, numk, dataFile, filetype, method, operand, transformcoef, iprimpos, kc) ! zie meteo module
               else if (ibedlevtyp == 2) then
                  success = timespaceinitialfield_mpi(xu, yu, blu, lnx, dataFile, filetype, method, operand, transformcoef, iprimpos, kc) ! zie meteo module
               else if (ibedlevtyp == 1) then
                  success = timespaceinitialfield_mpi(xz, yz, bl, ndx, dataFile, filetype, method, operand, transformcoef, iprimpos, kc) ! zie meteo module
               end if
            end if
            
         else if (strcmpi(quantity, 'waterdepth')) then
            call realloc(kcsini, ndx, keepExisting=.false.)
            call prepare_lateral_mask(kcsini, locType)
            
            success = timespaceinitialfield(xz, yz, hs, ndx, dataFile, filetype, method, operand, transformcoef, 2, kcsini)
            
         else if (strcmpi(quantity, 'frictioncoefficient')) then
            success = timespaceinitialfield(xu, yu, frcu, lnx, dataFile, filetype, method,  operand, transformcoef, 1) ! zie meteo module
               if (success) then
                  if (transformcoef(3) .ne. -999d0 .and. int(transformcoef(3)) .ne. ifrctypuni .and. operand == 'O') then
                     do L = 1,lnx
                        if (frcu(L) .ne. dmiss) then
                            ! type array only must be used if different from uni
                            ifrcutp(L) = int( transformcoef(3) )
                        endif
                     enddo
                  endif
               endif
         end if
      end if
   end do
   
   write(msgbuf,'(a,i8,a)') 'Finish initializing the initial field file '''//trim(filename)//''':', ib , ' blocks have been read and handled.'
   call msg_flush()      
  
end subroutine initInitialFields


!> Reads and initilazes a 1d Field file (*.ini). 
subroutine init1dField(filename, inifieldf, quant)
   use tree_data_types
   use tree_structures
   use messageHandling
   use unstruc_files, only: resolvePath
   use system_utils
   use m_alloc
   use m_missing
   use m_flow
   use m_flowgeom
   use network_data
   implicit none
   
   character(len=*), intent(in) :: filename            !< file name for 1dField file
   character(len=*), intent(in) :: inifieldf           !< file name of iniField file
   character(len=*), intent(in) :: quant               !< quantity that is specified in iniField file
   
   type(tree_data), pointer     :: field_ptr           !< tree of inifield-file's [Initial] or [Parameter] blocks
   type(tree_data), pointer     :: node_ptr            !
   integer                      :: istat               !
   !logical                      :: success
   
   integer, parameter           :: ini_key_len   = 32  !
   integer, parameter           :: ini_value_len = 256 !
   character(len=ini_key_len)   :: groupname           !
   character(len=ini_value_len) :: quantity            ! 
   character(len=ini_value_len) :: unit                !
   character(len=ini_value_len) :: branchId            !            
   
   double precision, allocatable:: values(:)           !
   integer                      :: numLocations        !
   double precision, allocatable:: chainage(:)         ! 
   
   integer                      :: num_items_in_file   !
   logical                      :: retVal
   character(len=ini_value_len) :: fnam
   integer                      :: ib, jaglobal, i, j
   double precision             :: mchainage
   
   call tree_create(trim(filename), field_ptr)
   call prop_file('ini',trim(filename),field_ptr,istat) 
      
   num_items_in_file = 0
   if (associated(field_ptr%child_nodes)) then
       num_items_in_file = size(field_ptr%child_nodes)
   endif
   
   ib = 0
   jaglobal = 0
   numLocations = 0
   
   
   ! loop on each block
   do i=1,num_items_in_file
   
      node_ptr => field_ptr%child_nodes(i)%node_ptr
      groupname = tree_get_name(node_ptr)
      
      ! Step 1: read the block
      if (strcmpi(groupname, 'General')) then
         cycle
      end if
      if (strcmpi(groupname, 'Global')) then
         if (jaglobal == 0) then
            ! read quantity
            call prop_get_string(node_ptr, '', 'quantity', quantity, retVal)
            if (.not. retVal) then
               write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''quantity'' is missing.'
               call err_flush()
            end if
            if (.not. strcmpi(quantity, quant)) then
               write(msgbuf, '(7a)') 'Wrong block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''quantity'' does not match the "quantity" which is specified in iniField file ''', trim(inifieldf), '''.'
               call err_flush()
            end if
            if ((.not. strcmpi(quantity, 'bedlevel')) .and. (.not.strcmpi(quantity, 'waterlevel')) .and. (.not. strcmpi(quantity,'waterdepth')) .and. (.not. strcmpi(quantity, 'frictioncoefficient'))) then
               write(msgbuf, '(5a)') 'Wrong block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''quantity'' does not match (refer to User Manual).'
               call err_flush()
            end if
            ! read unit
            call prop_get_string(node_ptr, '', 'unit', unit, retVal)
            if (.not. retVal) then
               write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''unit'' is missing.'
               call err_flush()
            end if
            
            call realloc(values, 1, keepExisting=.false., fill = dmiss)
            call prop_get_double(node_ptr, '', 'value', values(1), retVal)
            if (.not. retVal) then
               write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''value'' is missing.'
               call err_flush()
            end if
            branchId = ''
            call realloc(chainage, 1, keepExisting = .false., fill=dmiss)
            jaglobal = 1
         else
            write(msgbuf, '(5a)') 'In file ''', trim(filename), ''': [', trim(groupname), ']. Only the first [Global] block is read, other [global] blocks are ignored.'
            call warn_flush()
            cycle
         end if
      else if (strcmpi(groupname, 'Branch')) then
         call prop_get_string(node_ptr, '', 'branchId', branchId, retVal)
         if (.not. retVal) then
            write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''branchId'' is missing. Ignore this block.'
            call warn_flush()
            cycle
         end if
         
         call prop_get_integer(node_ptr, '', 'numLocations', numLocations, retVal)
         if (.not. retVal) then
            numLocations = 0
         end if
         
         if (numLocations > 0) then
            call realloc(chainage, numLocations, keepExisting = .false.)
            call prop_get_doubles(node_ptr, '', 'chainage', chainage, numLocations, retVal)
            if (.not. retVal) then
               write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''chainage'' is missing. Ignore this block.'
               call warn_flush()
               cycle
            end if
            
            ! check if the locations are sorted by increasing chainage
            mchainage = chainage(1)
            do j = 2, size(chainage)
               if (chainage(j) > mchainage) then
                  mchainage = chainage(j)
               else
                  call mess(LEVEL_ERROR, 'The locations are not sorted by increasing chainage in 1dField file '''//trim(filename)//'''.')
               end if
            end do
                          

            call realloc(values, numLocations, keepExisting = .false.)
            call prop_get_doubles(node_ptr, '', 'values', values, numLocations, retVal)
            if (.not. retVal) then
               write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''values'' is missing. Ignore this block.'
               call warn_flush()
               cycle
            end if
         else
            call realloc(values, 1, keepExisting = .false.)
            call prop_get_double(node_ptr, '', 'values', values(1),retVal)
            if (.not. retVal) then
               write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''values'' is missing. Ignore this block.'
               call warn_flush()
               cycle
            end if
         end if
         ib = ib + 1
      else
         write(msgbuf, '(5a)') 'Unrecognized block in file ''', trim(filename), ''': [', trim(groupname), ']. Ignoring this block.'
         call warn_flush()
         cycle
      end if
      
      
      ! Step 2: operations
      if (strcmpi(quantity, 'waterlevel')) then
         call spaceInit1dfield(branchId, chainage, values, 2, s1)
      else if (strcmpi(quantity, 'waterdepth')) then
         call spaceInit1dfield(branchId, chainage, values, 2, hs)
      else if (strcmpi(quantity, 'frictioncoefficient')) then
         call spaceInit1dfield(branchId, chainage, values, 1, frcu)
      else if (strcmpi(quantity, 'bedlevel')) then
         call spaceInit1dfield(branchId, chainage, values, 2, zk)
      end if 
   end do
   
   write(msgbuf,'(a, i10,a)') 'Finish initializing 1dField file '''//trim(filename)//''':', ib , ' [Branch] blocks have been read and handled.'
   call msg_flush()    

end subroutine init1dField


!> Converts fileType string to an integer
subroutine fileTypeStringToInteger(sFileType, iFileType)
   use timespace_parameters
   implicit none
   character(len=*), intent(in   ) :: sFileType        !< file type string
   integer,          intent(  out) :: iFileType        !< file type integer
   
   call str_lower(sFileType)
   select case (trim(sFileType))
      case ('arcinfo')
         iFileType = arcinfo
      case ('sample')
         iFileType = triangulation
      case ('1dfield')
         iFileType = field1D
      case ('polygon')
         iFileType = inside_polygon
   end select
   return

end subroutine fileTypeStringToInteger


!> Converts interpolationMethod string to an integer
subroutine methodStringToInteger(sMethod, imethod)
   implicit none
   character(len=*), intent(in   ) :: sMethod        !< method string
   integer,          intent(  out) :: imethod        !< method integer

   call str_lower(sMethod)
   select case (trim(sMethod))
      case ('constant')
         imethod = 4
      case ('triangulation')
         imethod = 5
      case ('averaging')
         imethod = 6
   end select
   return

end subroutine methodStringToInteger


!> Converts averaging type string to a double precision value
subroutine averagingTypeStringToDouble(sAveragingType, dAveragingType)
   implicit none
   character(len=*), intent(in   ) :: sAveragingType        ! averaging type string
   double precision, intent(  out) :: dAveragingType        ! averaging type double precision value
   
   call str_lower(sAveragingType)
   select case (trim(sAveragingType))
      case ('mean')
         dAveragingType = 1d0
      case ('nearestnb')
         dAveragingType = 2d0
      case ('max')
         dAveragingType = 3d0
      case ('min')
         dAveragingType = 4d0
      case ('invdist')
         dAveragingType = 5d0
      case ('minabs')
         dAveragingType = 6d0
   end select
   return

end subroutine averagingTypeStringToDouble

!> Initialize the values based on the given sample values
!! The method is: 
!! 1) When one sample value is given:
!!    if it is from a [Global] block, then this value will be set on all branches.
!!    if it is from a [Branch] block, then this value will be set on a this branch.
!! 2) if more than one sample values are given, then on this branch:
!!          *
!!         / \
!!        /   *----
!!   ----*
!! between two samples use linear interpolation,                             
!! on the left side of the most left sample, use constant value of this sample,
!! on the right side of the most right sample, use constant value of this sample.
subroutine spaceInit1dField(sBranchId, sChainages, sValues, ipos, res)
   use m_alloc
   use m_network
   use m_inquire_flowgeom
   use unstruc_channel_flow
   use m_flowparameters, only: eps10
   use precision_basics
   use m_hash_search
   use m_hash_list
   use dfm_error
   
   implicit none
   character(len=*), intent(in   ) :: sBranchId     !< Sample branchId
   double precision, intent(in   ) :: sChainages(:) !< Sample chainages
   double precision, intent(in   ) :: sValues(:)    !< Sample values
   integer,          intent(in   ) :: ipos          !< position: 1= u point location, 2= 1d flownode(netnode) location
   double precision, intent(  out) :: res(:)        !< result
   
   integer                 :: nbrstart, nbrend, ibr, k, j, i, ierr, ipre, ns, ncount
   type(t_branch), pointer :: pbr
   double precision        :: chai, sChaiPrev, sChai, sValPrev, sVal, minsChai, maxsChai
   character(len=256)      :: brId
   
  
   if (size(sValues) == 1) then 
   ! assign sValues(1) on all branches, or on a certain branch
      if (len_trim(sBranchId) == 0) then ! [Global] block, for all branches
         nbrstart = 1
         nbrend   = network%brs%Count
      else ! [Branch] block with a uniform value, for a certain branch
         nbrstart = hashsearch(network%brs%hashlist, sBranchId)
         nbrend   = nbrstart
      end if
      
      ! assign sValues to res
      do ibr = nbrstart, nbrend
         pbr => network%brs%branch(ibr)
         brId = pbr%id
         if (ipos == 1) then
            ncount = pbr%uPointsCount
         else if (ipos == 2) then
            ncount = pbr%gridPointsCount
         end if
         
         do j = 1, ncount
            if (ipos == 1) then
               chai = pbr%uPointsChainages(j)
               ierr = findlink(brid, chai, k) ! find flowlink index given branchId and chainage
            else if (ipos == 2) then
               chai = pbr%gridPointsChainages(j)
               ierr = findnode(brid, chai, k) ! find flownode/netnode index given branchId and chainage
            end if
         
            if (ierr == DFM_NOERR) then
               res(k) = sValues(1)
            else
               write(msgbuf,'(a, g11.4,a)') 'Error when finding the flow link/node which locates on branch '''//trim(brId)//''' and chainage =', chai , '.'
               call err_flush()  
            end if
         end do
      end do
            
   else 
   ![Branch] block with numLocations > 1, and needs interpolations
      ns = size(sChainages)
      minsChai = sChainages(1)
      maxsChai = sChainages(ns)
      
      ibr = hashsearch(network%brs%hashlist, sBranchId)
      pbr => network%brs%branch(ibr)
      
      if (ipos == 1) then
         ncount = pbr%uPointsCount
      else if (ipos == 2) then
         ncount = pbr%gridPointsCount
      end if
      
      ipre = 2
      do j = 1, ncount
         if (ipos == 1) then
            chai = pbr%uPointsChainages(j)
            ierr = findlink(sBranchId, chai, k) ! find flowlink index given branchId and chainage
         else if (ipos == 2) then
            chai = pbr%gridPointsChainages(j)
            ierr = findnode(sBranchId, chai, k) ! find flownode/netnode index given branchId and chainage
         end if
   
         if (ierr /= DFM_NOERR) then
            write(msgbuf,'(a, g11.4,a)') 'Error when finding the flow link/node which locates on branch '''//trim(brId)//''' and chainage =', chai , '.'
            call err_flush()  
         else
            if (comparereal(chai, minsChai, eps10) <= 0) then
               res(k) = sValues(1)
               cycle
            else if (comparereal(chai, maxsChai, eps10) >= 0) then
               res(k) = sValues(ns)
               cycle
            end if
               
            do i = ipre, ns
               sChaiPrev = sChainages(i-1)
               sChai     = sChainages(i)
               sValPrev  = sValues(i-1)
               sVal      = sValues(i)
               
               if (comparereal(chai, sChaiPrev, eps10) >= 0 .and. comparereal(chai, sChai, eps10) < 0) then
                  if (comparereal(sChai, sChaiPrev, eps10)/=0) then
                     res(k) = sValPrev + (sVal-sValPrev)/(sChai-sChaiPrev)*(chai-sChaiPrev)
                  else
                     res(k) = (sVal + sValPrev)/2
                  end if
                  ipre = i
                  exit
               end if  
            end do
         end if
      end do
   end if
end subroutine spaceInit1dField

end module unstruc_ini
