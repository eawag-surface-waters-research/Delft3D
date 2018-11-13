!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2018.                                
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

!  $Id$
!  $HeadURL$

!> This module contains support methods for the EC-module.
!! @author adri.mourits@deltares.nl
!! @author edwin.bos@deltares.nl
module m_ec_support
   use m_ec_message
   use m_ec_typedefs
   use m_alloc
   use string_module
   use m_ec_parameters
   use time_module

   implicit none
   
   private
   
   public :: ecTimeFrameRealHpTimestepsToModifiedJulianDate
   public :: ecTimeFrameRealHpTimestepsToDateTime
   public :: ecGetTimesteps
   public :: ecSupportOpenExistingFile
   public :: ecSupportOpenExistingFileGnu
   public :: ecSupportAddUniqueInt
   public :: ecSupportAddUniqueChar
   public :: ecSupportFindItemByQuantityLocation
   public :: ecSupportFindItem
   public :: ecSupportFindQuantity
   public :: ecSupportFindElementSet
   public :: ecSupportFindField
   public :: ecSupportFindConnection
   public :: ecSupportFindConverter
   public :: ecSupportFindFileReader
   public :: ecSupportFindFileReaderByFilename
   public :: ecSupportFindBCBlock
   public :: ecSupportFindNetCDF
   public :: ecSupportFindNetCDFByFilename
   public :: ecSupportFindBCFileByFilename
   public :: ecSupportNetcdfCheckError
   public :: ecSupportNCFindCFCoordinates
   public :: ecSupportTimestringToUnitAndRefdate
   public :: ecSupportTimeUnitConversionFactor
   public :: ecSupportTimeToTimesteps
   public :: ecSupportThisTimeToTimesteps
   public :: ecSupportFindRelatedBCBlock
   
   interface ecTimeFrameRealHpTimestepsToModifiedJulianDate
      module procedure ecTimeFrameRealHpTimestepsToModifiedJulianDate
   end interface ecTimeFrameRealHpTimestepsToModifiedJulianDate
   
   contains
      
      ! ==========================================================================
      
      !> Calculate a Julian Date from the number of timesteps in seconds since reference_date.
      function ecTimeFrameRealHpTimestepsToModifiedJulianDate(timeFramePtr, steps, mjd) result(success)
         logical                                 :: success      !< function status
         type(tEcTimeFrame), pointer             :: timeFramePtr !< intent(inout)
         real(hp),                   intent(in)  :: steps        !< number of time steps
         real(hp),                   intent(out) :: mjd          !< calculated Modified Julian Date
         !
         success = .true.
         !
         mjd = timeFramePtr%k_refdate + (steps / 60.0_hp / 60.0_hp / 24.0_hp)
         
      end function ecTimeFrameRealHpTimestepsToModifiedJulianDate
      
      ! =======================================================================

      !> Calculate a Gregorian date and hour-minutes-seconds integer since reference date
      function ecTimeFrameRealHpTimestepsToDateTime(timeFramePtr, steps, yyyymmdd, hhmmss) result(success)
      use mathconsts, only : daysec_hp
         logical                                 :: success      !< function status
         type(tEcTimeFrame),         intent(in)  :: timeFramePtr !< time frame pointer
         real(hp),                   intent(in)  :: steps        !< number of time steps
         integer,                    intent(out) :: yyyymmdd     !< calculated Gregorian date
         integer,                    intent(out) :: hhmmss       !< time of the day
         real(hp)                                :: ssm          !< seconds since midnight helper variable
         integer                                 :: hh, mm, ss   !< hours, minutes, seconds helper variables
         integer                                 :: ierr         !< return code mjd2date
         !
         ierr = mjd2date(timeFramePtr%k_refdate  + (steps / daysec_hp), yyyymmdd)
         success = (ierr == 1)

         if (success) then
            ssm = mod(steps, daysec_hp)
            hh = int(ssm) / 3600
            mm = int(ssm) / 60 - hh * 60
            ss = int(ssm) - hh * 3600 - mm * 60
            hhmmss = hh*10000 + mm*100 + ss
         else
            hhmmss = 0
         endif

      end function ecTimeFrameRealHpTimestepsToDateTime
      
      ! =======================================================================
      
      !> Read and convert the timesteps to seconds.
      !! Takes a string of format: TIME = 0 hours since 2006-01-01 00:00:00 +00:00
      !! or ... TIME (HRS)      6.0 20000101 6
      function ecGetTimesteps(rec, time_steps, convert) result(success)
         logical                                :: success    !< function status
         character(len=maxNameLen), intent(in)  :: rec        !< time information string
         real(hp),                  intent(out) :: time_steps !< timesteps in seconds
         logical, optional,         intent(in)  :: convert    !< convert to seconds or leave unconverted

         integer :: posSince !< position in string of 'since'
         !
         success = .false.
         !
         if (len(trim(rec)) == 0) then
            call setECMessage("ec_provider::ecGetTimesteps: Input string is empty.")
            return
         end if
         posSince = index(rec, 'since')
         if (posSince > 0) then
            read(rec(index(rec, '=')+1 : posSince-1), *) time_steps
         else if ( .not. ecSupportTimestringArcInfo(rec, time_steps=time_steps)) then
            call setECMessage("ec_provider::ecGetTimesteps: can not find time step in: " // trim(rec) // ".")
            return
         endif
         call str_lower(rec)
         if (present(convert)) then
            if (.not. convert) then
                success = .true.
                return
            end if
         end if
         if (index(rec, 'seconds') /= 0) then
            continue
         else if (index(rec, 'minutes') /= 0) then
            time_steps = time_steps * 60.0_hp
         else if (index(rec, 'hours') /= 0 .or. index( rec, 'hrs') /= 0) then
            time_steps = time_steps * 60.0_hp * 60.0_hp
         else
            call setECMessage("ec_provider::ecGetTimesteps: Unable to identify the time unit.")
            return
         end if
         success = .true.
      end function ecGetTimesteps
      
      ! ==========================================================================
      
      !> Attempt to open an file for reading that might already have been opened under another handle.
      !> Workaround for GNU Fortran (which normally does not support multiple file openings of the same file)
      function ecSupportOpenExistingFileGnu(minp, filename) result(success)
         !
         use multi_file_io

         logical                                      :: success  !< function status
         integer(kind=8),               intent(inout) :: minp     !< IO unit number
         character(len=*), intent(in)                 :: filename !< relative path
         success = .false.

         minp = mf_open(filename)
         if (minp<=0) then 
            call setECMessage("ec_support::ecSupportOpenExistingFileGnu: Opening "//trim(filename)//" failed.")
            return
         endif 
         success = .true.
      end function ecSupportOpenExistingFileGnu

      ! ==========================================================================
      
      !> Attempt to open an existing file.
      function ecSupportOpenExistingFile(minp, filename) result(success)
         use netcdf
         !
         logical                         :: success  !< function status
         integer,          intent(inout) :: minp     !< IO unit number
         character(len=*), intent(in)    :: filename !< relative path
         !
         integer :: ierror         !< netcdf helper variable
         integer :: i              !< loop counter
         logical :: unitused       !< IO unit number already in use
         integer :: istat          !< status of file open operation
         integer :: chunkSizeHint  !< chunk size for NetCDF
         !
         success = .false.
         unitused = .false.
         ! Sanity checks.
         if (len_trim(filename) == 0) then
            call setECMessage("ec_support::ecSupportOpenExistingFile: Name is empty")
            return
         endif
         inquire(file = trim(filename), exist = success)
         if (.not. success) then
            call setECMessage("ec_support::ecSupportOpenExistingFile: File does not exist: ", trim(filename))
            return
         endif
         ! Special case: NetCDF.
         if (index(filename, '.nc') > 0) then
            chunkSizeHint = 4096   ! maximum allowed value
            ierror = nf90_open(trim(filename), NF90_NOWRITE, minp, chunkSizeHint)
            success = ecSupportNetcdfCheckError(ierror, "opening file", filename)
            return
         endif
         ! Locate an unused file unit.
         do i = 10, maxFileUnits
            inquire (unit = i, opened = unitused) 
            if (.not. unitused) exit
         enddo
         if (unitused) then
            call setECMessage("ec_support::ecSupportOpenExistingFile: No free unit number available")
            success = .false.
            return
         endif
         minp = i
         ! Open the data file.
         open(minp, file = trim(filename), action = 'READ', iostat = istat)
         if (istat == 0) then
            success = .true.
         else
            call setECMessage("ec_support::ecSupportOpenExistingFile: opening file " // trim(filename) // " failed")
         endif
      end function ecSupportOpenExistingFile
   
      ! ==========================================================================
   
      !> Add an integer to a set of integers.
      function ecSupportAddUniqueInt(intArr, anInt) result(success)
         logical                        :: success !< function status
         integer, dimension(:), pointer :: intArr !< array containing unique integers (a set)
         integer, intent(in)            :: anInt  !< integer to be added to the set of integers
   
         integer                        :: i         !< loop counter
         integer                        :: istat     !< deallocate() status
         integer                        :: lenArr    !< lenght of intArr
         integer, dimension(:), pointer :: newIntArr !< larger version of intArr
      
         success = .false.
         newIntArr => null()
         i = 0
         istat = 1
         lenArr = 0
      
         if (.not. associated(intArr)) then
            call setECMessage("ec_support::ecSupportAddUniqueInt: Dummy argument pointer intArr is not associated.")
         else
            lenArr = size(intArr)
            do i=1, lenArr
               if (intArr(i) == anInt) then
                  ! This integer is already in intArr
                  success = .true.
                  return
               endif
            enddo
            ! This integer is not yet in intArr, so add it.
            allocate(newIntArr(lenArr+1), STAT = istat)
            if (istat /= 0 ) then
               call setECMessage("ec_support::ecSupportAddUniqueInt: Unable to allocate additional memory.")
            else
               do i=1, lenArr
                  newIntArr(i) = intArr(i) ! Copy existing integers.
               enddo
               newIntArr(lenArr+1) = anInt ! Add the new integer.
               deallocate(intArr, STAT = istat)
               if (istat /= 0 ) then
                  call setECMessage("WARNING: ec_support::ecSupportAddUniqueInt: Unable to deallocate old memory.")
               end if
               intArr => newIntArr
               success = .true.
            end if
         endif
      end function ecSupportAddUniqueInt
   
      ! ==========================================================================
   
      !> Add a char to a set of chars.
      function ecSupportAddUniqueChar(charArr, aChar) result(success)
         logical                                             :: success !< function status
         character(len=maxNameLen), dimension(:), pointer    :: charArr !< array containing unique chars (a set)
         character(len=*),                        intent(in) :: aChar   !< char to be added to the set of chars
         !
         integer                                          :: i          !< loop counter
         integer                                          :: istat      !< deallocate() status
         integer                                          :: lenArr     !< lenght of charArr
         character(len=maxNameLen), dimension(:), pointer :: newCharArr !< larger version of charArr
         !
         success = .false.
         newCharArr => null()
         i = 0
         istat = 1
         lenArr = 0
         !
         !if (.not. associated(charArr)) then
         !   reallocP(charArr, 1, STAT = istat)
         !   if (istat == 0) then
         !      charArr(1) = aChar
         !   else
         !      call setECMessage("ec_support::ecSupportAddUniqueChar: Unable to allocate additional memory.")
         !   end if
         !   return
         !end if
         !! 
         !lenArr = size(charArr)
         !do i=1, lenArr
         !   if (charArr(i) == aChar) then
         !      ! This char is already in charArr
         !      success = .true.
         !      return
         !   endif
         !enddo
         !! This char is not yet in charArr, so add it.
         !reallocP(charArr, lenArr+1, STAT = istat)
         !if (istat == 0) then
         !   charArr(lenArr+1) = aChar
         !   success = .true.
         !else
         !   call setECMessage("ec_support::ecSupportAddUniqueChar: Unable to allocate additional memory.")
         !end if
      end function ecSupportAddUniqueChar
      
      ! =======================================================================
      ! Find methods
      ! =======================================================================
      
      !> Retrieve the pointer to the Quantity with id == quantityId.
      function ecSupportFindQuantity(instancePtr, quantityId) result(quantityPtr)
         type(tEcQuantity), pointer            :: quantityPtr !< Quantity corresponding to quantityId
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: quantityId  !< unique Quantity id
         !
         integer :: i !< loop counter
         !
         quantityPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nQuantities
               if (instancePtr%ecQuantitiesPtr(i)%ptr%id == quantityId) then
                  quantityPtr => instancePtr%ecQuantitiesPtr(i)%ptr
               end if
            end do
         else
            call setECMessage("ec_support::ecSupportFindQuantity: Dummy argument instancePtr is not associated.")
         end if
      end function ecSupportFindQuantity
      
      ! =======================================================================
      !> Retrieve the pointer to a NetCDF object by filename 
      function ecSupportFindNetCDFByFilename(instancePtr, ncfilename) result(netCDFPtr)
         type(tEcNetCDF), pointer            :: netCDFPtr   !< Quantity corresponding to quantityId
         type(tEcInstance), pointer          :: instancePtr !< intent(in)
         character(len=*),  intent(in)       :: ncfilename  !< netCDF filename 
         integer                             :: netCDFId
         !
         integer :: i !< loop counter
         !
         netCDFPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nNetCDFs
               if (instancePtr%ecNetCDFsPtr(i)%ptr%ncfilename == ncfilename) then
                  netCDFPtr => instancePtr%ecNetCDFsPtr(i)%ptr
               end if
            end do
         end if
      end function ecSupportFindNetCDFByFilename

      ! =======================================================================
      !> Retrieve the pointer to a BC-File object by filename 
      function ecSupportFindBCFileByFilename(instancePtr, bcfilename) result(BCFilePtr)
         type(tEcBCFile), pointer            :: BCFilePtr   !< Quantity corresponding to quantityId
         type(tEcInstance), pointer          :: instancePtr !< intent(in)
         character(len=*),  intent(in)       :: bcfilename  !< BC filename 
         !
         integer :: i !< loop counter
         !
         BCFilePtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nBCFiles
               if (instancePtr%ecBCFilesPtr(i)%ptr%bcfilename == bcfilename) then
                  BCFilePtr => instancePtr%ecBCFilesPtr(i)%ptr
               end if
            end do
         end if
      end function ecSupportFindBCFileByFilename
      ! =======================================================================
      
      !> Retrieve the pointer to the ElementSet with id == elementSetId.
      function ecSupportFindElementSet(instancePtr, elementSetId) result(elementSetPtr)
         type(tEcElementSet), pointer            :: elementSetPtr !< ElementSet corresponding to elementSetId
         type(tEcInstance),   pointer            :: instancePtr   !< intent(in)
         integer,                     intent(in) :: elementSetId  !< unique ElementSet id
         !
         integer :: i !< loop counter
         !
         elementSetPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nElementSets
               if (instancePtr%ecElementSetsPtr(i)%ptr%id == elementSetId) then
                  elementSetPtr => instancePtr%ecElementSetsPtr(i)%ptr
               end if
            end do
         else
            call setECMessage("ec_support::ecSupportFindElementSet: Dummy argument instancePtr is not associated.")
         end if
      end function ecSupportFindElementSet
      
      ! =======================================================================
      
      !> Retrieve the pointer to the Field with id == fieldId.
      function ecSupportFindField(instancePtr, fieldId) result(fieldPtr)
         type(tEcField),    pointer            :: fieldPtr    !< Field corresponding to fieldId
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: fieldId     !< unique Field id
         !
         integer :: i !< loop counter
         !
         fieldPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nFields
               if (instancePtr%ecFieldsPtr(i)%ptr%id == fieldId) then
                  fieldPtr => instancePtr%ecFieldsPtr(i)%ptr
               end if
            end do
         else
            call setECMessage("ec_support::ecSupportFindField: Dummy argument instancePtr is not associated.")
         end if
      end function ecSupportFindField

subroutine ecInstanceListSourceItems(instancePtr,dev)
         implicit none
         ! List source items by quantity and location
         type(tEcInstance), pointer :: instancePtr           !< EC instance 
         integer, intent(in)        :: dev 
         type(tEcItem), pointer     :: sourceItemPtr
         integer  :: ii 
         do ii=1, instancePtr%nItems 
            sourceItemPtr => instancePtr%ecItemsPtr(ii)%ptr
            if (sourceItemPtr%role == itemType_source) then
                     write(dev,'(a,i5.5)') 'Source Item ',sourceItemPtr%id
                     write(dev,'(a)')      '  Quantity = '//trim(sourceItemPtr%quantityPtr%name)
                     write(dev,'(a)')      '  Location = '//trim(sourceItemPtr%elementsetPtr%name)
                     write(dev,*) ''
            endif 
         enddo
end subroutine ecInstanceListSourceItems
      
      !! =======================================================================
      !!> Retrieve the item ID given a quantitystring and locationstring
      !!> Loop over all items in the EC instance qualified as 'source' 
      !function ecSupportFindItemByQuantityLocation(instancePtr, quantityname, locationname ) result(itemID)
      !   type(tEcInstance), pointer            :: instancePtr    !< EC-instance
      !   character(len=*), intent(in)         :: quantityname   !< Desired quantity  
      !   character(len=*), intent(in)         :: locationname   !< Desired location 
      !   integer                               :: itemID         !< returned item ID
      !   integer :: i                                            !< loop counter over items 
      !   type (tEcItem), pointer               :: itemPtr
      !   
      !   itemID = -1 
      !   if (associated(instancePtr)) then
      !      do i=1, instancePtr%nItems
      !         itemPtr => instancePtr%ecItemsPtr(i)%ptr
      !         if (itemPtr%role==itemType_source) then
      !            if (itemPtr%quantityPtr%name==quantityname .and. itemPtr%elementSetPtr%name==locationname) then
      !               itemID = itemPtr%id
      !               exit
      !            end if
      !         end if
      !      end do
      !   end if 
      !end function ecSupportFindItemByQuantityLocation

      ! =======================================================================
      !> Retrieve the item ID given a quantitystring and locationstring
      !> Use the fact that each filereader is associated with ONE location, but possibly MULTIPLE quantities
      !> i.e., select filereader first and check its items. 
      function ecSupportFindItemByQuantityLocation(instancePtr, locationname, quantityname, isLateral) result(itemID)
         type(tEcInstance), pointer             :: instancePtr    !< EC-instance
         character(len=*), intent(in)           :: quantityname   !< Desired quantity  
         character(len=*), intent(in)           :: locationname   !< Desired location 
         logical         , intent(in), optional :: isLateral      !< searching for lateral discharge?
         integer                                :: itemID         !< returned item ID
         integer                                :: i, j           !< loop counter over filereader, items 
         logical                                :: found          !< item found?
         type (tEcItem), pointer                :: itemPtr
         type (tEcFileReader), pointer          :: fileReaderPtr
         character(len=:), allocatable          :: quantity_requested, location_requested
         character(len=:), allocatable          :: quantity_supplied, location_supplied
!        character(len=maxNameLen)         :: quantityname_upper, locationname_upper
         

         quantity_requested = trim(quantityname)
         call str_upper(quantity_requested)
         location_requested = trim(locationname)
         call str_upper(location_requested)
         itemID = -1 
         if (associated(instancePtr)) then
           frs:do i=1, instancePtr%nFileReaders
               fileReaderPtr => instancePtr%ecFileReadersPtr(i)%ptr
               if (fileReaderPtr%nItems<=0) cycle                                               ! No items to check 
               location_supplied = fileReaderPtr%items(1)%ptr%elementSetPtr%name
               if (location_supplied/=location_requested) cycle     ! Items have the wrong location 
               do j=1, fileReaderPtr%nItems
                  itemPtr => fileReaderPtr%items(j)%ptr
                  quantity_supplied = itemPtr%quantityPtr%name
                  call str_upper(quantity_supplied)
                  if (quantity_supplied==quantity_requested) then
                     found = .true.
                     if (present(isLateral)) then
                        found = .false.
                        if (isLateral) then
                           if (associated(fileReaderPtr%bc)) then
                              found = fileReaderPtr%bc%isLateral
                           endif
                        endif
                     endif
                     if (found) then
                        itemID = itemPtr%id
                        exit frs
                     endif
                  end if
               end do 
            end do frs
         end if 
      end function ecSupportFindItemByQuantityLocation
      ! =======================================================================

      !function ecSupportCreateTimeInterpolatedItem(instancePtr, sourceItemId, tgtNdx) result(itemId)
      !    use m_ec_item
      !    use m_ec_converter,  only: ecConverterSetType, ecConverterSetInterpolation, ecConverterSetOperand, ecConverterSetElement
      !    use m_ec_instance,   only: ecInstanceCreateConverter, ecInstanceCreateConnection, ecInstanceCreateItem, ecInstanceCreateField, ecInstanceCreateQuantity
      !    use m_ec_connection, only: ecConnectionAddTargetItem, ecConnectionAddSourceItem, ecConnectionSetConverter 
      !    use m_ec_quantity,   only: ecQuantitySetName
      !    use m_ec_field,      only: ecFieldCreate1dArray
      !    use m_ec_item
      !
      !
      !    type(tEcInstance), pointer    :: instancePtr    !< EC-instance
      !    integer, intent(in)           :: sourceItemId   !< Source item id, before temporal interpolation
      !    integer, intent(in), optional :: tgtNdx         !< Optional target index, 1 is assumed as default
      !    integer                       :: targetItemId   !< Target item id, after temporal interpolation
      !    integer                       :: itemId         !< returned  target item ID, if successful, otherwise -1 
      !    integer                       :: convertId 
      !    type(tECItem), pointer        :: sourceItemPtr => null() 
      !    type(tECItem), pointer        :: targetItemPtr => null()
      !    character(len=:), allocatable :: quantityName
      !    integer                       :: arraySize
      !
      !    integer :: targetIndex 
      !    integer :: converterId, connectionId, quantityId, elementSetId, fieldId
      !    
      !    if (present(tgtNdx)) then 
      !       targetIndex = tgtNdx
      !    else
      !       targetIndex = 1
      !    end if 
      !
      !    sourceItemPtr => ecSupportFindItem(instancePtr, sourceItemId)
      !
      !    ! TODO: create target item:
      !    !       . elementset-name = source_item's elementset-name
      !    !       . quantity-name = source_item's quantity-name + '-interpolated'
      !    itemId = -1 
      !
      !    ! Set up the target item 
      !    targetItemId = ecInstanceCreateItem(instancePtr)
      !    fieldId = ecInstanceCreateField(instancePtr)
      !
      !    arraySize = size(sourceItemPtr%sourceT0FieldPtr%arr1d)
      !    if (.not. (ecFieldCreate1dArray(instancePtr, fieldId, arraySize))) return
      !
      !    if (.not. ecItemSetRole(instancePtr, targetItemId, itemType_target)) return
      !    if (.not. ecItemSetTargetField(instancePtr, targetItemId, fieldId)) return
      !    if (.not. ecItemSetType(instancePtr, targetItemId, accessType_evaluate)) return 
      !    quantityId = ecInstanceCreateQuantity(instancePtr)
      !    quantityName = trim(sourceItemPtr%quantityPtr%name)
      !    if (.not. ecItemSetQuantity(instancePtr, targetItemId, quantityId)) return
      !    if (.not. (ecQuantitySetName(instancePtr, quantityId, quantityName//'_interpolated'))) return
      !    elementSetId = sourceItemPtr%elementSetPtr%id
      !    if (.not. ecItemSetElementSet(instancePtr, targetItemId, elementSetId)) return
      !
      !    ! Construct a new Converter.
      !    converterId = ecInstanceCreateConverter(instancePtr)
      !
      !    ! Initialize the new Converter.
      !    if (.not. (ecConverterSetType(instancePtr, converterId, convType_uniform))) return
      !    if (.not. (ecConverterSetOperand(instancePtr, converterId, operand_replace_element))) return
      !    if (.not. (ecConverterSetInterpolation(instancePtr, converterId, interpolate_timespace))) return
      !    if (.not. (ecConverterSetElement(instancePtr, converterId, targetIndex))) return
      !
      !    ! Construct a new Connection.
      !    connectionId = ecInstanceCreateConnection(instancePtr)
      !    if (.not. ecConnectionSetConverter(instancePtr, connectionId, converterId)) return
      !
      !    ! Initialize the new Connection.
      !    if (.not. ecConnectionAddSourceItem(instancePtr, connectionId, sourceItemId)) return
      !    if (.not. ecConnectionAddTargetItem(instancePtr, connectionId, targetItemId)) return
      !    if (.not. ecItemAddConnection(instancePtr, targetItemId, connectionId)) return
      !    itemId = targetItemId
      !end function ecSupportCreateTimeInterpolatedItem
      ! =======================================================================

      
      !> Retrieve the pointer to the Item with id == itemId.
      function ecSupportFindItem(instancePtr, itemId) result(itemPtr)
         type(tEcItem),     pointer            :: itemPtr     !< Item corresponding to itemId
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: itemId      !< unique Item id
         !
         integer :: i !< loop counter
         !
         itemPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nItems
               if (instancePtr%ecItemsPtr(i)%ptr%id == itemId) then
                  itemPtr => instancePtr%ecItemsPtr(i)%ptr
               end if
            end do
         else
            call setECMessage("ec_support::ecSupportFindItem: Dummy argument instancePtr is not associated.")
         end if
      end function ecSupportFindItem
      
      ! =======================================================================
      
      !> Retrieve the pointer to the Connection with id == connectionId.
      function ecSupportFindConnection(instancePtr, connectionId) result(connectionPtr)
         type(tEcConnection),     pointer            :: connectionPtr !< Item corresponding to connectionId
         type(tEcInstance),       pointer            :: instancePtr   !< intent(in)
         integer,                         intent(in) :: connectionId  !< unique Connection id
         !
         integer :: i !< loop counter
         !
         connectionPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nConnections
               if (instancePtr%ecConnectionsPtr(i)%ptr%id == connectionId) then
                  connectionPtr => instancePtr%ecConnectionsPtr(i)%ptr
               end if
            end do
         else
            call setECMessage("ec_support::ecSupportFindConnection: Dummy argument instancePtr is not associated.")
         end if
      end function ecSupportFindConnection
      
      ! =======================================================================
      
      !> Retrieve the pointer to the Converter with id == converterId.
      function ecSupportFindConverter(instancePtr, converterId) result(converterPtr)
         type(tEcConverter), pointer            :: converterPtr !< Item corresponding to converterId
         type(tEcInstance),  pointer            :: instancePtr  !< intent(in)
         integer,                    intent(in) :: converterId  !< unique Converter id
         !
         integer :: i !< loop counter
         !
         converterPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nConverters
               if (instancePtr%ecConvertersPtr(i)%ptr%id == converterId) then
                  converterPtr => instancePtr%ecConvertersPtr(i)%ptr
               end if
            end do
         else
            call setECMessage("ec_support::ecSupportFindConverter: Dummy argument instancePtr is not associated.")
         end if
      end function ecSupportFindConverter
      
      ! =======================================================================
      
      !> Retrieve the pointer to the FileReader with id == converterId.
      function ecSupportFindFileReader(instancePtr, fileReaderId) result(fileReaderPtr)
         type(tEcFileReader), pointer            :: fileReaderPtr !< FileReader corresponding to fileReaderId
         type(tEcInstance),   pointer            :: instancePtr   !< intent(in)
         integer,                     intent(in) :: fileReaderId  !< unique FileReader id
         !
         integer :: i !< loop counter
         !
         fileReaderPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nFileReaders
               if (instancePtr%ecFileReadersPtr(i)%ptr%id == fileReaderId) then
                  fileReaderPtr => instancePtr%ecFileReadersPtr(i)%ptr
               end if
            end do
         else
            call setECMessage("ec_support::ecSupportFindFileReader: Dummy argument instancePtr is not associated.")
         end if
      end function ecSupportFindFileReader
      
      ! =======================================================================
      !> Retrieve the pointer to the FileReader with id == converterId.
      function ecSupportFindFileReaderByFilename(instancePtr, filename) result(fileReaderPtr)
         type(tEcFileReader), pointer            :: fileReaderPtr !< FileReader corresponding to fileReaderId
         type(tEcInstance),   pointer            :: instancePtr   !< intent(in)
         character(*),        intent(in)         :: filename      !< relative path of data file
         !
         integer :: i !< loop counter
         !
         fileReaderPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nFileReaders
               if (associated(instancePtr%ecFileReadersPtr(i)%ptr%bc)) then                  ! if filereader has bc-block
                  if (strcmpi(instancePtr%ecFileReadersPtr(i)%ptr%bc%fName, fileName)) then  ! this bc-block has the filename
                     fileReaderPtr => instancePtr%ecFileReadersPtr(i)%ptr
                  end if
               else                                                                          ! else
                  if (strcmpi(instancePtr%ecFileReadersPtr(i)%ptr%fileName, fileName)) then  ! the filereader has the filename
                     fileReaderPtr => instancePtr%ecFileReadersPtr(i)%ptr
                  end if
               end if
            end do
         else
            call setECMessage("ec_support::ecSupportFindFileReader: Dummy argument instancePtr is not associated.")
         end if
      end function ecSupportFindFileReaderByFilename

      ! =======================================================================

      !> Retrieve the pointer to the BCBlock with id == bcBlockId.
      function ecSupportFindBCBlock(instancePtr, bcBlockId) result(bcBlockPtr)
         type(tEcBCBlock), pointer               :: bcBlockPtr    !< BCBlock corresponding to bcBlockId
         type(tEcInstance), pointer              :: instancePtr   !< intent(in)
         integer, intent(in)                     :: bcBlockId     !< unique BCBlock id
         !
         integer :: i !< loop counter
         !
         bcBlockPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nBCBlocks
               if (instancePtr%ecBCBlocksPtr(i)%ptr%id == bcBlockId) then
                  bcBlockPtr => instancePtr%ecBCBlocksPtr(i)%ptr
               end if
            end do
         else
            call setECMessage("ec_support::ecSupportFindBCBlock: Dummy argument instancePtr is not associated.")
         end if
      end function ecSupportFindBCBlock

      ! =======================================================================

      !> Retrieve the pointer to the NetCDF instance with id == netCDFId.
      function ecSupportFindNetCDF(instancePtr, netCDFId) result(netCDFPtr)
         type(tEcNetCDF), pointer                :: netCDFPtr     !< NetCDF instance for the given Id
         type(tEcInstance), pointer              :: instancePtr   !< intent(in)
         integer, intent(in)                     :: netCDFId      !< unique NetCDF id
         !
         integer :: i !< loop counter
         !
         netCDFPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nNetCDFs
               if (instancePtr%ecNetCDFsPtr(i)%ptr%id == netCDFId) then
                  netCDFPtr => instancePtr%ecNetCDFsPtr(i)%ptr
               end if
            end do
         else
            call setECMessage("ec_support::ecSupportFindNetCDF: Dummy argument instancePtr is not associated.")
         end if
      end function ecSupportFindNetCDF

      ! =======================================================================
      !> Find the file reader for the bc block that contains the component definition for the comp.correction block
      function ecSupportFindRelatedBCBlock(instancePtr, corrFileReaderPtr, qname, bcname, func) result(cmpFileReaderPtr)
         type(tEcFileReader), pointer :: cmpFileReaderPtr  !< resulting file reader
         type(tEcInstance),   pointer :: instancePtr       !< intent(in)
         type(tEcFileReader), pointer :: corrFileReaderPtr !< intent(inout)
         character(len=*)             :: qname             !< quantity name
         character(len=*)             :: bcname            !< point on poly name
         integer, intent(in)          :: func              !< function type

         integer                      :: iFileReader
         type (tEcBCBlock), pointer   :: BCBlockptr 
         !
         cmpFileReaderPtr => null()
         do iFileReader = 1, instancePtr%nFileReaders
            BCBlockptr => instancePtr%EcFileReadersPtr(iFileReader)%ptr%bc
            if (associated(BCBlockptr)) then 
               if (trim(BCBlockptr%bcname)==trim(bcname) .and. (trim(BCBlockptr%qname)==trim(qname))  &
                                                         .and. BCBlockptr%func == func) then 
                  cmpFileReaderPtr => instancePtr%EcFileReadersPtr(iFileReader)%ptr 
                  exit 
               endif 
            else 
            endif 
         enddo 
      end function ecSupportFindRelatedBCBlock

      
      !> Translate NetCDF error code into a NetCDF error message.
      function ecSupportNetcdfCheckError(ierror, description, filename) result(success)
         use netcdf
         !
         logical                                   :: success     !< 
         integer,                       intent(in) :: ierror      !< 
         character(len=*),              intent(in) :: description !< 
         character(len=maxFileNameLen), intent(in) :: filename    !< 
         !
         character(3000) :: message
         !
         if (ierror /= nf90_noerr) then
            write (message,'(6a)') 'ERROR ', trim(description), '. NetCDF file : "', trim(filename), '". Error message:', nf90_strerror(ierror)
            call setECMessage(message)
            success = .false.
         else
            success = .true.
         endif
      end function ecSupportNetcdfCheckError

      ! =======================================================================

      !> Extracts time unit and reference date from a standard time string.
      !! ASCII example: "TIME = 0 hours since 2006-01-01 00:00:00 +00:00"
      !! NetCDF example: "minutes since 1970-01-01 00:00:00.0 +0000"
      function ecSupportTimestringToUnitAndRefdate(string, unit, ref_date, tzone, tzone_default) result(success)
         use netcdf
         use time_module
         !
         logical                           :: success       !< function status
         character(len=*),   intent(inout) :: string        !< units string (at out in lowercase)
         integer,  optional, intent(out)   :: unit          !< time unit enumeration
         real(hp), optional, intent(out)   :: ref_date      !< reference date formatted as Modified Julian Date
         real(hp), optional, intent(out)   :: tzone         !< time zone
         real(hp), optional, intent(in)    :: tzone_default !< default for time zone
         !
         integer :: i        !< helper index for location of 'since'
         integer :: j        !< helper index for location of '+/-' in time zone
         integer :: jplus    !< helper index for location of '+' in time zone
         integer :: jmin     !< helper index for location of '-' in time zone
         integer :: minsize  !< helper index for time zone
         real(hp):: temp     !< helper variable
         logical :: ok       !< check of refdate is found
         character(len=20) :: date, time  !< parts of string for date and time
         !
         success = .false.
         !
         call str_lower(string)
         ! Determine the time unit.
         if (present(unit)) then
            if (index(string, 'seconds') /= 0) then
               unit = ec_second
            else if (index(string, 'minutes') /= 0) then
               unit = ec_minute
            else if (index(string, 'hours') /= 0 .or. index( string, 'hrs') /= 0) then
               unit = ec_hour
            else if (index(string, 'days') /= 0) then
               unit = ec_day
            else
               call setECMessage("unitstring = '"//trim(string)//"'.")
               call setECMessage("ec_support::ecSupportTimestringToUnitAndRefdate: Unable to identify the time unit.")
               return
            end if
         end if
         ! Determine the reference date.
         i = index(string, 'since') + 6
         call split_date_time(string(i:), date, time)
         if (present(ref_date)) then
            if (i /= 6) then
               ! Date
               if (ymd2reduced_jul(date, ref_date)) then
                  ! Time
                  if(len_trim(time)>=8) then
                     read(time(1 : 2), *) temp
                     ref_date = ref_date + dble(temp) / 24.0_hp
                     read(time(4 : 5), *) temp
                     ref_date = ref_date + dble(temp) / 24.0_hp / 60.0_hp
                     read(time(7 : 8), *) temp
                     ref_date = ref_date + dble(temp) / 24.0_hp / 60.0_hp / 60.0_hp
                  end if
                  ok = .true.
               else
                  ref_date = -999.0_hp
                  ok = .false.
               endif
            else
               ok = ecSupportTimestringArcInfo(string, ref_date)
            endif
            if (.not. ok) then
               call setECMessage("ec_support::ecSupportTimestringToUnitAndRefdate: Unable to identify keyword: since.")
               return
            end if
         end if

         ! Determine the timezone
         if (present(tzone)) then
             minsize = i + 18   ! +/- is to be found after date and time (note that date has '-')
             jplus = index(string, '+', back=.true.)
             jmin  = index(string, '-', back=.true.)
             j     = max(jplus, jmin)
             if (j > minsize) then
                 if (present(tzone_default)) then
                     success = parseTimezone(string(j:), tzone, tzone_default)
                 else
                     success = parseTimezone(string(j:), tzone)
                 endif
             else
                 call setECMessage("WARNING: ec_support::ecSupportTimestringToUnitAndRefdate: no timezone found; assume same as Kernel.")
                 if (present(tzone_default)) then
                     tzone = tzone_default
                 else
                     tzone = 0.0_hp
                 endif
                 success = .true.
             endif
         else
             success = .true.
         end if
         !
      end function ecSupportTimestringToUnitAndRefdate

      !> Extracts time unit and reference date from a time string in Arc Info format.
      !! example: ... TIME (HRS)     18.0 20000101 18
      function ecSupportTimestringArcInfo(rec, ref_date, time_steps) result (success)
         character(len=*)       , intent(in)  :: rec        !< input string
         real(kind=hp), optional, intent(out) :: ref_date   !< reference date found
         real(kind=hp), optional, intent(out) :: time_steps !< time step found
         logical                              :: success    !< function result

         integer       :: yyyymmdd    !< reference date as Gregorian yyyymmdd
         integer       :: posHrs      !< position in a string of '(HRS)', 'hrs', 'hours'
         integer       :: posNumbers  !< first position of the numbers in a string (actually, the first space after '(HRS)')
         integer       :: posTime     !< position in a string of 'TIME' or 'time'
         integer       :: ierr        !< error code
         integer       :: i           !< loop counter
         real(kind=hp) :: time        !< time found
         integer       :: hh          !< hour in refdate found

         success = .false.
         posNumbers = 0

         posTime = max(index(rec, 'TIME'), index(rec, 'time'))

         if (posTime > 0) then
            posHrs = max(index(rec, '(HRS)'), index(rec, 'hrs'), index(rec, 'hours'))
            do i = posHrs+3, len_trim(rec)
               if (rec(i:i) == ' ') then
                  posNumbers = i
                  exit
               endif
            enddo
         endif

         if (present(ref_date)) then
            ref_date = -999.0_hp   ! initialize

            if (posNumbers > 0) then
               read(rec(posNumbers:), *, iostat=ierr) time, yyyymmdd, hh
               if (ierr /= 0) then
                  ! may be hh is missing, try again:
                  read(rec(posNumbers:), *, iostat=ierr) time, yyyymmdd
                  hh = 0
               endif
            endif

            if (ierr == 0) then
               success = ymd2reduced_jul(yyyymmdd, ref_date)
               ref_date = ref_date + real(hh, hp) / 24.0_hp
            endif
         endif

         if (present(time_steps)) then
            time_steps = -999.0_hp   ! initialize

            if (posNumbers > 0) then
               read(rec(posNumbers:), *, iostat=ierr) time
            endif

            if (ierr == 0) then
               time_steps = time
               success = .true.
            endif
         endif

      end function ecSupportTimestringArcInfo

      ! =======================================================================

      !> Extracts time zone from a standard time string.
      !! examples: "+01:00", "+0200", "-01:00", "-0200", "+5:30"
      function parseTimezone(string, tzone, tzone_default) result(success)
         logical                           :: success         !< function status
         character(len=*),   intent(in)    :: string          !< units string
         real(hp),           intent(out)   :: tzone           !< time zone
         real(hp), optional, intent(in)    :: tzone_default   !< default value for time zone

         integer          :: ierr        !< error code
         integer          :: jcolon      !< helper index for location of ':' in timezone
         integer          :: jend        !< helper index
         integer          :: posNulChar  !< position of null char; end of string if from C
         real(hp)         :: min         !< minutes part of time zone, as double
         real(hp)         :: hour        !< hours part of time zone, as double
         character(len=2) :: cmin        !< minutes part of time zone, as character string
         character(len=3) :: chour       !< hours part of time zone, as character string

         if (present(tzone_default)) then
            tzone = tzone_default
         else
            tzone = 0.0_hp
         endif

         jcolon = index(string, ':')

         if (jcolon == 0) then
            posNulChar = index(string, char(0))
            if (posNulChar > 0) then
               jend = posNulChar-1
            else
               jend = len_trim(string)
            endif
            cmin = string(jend-1:)
            chour = string(:jend-2)
         else
            cmin = string(jcolon+1:)
            chour = string(:jcolon-1)
         end if

         read(chour, *, iostat=ierr) hour
         if (ierr == 0) read(cmin, *, iostat=ierr) min
         if (string(1:1) == '-') then
            tzone = hour - min / 60.0_hp
         else
            tzone = hour + min / 60.0_hp
         endif

         success = (ierr == 0)
         if (.not. success) call setECMessage("ec_support::parseTimezone: error parsing time zone " // trim(string))
      end function parseTimezone

      ! =======================================================================

      !> Calculate conversion factor from ec_timestep_unit to seconds
      function ecSupportTimeUnitConversionFactor(unit) result(factor)
         integer             :: factor
         integer, intent(in) :: unit !< time unit enum
         !
         factor = 1 ! default return value
         !
         if (unit == ec_second) then
            factor = 1
         else if (unit == ec_minute) then
            factor = 60
         else if (unit == ec_hour) then
            factor = 3600
         else if (unit == ec_day) then
            factor = 3600*24
         end if
      end function ecSupportTimeUnitConversionFactor

      ! =======================================================================
      
      !> Convert times(i) * ec_timestep_unit since ec_refdate to seconds since k_refdate.
      function ecSupportTimeToTimesteps(tframe, index) result(timesteps)
         real(hp)                       :: timesteps !< function result, seconds since k_refdate
         type(tEcTimeFrame), intent(in) :: tframe    !< TimeFrame containing input data for conversion
         integer,            intent(in) :: index     !< index in times array, indicating which time needs to be converted
         !
         integer :: factor !< conversion factor from ec_timestep_unit to seconds
         !
         factor = ecSupportTimeUnitConversionFactor(tframe%ec_timestep_unit)
         !
         timesteps = tframe%times(index) * factor + (tframe%ec_refdate - tframe%k_refdate) * 60.0_hp*60.0_hp*24.0_hp
         !
         ! Correct for Kernel's timzone in seconds
         timesteps = timesteps + (tframe%k_timezone-tframe%ec_timezone) * 60.0_hp*60.0_hp
      end function ecSupportTimeToTimesteps

      ! =======================================================================
      
      !> Convert thistime * ec_timestep_unit since ec_refdate to seconds since k_refdate.
      function ecSupportThisTimeToTimesteps(tframe, thistime) result(timesteps)
         real(hp)                       :: timesteps !< function result, seconds since k_refdate
         type(tEcTimeFrame), intent(in) :: tframe    !< TimeFrame containing input data for conversion
         real(hp),           intent(in) :: thistime  !< this time needs to be converted
         !
         integer :: factor_in    !< conversion factor from ec_timestep_unit to seconds    (EC-module)
         integer :: factor_out   !< conversion factor from k_timestep_unit to seconds     (Kernel)
         integer :: factor       !< resulting conversion factor
         !
         if (tframe%k_refdate > (-1.0d+0 + 1.0d-10)) then
            ! convert time stamp in file (*.tmp) to kernel time stamp
            factor_in = ecSupportTimeUnitConversionFactor(tframe%ec_timestep_unit)
            factor_out = ecSupportTimeUnitConversionFactor(tframe%k_timestep_unit)
            factor = real(factor_in)/real(factor_out)
            !
            timesteps = thistime * factor + (tframe%ec_refdate - tframe%k_refdate) * 60.0_hp*60.0_hp*24.0_hp
            !
            ! Correct for difference in Kernel's timezone and EC's timezone.
            timesteps = timesteps + (tframe%k_timezone - tframe%ec_timezone)*3600.0_hp
         else
            ! no kernel ref date defined, convert to modified julian day
            factor_in = ecSupportTimeUnitConversionFactor(tframe%ec_timestep_unit)
            timesteps = tframe%ec_refdate + factor_in * thistime / 86400.0_hp
         endif
      end function ecSupportThisTimeToTimesteps
      

      !> Find the CF-compliant longitude and latitude dimensions and associated variables
      function ecSupportNCFindCFCoordinates(ncid, lon_varid, lon_dimid, lat_varid, lat_dimid,      &
                                             grid_lon_varid, grid_lat_varid,                       &
                                                    x_varid,   x_dimid,   y_varid,   y_dimid,      &
                                                    z_varid,   z_dimid,                            &
                                                  tim_varid, tim_dimid) result(success)
      use netcdf
      logical              :: success
      integer, intent(in)  :: ncid           !< NetCDF file ID
      integer, intent(out) :: lon_varid      !< One dimensional coordinate variable recognized as absolute longitude
      integer, intent(out) :: lat_varid      !< One dimensional coordinate variable recognized as absolute latitude
      integer, intent(out) :: grid_lon_varid !< One dimensional coordinate variable recognized as 'rotated-pole' longitude
      integer, intent(out) :: grid_lat_varid !< One dimensional coordinate variable recognized as 'rotated-pole' latitude
      integer, intent(out) ::   x_varid      !< One dimensional coordinate variable recognized as X
      integer, intent(out) ::   y_varid      !< One dimensional coordinate variable recognized as Y
      integer, intent(out) ::   z_varid      !< One dimensional coordinate variable recognized as Z
      integer, intent(out) :: tim_varid      !< One dimensional coordinate variable recognized as time
      integer, intent(out) :: lon_dimid      !< Longitude dimension
      integer, intent(out) :: lat_dimid      !< Latitude dimension
      integer, intent(out) ::   x_dimid      !< X dimension
      integer, intent(out) ::   y_dimid      !< Y dimension
      integer, intent(out) ::   z_dimid      !< Z dimension
      integer, intent(out) :: tim_dimid      !< Time dimension
      integer :: ndim, nvar, ivar, nglobatts, unlimdimid, ierr
      integer :: dimids(1)
      character(len=NF90_MAX_NAME)  :: units, axis, varname, stdname

      success = .False.
      lon_varid = -1
      lat_varid = -1
      x_varid = -1
      y_varid = -1
      z_varid = -1
      tim_varid = -1
      lon_dimid = -1
      lat_dimid = -1
      x_dimid = -1
      y_dimid = -1
      z_dimid = -1
      tim_dimid = -1
      ierr = nf90_inquire(ncid, ndim, nvar, nglobatts, unlimdimid)
      do ivar=1,nvar
         ierr = nf90_inquire_variable(ncid, ivar, ndims=ndim)                      ! number of variables
         units=''
         ierr = nf90_get_att(ncid, ivar, 'units', units)
         if (ndim==1) then
            ierr = nf90_inquire_variable(ncid, ivar, dimids=dimids)                ! number of variables
            select case (trim(units))
               case ('degrees_east','degree_east','degree_E','degrees_E','degreeE','degreesE')
                  lon_varid = ivar
                  lon_dimid = dimids(1)
               case ('degrees_north','degree_north','degree_N','degrees_N','degreeN','degreesN')
                  lat_varid = ivar
                  lat_dimid = dimids(1)
               case ('degrees')
                   stdname = ''
                   ierr = nf90_get_att(ncid, ivar, 'standard_name', stdname)
                   if (ierr == 0) then
                      select case (stdname) 
                         case ('grid_longitude')
                            grid_lon_varid = ivar
                            lon_dimid = dimids(1)
                         case ('grid_latitude')
                            grid_lat_varid = ivar
                            lat_dimid = dimids(1)
                      end select
                   else
                      call setECmessage("attribute 'standard_name' not found for variable " // trim(varname))
                   endif
                   !RL Set lon and lat dimids ??
               case ('m','meters','km','kilometers')
                  axis=''
                  ierr = nf90_get_att(ncid, ivar, 'axis', axis)
                  if (ierr /= 0) ierr = nf90_get_att(ncid, ivar, 'AXIS', axis) ! support 'axis' in upper case, lower case and camel case
                  if (ierr /= 0) ierr = nf90_get_att(ncid, ivar, 'Axis', axis)

                  if (ierr /= 0) then
                      ierr = nf90_inquire_variable(ncid, ivar, name = varname)
                      call setECmessage("attribute 'axis' not found for variable " // trim(varname))
                  else if (strcmpi(axis,'X')) then
                     x_varid = ivar
                     x_dimid = dimids(1)
                  else if (strcmpi(axis,'Y')) then
                     y_varid = ivar
                     y_dimid = dimids(1)
                  else if (strcmpi(axis,'Z')) then
                     z_varid = ivar
                     z_dimid = dimids(1)
                  end if
               case default
                  ! see if is the time dimension
                  if ((index(units,'seconds since')>0)   &
                  .or.(index(units,'minutes since')>0)   &
                  .or.(index(units,'hours since')>0)     &
                  .or.(index(units,'days since')>0))     then
                     tim_varid = ivar
                     tim_dimid = dimids(1)
                  end if
               end select
         end if
         if (ndim==2) then                ! Find lat and lon even if they are no coordinate axis
            select case (trim(units))
               case ('degrees_east','degree_east','degree_E','degrees_E','degreeE','degreesE')
                  lon_varid = ivar
               case ('degrees_north','degree_north','degree_N','degrees_N','degreeN','degreesN')
                  lat_varid = ivar
               case ('degrees')
                   stdname = ''
                   ierr = nf90_get_att(ncid, ivar, 'standard_name', stdname)
                   if (ierr == 0) then
                      select case (stdname) 
                         case ('grid_latitude')
                            grid_lat_varid = ivar
                         case ('grid_longitude')
                            grid_lon_varid = ivar
                      end select
                   else
                      call setECmessage("attribute 'standard_name' not found for variable " // trim(varname))
                   endif
               case ('m','meters','km','kilometers')
                   stdname = ''
                   ierr = nf90_get_att(ncid, ivar, 'standard_name', stdname)
                   select case (stdname) 
                      case ('projection_x_coordinate')
                         x_varid = ivar
                      case ('projection_y_coordinate')
                         y_varid = ivar
                   end select
               end select
         end if
      end do   !ivar
      
      success = .True.
      end function ecSupportNCFindCFCoordinates

      !!> Return dimension id's and lengths for the specified variable
      !function ecSupportNCGetVarDim(ncid, varid, dimid, dimlen, ndim) result(success)
      !use netcdf
      !logical              :: success
      !integer, intent(in)  :: ncid                      !< NetCDF file ID
      !integer, intent(in)  :: varid                     !< Variable file ID
      !
      !integer, dimension(:), allocatable :: intent(out) !< array of dimension ID's for this variable 
      !integer, dimension(:), allocatable :: intent(out) !< array of dimension s for this variable 
      !end function ecSupportNCGetVarDim

end module m_ec_support

! =============================================================================

!> This module contains the allocation methods for the EC-module's pointer arrays.
module m_ec_alloc
   use m_ec_typedefs
   use m_ec_message
   
   implicit none
   
   private
   
   public :: ecArrayIncrease
   
   interface ecArrayIncrease
      module procedure ecConnectionPtrArrayIncrease
      module procedure ecConverterPtrArrayIncrease
      module procedure ecElementSetPtrArrayIncrease
      module procedure ecFieldPtrArrayIncrease
      module procedure ecFileReaderPtrArrayIncrease
      module procedure ecBCBlockPtrArrayIncrease
      module procedure ecNetCDFPtrArrayIncrease
      module procedure ecItemPtrArrayIncrease
      module procedure ecQuantityPtrArrayIncrease
   end interface ecArrayIncrease
   
   contains
      
      !> Increases the size of an array of tEcConnectionPtr instances by 10.
      function ecConnectionPtrArrayIncrease(ptr, nConnections) result(success)
         logical                                       :: success      !< function status
         type(tEcConnectionPtr), dimension(:), pointer :: ptr          !< intent(inout)
         integer                                       :: nConnections !< Number of tEcConnectionPtrs =< size(ptr)
         !
         integer                                       :: istat   !< allocate() status
         type(tEcConnectionPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                       :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ec_alloc::ecConnectionPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nConnections
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ec_alloc::ecConnectionPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecConnectionPtrArrayIncrease
      
      ! =======================================================================
      
      !> Increases the size of an array of tEcConverterPtr instances by 10.
      function ecConverterPtrArrayIncrease(ptr, nConverters) result(success)
         logical                                      :: success      !< function status
         type(tEcConverterPtr), dimension(:), pointer :: ptr          !< intent(inout)
         integer                                      :: nConverters  !< Number of tEcConverterPtrs =< size(ptr)
         !
         integer                                      :: istat   !< allocate() status
         type(tEcConverterPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                      :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ec_alloc::ecConverterPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nConverters
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ec_alloc::ecConverterPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecConverterPtrArrayIncrease
      
      ! =======================================================================
      
      !> Increases the size of an array of tEcElementSetPtr instances by 10.
      function ecElementSetPtrArrayIncrease(ptr, nElementSets) result(success)
         logical                                       :: success      !< function status
         type(tEcElementSetPtr), dimension(:), pointer :: ptr          !< intent(inout)
         integer                                       :: nElementSets !< Number of tEcElementSetPtrs =< size(ptr)
         !
         integer                                       :: istat   !< allocate() status
         type(tEcElementSetPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                       :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ec_alloc::ecElementSetPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nElementSets
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ec_alloc::ecElementSetPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecElementSetPtrArrayIncrease
      
      ! =======================================================================
      
      !> Increases the size of an array of tEcFieldPtr instances by 10.
      function ecFieldPtrArrayIncrease(ptr, nFields) result(success)
         logical                                  :: success !< function status
         type(tEcFieldPtr), dimension(:), pointer :: ptr     !< intent(inout)
         integer                                  :: nFields !< Number of tEcFieldPtrs =< size(ptr)
         !
         integer                                  :: istat   !< allocate() status
         type(tEcFieldPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                  :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ec_alloc::ecFieldPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nFields
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ec_alloc::ecFieldPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecFieldPtrArrayIncrease
      
      ! =======================================================================
      
      !> Increases the size of an array of tEcFileReaderPtr instances by 10.
      function ecFileReaderPtrArrayIncrease(ptr, nFileReaders) result(success)
         logical                                       :: success      !< function status
         type(tEcFileReaderPtr), dimension(:), pointer :: ptr          !< intent(inout)
         integer                                       :: nFileReaders !< Number of tEcFileReaderPtrs =< size(ptr)
         !
         integer                                       :: istat   !< allocate() status
         type(tEcFileReaderPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                       :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ec_alloc::ecFileReaderPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nFileReaders
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ec_alloc::ecFileReaderPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecFileReaderPtrArrayIncrease

      ! =======================================================================
      
      !> Increases the size of an array of tEcBCBlockPtr instances by 10.
      function ecBCBlockPtrArrayIncrease(ptr, nBCBlocks) result(success)
         logical                                       :: success      !< function status
         type(tEcBCBlockPtr), dimension(:), pointer    :: ptr          !< intent(inout)
         integer                                       :: nBCBlocks    !< Number of tEcBCBlockPtrs =< size(ptr)
         !
         integer                                       :: istat   !< allocate() status
         type(tEcBCBlockPtr), dimension(:), pointer    :: new_ptr !< new array
         integer                                       :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ec_alloc::ecBCBlockPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nBCBlocks
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ec_alloc::ecBCBlockPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecBCBlockPtrArrayIncrease
      
      ! =======================================================================
      
      !> Increases the size of an array of tEcNetCDFPtr instances by 10.
      function ecNetCDFPtrArrayIncrease(ptr, nNetCDFs) result(success)
         logical                                       :: success      !< function status
         type(tEcNetCDFPtr), dimension(:), pointer     :: ptr          !< intent(inout)
         integer                                       :: nNetCDFs     !< Number of tEcBCBlockPtrs =< size(ptr)
         !
         integer                                       :: istat   !< allocate() status
         type(tEcNetCDFPtr), dimension(:), pointer     :: new_ptr !< new array
         integer                                       :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ec_alloc::ecNetCDFPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nNetCDFs
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ec_alloc::ecNetCDFPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecNetCDFPtrArrayIncrease
      
      ! =======================================================================

      !> Increases the size of an array of tEcItemPtr instances by 10.
      function ecItemPtrArrayIncrease(ptr, nItems) result(success)
         logical                                 :: success !< function status
         type(tEcItemPtr), dimension(:), pointer :: ptr     !< intent(inout)
         integer                                 :: nItems  !< Number of tEcItemPtrs =< size(ptr)
         !
         integer                                 :: istat   !< allocate() status
         type(tEcItemPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                 :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ec_alloc::ecItemPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nItems
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ec_alloc::ecItemPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecItemPtrArrayIncrease
      
      ! =======================================================================
      
      !> Increases the size of an array of tEcQuantityPtr instances by 10.
      function ecQuantityPtrArrayIncrease(ptr, nQuantitys) result(success)
         logical                                     :: success    !< function status
         type(tEcQuantityPtr), dimension(:), pointer :: ptr        !< intent(inout)
         integer                                     :: nQuantitys !< Number of tEcQuantityPtrs =< size(ptr)
         !
         integer                                     :: istat   !< allocate() status
         type(tEcQuantityPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                     :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ec_alloc::ecQuantityPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nQuantitys
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ec_alloc::ecQuantityPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecQuantityPtrArrayIncrease
      
end module m_ec_alloc
