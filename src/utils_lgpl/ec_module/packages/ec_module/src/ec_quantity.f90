!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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

!> This module contains all the methods for the datatype tEcQuantity.
!! @author arjen.markus@deltares.nl
!! @author adri.mourits@deltares.nl
!! @author stef.hummel@deltares.nl
!! @author edwin.bos@deltares.nl
module m_ec_quantity
   use m_ec_typedefs
   use m_ec_message
   use m_ec_support
   use m_ec_alloc
   
   implicit none
   
   private
   
   public :: ecQuantityCreate
   public :: ecQuantityFree1dArray
   public :: ecQuantitySet
   public :: ecQuantitySetName
   public :: ecQuantitySetUnits
   public :: ecQuantitySetVectorMax
   public :: ecQuantitySetFillValue
   public :: ecQuantitySetScaleOffset
   
   contains
      
      ! =======================================================================
      
      !> Construct a new Quantity with the specified id.
      !! Failure is indicated by returning a null pointer.
      function ecQuantityCreate(quantityId) result(quantityPtr)
         type(tEcQuantity), pointer    :: quantityPtr !< the new Quantity, intent(out)
         integer,           intent(in) :: quantityId  !< unique Quantity id
         !
         integer :: istat !< allocate() status
         !
         ! allocation
         allocate(quantityPtr, stat = istat)
         if (istat /= 0) then
            call setECMessage("ERROR: ec_quantity::ecQuantityCreate: Unable to allocate additional memory.")
            quantityPtr => null()
            return
         end if
         ! initialization
         quantityPtr%id = quantityId
         quantityPtr%name = ' '
         quantityPtr%units = ' '
      end function ecQuantityCreate
      
      ! =======================================================================
      
      !> Frees a 1D array of tEcQuantityPtrs, after which the quantityPtr is deallocated.
      function ecQuantityFree1dArray(quantityPtr, nQuantities) result (success)
         logical                                     :: success     !< function status
         type(tEcQuantityPtr), dimension(:), pointer :: quantityPtr !< intent(inout)
         integer, intent(inout)                      :: nQuantities !< number of Quantities
         !
         integer :: i     !< loop counter
         integer :: istat !< deallocate() status
         !
         success = .true.
         !
         if (.not. associated(quantityPtr)) then
            call setECMessage("WARNING: ec_quantity::ecQuantityFree1dArray: Dummy argument quantityPtr is already disassociated.")
         else
            ! Free(nothing to do) and deallocate all tEcQuantityPtrs in the 1d array.
            do i=1, nQuantities
               deallocate(quantityPtr(i)%ptr, stat = istat)
               if (istat /= 0) success = .false.
            end do
            ! Finally deallocate the tEcQuantityPtr(:) pointer.
            if (success) then
               deallocate(quantityPtr, stat = istat)
               if (istat /= 0) success = .false.
            end if
         end if
         nQuantities = 0
      end function ecQuantityFree1dArray
      
      ! =======================================================================
      
      !> Change the properties of the Quantity corresponding to quantityId.
      function ecQuantitySet(instancePtr, quantityId, newName, newUnits, newVectorMax) result(success)
         logical                               :: success      !< function status
         type(tEcInstance), pointer            :: instancePtr  !< intent(in)
         integer,                   intent(in) :: quantityId   !< unique Quantity id
         character(*),              intent(in) :: newName      !< new name of the Quantity
         character(*),              intent(in) :: newUnits     !< new units of the Quantity
         integer,                   intent(in) :: newVectorMax !< new vectormax of the Quantity
         !
         type(tEcQuantity), pointer :: quantityPtr !< Quantity corresponding to quantityId
         character(len=maxNameLen)  :: name        !< new name of the Quantity, converted to the correct length
         character(len=maxNameLen)  :: units       !< new units of the Quantity, converted to the correct length
         !
         success = .false.
         quantityPtr => null()
         !
         if (len_trim(newName) > maxNameLen) then
            call setECMessage("ERROR: ec_quantity::ecQuantitySet: The new name string is too long, unable to change name.")
         else
            name = newName
         end if
         !
         if (len_trim(newUnits) > maxNameLen) then
            call setECMessage("ERROR: ec_quantity::ecQuantitySet: The new units string is too long, unable to change units.")
            return
         else
            units = newUnits
         end if
         !
         quantityPtr => ecSupportFindQuantity(instancePtr, quantityId)
         if (associated(quantityPtr)) then
            quantityPtr%name = name
            quantityPtr%units = units
            quantityPtr%vectorMax = newVectorMax
            success = .true.
         else
            call setECMessage("ERROR: ec_quantity::ecQuantitySet: Cannot find a Quantity with the supplied id.")
         end if
      end function ecQuantitySet
      
      ! =======================================================================
      !> Change the FillValue of the Quantity corresponding to quantityId.
      function ecQuantitySetFillValue(instancePtr, quantityId, fillvalue) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: quantityId  !< unique Quantity id
         real(hp),                  intent(in) :: fillvalue   !< to be used in the case of missing values (netcdf)
         !
         type(tEcQuantity), pointer :: quantityPtr !< Quantity corresponding to quantityId
         !
         success = .false.
         quantityPtr => null()
         !
         quantityPtr => ecSupportFindQuantity(instancePtr, quantityId)
         if (associated(quantityPtr)) then
            quantityPtr%fillvalue = fillvalue
            success = .true.
         else
            call setECMessage("ERROR: ec_quantity::ecQuantitySetFillValue: Cannot find a Quantity with the supplied id.")
         end if
      end function ecQuantitySetFillValue

      ! =======================================================================
      !> Change the Scalefactor and Offset shift of the Quantity corresponding to quantityId.
      function ecQuantitySetScaleOffset(instancePtr, quantityId, scale, offset) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: quantityId  !< unique Quantity id
         real(hp),                  intent(in) :: scale       !< multiplication factor to be applied to the raw data
         real(hp),                  intent(in) :: offset      !< offset to be applied to the raw data
                                                              !< order: new = (old*scale) + offset
         !
         type(tEcQuantity), pointer :: quantityPtr !< Quantity corresponding to quantityId
         !
         success = .false.
         quantityPtr => null()
         !
         quantityPtr => ecSupportFindQuantity(instancePtr, quantityId)
         if (associated(quantityPtr)) then
            quantityPtr%factor = scale
            quantityPtr%offset = offset
            success = .true.
         else
            call setECMessage("ERROR: ec_quantity::ecQuantitySetScaleOffset: Cannot find a Quantity with the supplied id.")
         end if
      end function ecQuantitySetScaleOffset

      !> Change the name of the Quantity corresponding to quantityId.
      function ecQuantitySetName(instancePtr, quantityId, newName) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: quantityId  !< unique Quantity id
         character(*),              intent(in) :: newName     !< new name of the Quantity
         !
         type(tEcQuantity), pointer :: quantityPtr !< Quantity corresponding to quantityId
         character(len=maxNameLen)  :: name        !< new name of the Quantity, converted to the correct length
         !
         success = .false.
         quantityPtr => null()
         !
         if (len_trim(newName) > maxNameLen) then
            call setECMessage("ERROR: ec_quantity::ecQuantitySetName: The new name string is too long, unable to change name.")
         else
            name = newName
         end if
         quantityPtr => ecSupportFindQuantity(instancePtr, quantityId)
         if (associated(quantityPtr)) then
            quantityPtr%name = name
            success = .true.
         else
            call setECMessage("ERROR: ec_quantity::ecQuantitySetName: Cannot find a Quantity with the supplied id.")
         end if
      end function ecQuantitySetName

      ! =======================================================================
      
      !> Change the vectormax of the Quantity corresponding to quantityId.
      function ecQuantitySetVectorMax(instancePtr, quantityId, newVectorMax) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: quantityId  !< unique Quantity id
         integer,                   intent(in) :: newVectorMax!< new vectormax of the Quantity
         !
         type(tEcQuantity), pointer :: quantityPtr !< Quantity corresponding to quantityId
         !
         success = .false.
         quantityPtr => null()
         !
         quantityPtr => ecSupportFindQuantity(instancePtr, quantityId)
         if (associated(quantityPtr)) then
            quantityPtr%vectorMax = newVectorMax
            success = .true.
         else
            call setECMessage("ERROR: ec_quantity::ecQuantitySetVectorMax: Cannot find a Quantity with the supplied id.")
         end if
      end function ecQuantitySetVectorMax
      
      ! =======================================================================
      
      !> Change the units of the Quantity corresponding to quantityId.
      function ecQuantitySetUnits(instancePtr, quantityId, newUnits) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: quantityId  !< unique Quantity id
         character(*),              intent(in) :: newUnits    !< new units of the Quantity
         !
         type(tEcQuantity), pointer :: quantityPtr !< Quantity corresponding to quantityId
         character(len=maxNameLen)  :: units       !< new units of the Quantity, converted to the correct length
         !
         success = .false.
         quantityPtr => null()
         !
         if (len_trim(newUnits) > maxNameLen) then
            call setECMessage("ERROR: ec_quantity::ecQuantitySetUnits: The new units string is too long, unable to change units.")
            return
         else
            units = newUnits
         end if
         quantityPtr => ecSupportFindQuantity(instancePtr, quantityId)
         if (associated(quantityPtr)) then
            quantityPtr%units = units
            success = .true.
         else
            call setECMessage("ERROR: ec_quantity::ecQuantitySetUnits: Cannot find a Quantity with the supplied id.")
         end if
      end function ecQuantitySetUnits
end module m_ec_quantity
