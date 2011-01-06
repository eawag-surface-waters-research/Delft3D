program deltares_hydro
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
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use properties
implicit none
!
! Parameters
!
integer, parameter :: groupNameLen =  20
integer, parameter :: stringLen    = 256
integer, parameter :: numGroups    =   2
!
! Type defs
!
type group
   character(groupNameLen)  :: name
   integer                  :: numChilds
   type(tree_data), pointer :: ptr
end type group
!
! Local variables
!
integer                                         :: error
integer                                         :: libHandle
integer                                         :: numKeyVal
integer                                         :: mode       ! integer flag denoting whether delft3d arguments are in a file (1) or not (0)
integer                                         :: i
integer                                         :: j
integer                                         :: k
integer                                         :: istat
integer                                         :: numCompChilds
integer                                         :: numOlvChilds
integer                                         :: libNameSet
integer, external                               :: iargc
integer, external                               :: open_shared_library
integer, external                               :: close_shared_library
integer, external                               :: perform_function
logical                                         :: dllIsOpen
logical                                         :: ex
logical                                         :: success
character(stringLen), dimension(:), allocatable :: arguments ! all commandline arguments
character(stringLen), dimension(:), allocatable :: keys      ! keys   from config file to be passed to shared library
character(stringLen), dimension(:), allocatable :: values    ! values from config file to be passed to shared library
character(stringLen)                            :: message
character(stringLen)                            :: libName
character(stringLen)                            :: libFunction
character(stringLen)                            :: inputFile
character(groupNameLen)                         :: groupName
character(256)                                  :: version_full ! by calling getfullversionstring_deltares_hydro, the version number is visible with the what command
type(tree_data), pointer                        :: configPtr
type(group), dimension(numGroups)               :: configGroups
!
!! executable statements -------------------------------------------------------
!
call getfullversionstring_deltares_hydro(version_full)
nullify(configPtr)
do i=1, numGroups
   configGroups(i)%numChilds = 0
   nullify(configGroups(i)%ptr)
enddo
configGroups(1)%name = 'component'
configGroups(2)%name = 'remoteolv'
dllIsOpen            = .false.
numKeyVal            = 0
!
! iargc is used for allocation array arguments
! check whether it has a realistic value
!
if (iargc() /= 1) then
   call printUsage()
   call deltaresHydroStop()
endif
allocate(arguments(0:iargc()))
do i=0,iargc()
   call getarg(i,arguments(i))
enddo
!
! Check existance of the configuration file, specified in the first argument
!
inquire (file = trim(arguments(1)), exist = ex)
if (.not. ex) then
   write(*,'(3a)') 'ERROR: Configuration file "', trim(arguments(1)), '" does not exist.'
   call printUsage()
   call deltaresHydroStop()
endif
call tree_create('Trunk', configPtr)
call tree_put_data( configPtr, transfer(trim(arguments(1)),node_value), 'STRING' )
!
! Put config-file in input tree
!
call prop_file('ini', trim(arguments(1)), configPtr, istat)
if (istat /= 0) then
   select case (istat)
   case(1)
      write(*,'(3a)') 'ERROR: Configuration file "', trim(arguments(1)), '" not found.'
   case(3)
      write(*,'(3a)') 'ERROR: Premature EOF in file "', trim(arguments(1)), '".'
   case default
      write(*,'(3a)') 'ERROR: Read error from file "', trim(arguments(1)), '".'
   endselect
   call printUsage()
   call deltaresHydroStop()
endif
!
! Read config file
!
!
! Search for groups
!
groupName = ' '
do i = 1, size(configPtr%child_nodes)
   groupName = tree_get_name(configPtr%child_nodes(i)%node_ptr)
   do j = 1, numGroups
      if (trim(groupName) == configGroups(j)%name) then
         configGroups(j)%ptr => configPtr%child_nodes(i)%node_ptr
         configGroups(j)%numChilds = size(configGroups(j)%ptr%child_nodes)
         numKeyVal = numKeyVal + configGroups(j)%numChilds
      endif
   enddo
enddo
!
! group [Component] must exist
!
if (.not.associated(configGroups(1)%ptr)) then
   write(*,'(5a)') 'ERROR: in config file "', trim(arguments(1)), '", group "[Component]" not found.'
   call printUsage()
   call deltaresHydroStop()
endif
!
allocate(keys  (numKeyVal))
allocate(values(numKeyVal))
keys        = ' '
values      = ' '
libName     = ' '
libNameSet  = 0
libFunction = 'runme'
call upperCase(libFunction)
k           = 0
do i = 1, numGroups
   do j = 1, configGroups(i)%numChilds
      k = k + 1
      keys(k) = trim(configGroups(i)%name) // ',' // tree_get_name(configGroups(i)%ptr%child_nodes(j)%node_ptr)
      call tree_get_data_string(configGroups(i)%ptr%child_nodes(j)%node_ptr, values(k), success)
      if (.not. success) then
         write(*,'(4a)') 'ERROR: Can not read value of keyword "', trim(keys(k)), '" in config file "', trim(arguments(1)), '".'
      endif
      if (keys(k) == trim(configGroups(1)%name) // ',' // 'name' .and. values(k) /= ' ') then
         !
         ! Non-empty library name placed in values(i)
         !
         libNameSet = k
      endif
      if (keys(k)/= trim(configGroups(i)%name) // ',' .and. values(k)==' ') then
         write(*,'(5a)') 'ERROR: in config file "', trim(arguments(1)), '", keyword "', trim(keys(k)), '" has an empty value.'
         call printUsage()
         call deltaresHydroStop()
      endif
   enddo
enddo
!
! Check that libName and LibFunction are specified in inifile
!
if (libNameSet == 0) then
   write(*,'(3a)') 'ERROR: Can not find keyword "Name" in config file "', trim(arguments(1)), '".'
   call printUsage()
   call deltaresHydroStop()
endif
!
! Open shared library
!
! If Windows then <libname>.dll, if linux then lib<libname>.so
!
#if defined (WIN32)
libName = trim(values(libNameSet)) // '.dll'
#endif
#if defined (HAVE_CONFIG_H)
libName = 'lib' // trim(values(libNameSet)) // '.so'
#endif
!
istat   = 0
istat   = open_shared_library(libHandle, trim(libName))
if (istat /= 0) then
   write(*,'(3a)') 'ERROR: Can not open shared library "', trim(libName), '".'
   call printUsage()
   call deltaresHydroStop()
endif
dllIsOpen = .true.
!
! Execute the named function in the shared library
!
error   = 0
message = ' '
error = perform_function(libHandle, libFunction, &
                         numKeyVal, keys       , values, message)
if (error /= 0) then
   write(*,'(4a)') 'ERROR: Cannot find function "',trim(libFunction),'" in dynamic library "',trim(libName),'".'
   call deltaresHydroStop()
endif
if (message /= ' ') then
   write (*,'(5a)') 'Message from function "',trim(libFunction),'" in dynamic library "',trim(libName),'":'
   write (*,'(7x,a)') trim(message)
   call deltaresHydroStop()
endif
call deltaresHydroStop()


contains
subroutine deltaresHydroStop()
   if (allocated(arguments) ) deallocate(arguments)
   if (allocated(keys)      ) deallocate(keys)
   if (allocated(values)    ) deallocate(values)
   ! TODO: call tree_fold instead of deallocate
   if (associated(configPtr)) deallocate(configPtr)
   if (dllIsOpen) then
      istat = close_shared_library(libHandle)
   endif
   stop
end subroutine deltaresHydroStop


end program deltares_hydro



subroutine printUsage()
   implicit none
   write(*,'(a)') "Usage:"
   write(*,'(a)') "deltares_hydro.exe <config.ini>"
   write(*,'(a)') "    <config.ini>  : Name of configuration file in ini format"
   write(*,'(a)') "                    Example configuration file:"
   write(*,'(a)') "                    [Component]"
   write(*,'(a)') "                       Name    = flow2d3d"
   write(*,'(a)') "                       MdfFile = f34"
end subroutine printUsage

!
! Support functions for string compare
!
subroutine upperCase(string)
   ! arguments
   character(*), intent(inout) :: string ! incoming /resulting (lowercase) string
   ! locals
   integer :: i
   integer :: j
   ! body
   do i=1,len(string)
      j=ichar(string(i:i))
      if (j > 96 .and. j<123) then
         string(i:i) = char(j-32)
      endif
   enddo
end subroutine upperCase
subroutine lowerCase(string)
   ! arguments
   character(*), intent(inout) :: string ! incoming /resulting (lowercase) string
   ! locals
   integer :: i
   integer :: j
   ! body
   do i=1,len(string)
      j=ichar(string(i:i))
      if (j > 64 .and. j<91) then
         string(i:i) = char(j+32)
      endif
   enddo
end subroutine lowerCase
