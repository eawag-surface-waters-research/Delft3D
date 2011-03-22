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
subroutine runme(max_keyval, keys   , values   , error_message)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'RUNME' :: RUNME
!!--description-----------------------------------------------------------------
!
! 1) Interpretation of the keys/values from the config file
! 2) Create runid, based on mdfFile/ddbFile
! 3) In dd_execute:
!    - RemoteOLV initialization
!    - DD initialization
!    - Start all processes:
!
!> @file runme.f90
!! The dynamic library 'Delft3D-FLOW'.
!! Flow-related routines are in unstruc.f90
!<
!
!> \mainpage Delft3D-FLOW API docs
!! \li \b Main \b program: deltares_hydro.F90
!! \li \b Dynamic \b library \b entrance: runme.f90
!! \li \b DD \b and \b RemoteOLV \b preparations, \b start \b all \b processes: hydra.cpp (Hydra::Execute)
!! \li \b Global \b data: modules.f90 (flow, geometry, times, parameters, ...)
!! \li \b Subdomain \b calculation \b toplevel: trisim.f90
!! \li \b Preprocessor: \b Convert \b time \b related \b data: tdatom.f90
!! \li \b Timeloop: trisol.f90
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
implicit none
!
include 'fsm.i'   ! for FSM_SILENT (to start single threaded call)
!
! Subroutine arguments
!
integer                                , intent(in)  :: max_keyval
character(256), dimension(max_keyval)  , intent(in)  :: keys
character(256), dimension(max_keyval)  , intent(in)  :: values
character(256)                         , intent(out) :: error_message ! not empty: echo and stop run
!
! Local variables
!
integer        :: argsinfile   ! 1: Delft3D-FLOW args (-c ddb -M -d 9 etc) are in an ASCII inputfile
integer        :: i
integer        :: rolvWait
logical        :: ex           ! logical to test if a file exists
character(256) :: engineName
character(256) :: mdfFile      ! Name of input file of single domain Delft3D-FLOW calculation
character(256) :: ddbFile      ! Name of input file containing the DomainDecomposition boundaries
character(256) :: urlFile      ! Name of file to be written, containing remoteOLV hook
character(256) :: runid
character(256) :: jarPath
character(256) :: jrePath
character(256) :: version_full ! by calling getfullversionstring_deltares_hydro, the version number is visible with the what command
!
!! executable statements -------------------------------------------------------
!
call getfullversionstring_flow2d3d(version_full)
!
! The output argument error_message MUST have value ' ' to continue the calculation.
!
!
error_message = ' '
engineName    = 'flow2d3d'
mdfFile       = ' '
ddbFile       = ' '
runid         = ' '
jarPath       = ' '
jrePath       = ' '
urlFile       = ' '
rolvWait      = 0
!
do i = 1, max_keyval
   call small(keys(i),999)
   select case (keys(i))
   case ('component,mdffile')
      mdfFile  = values(i)
   case ('component,filewait')
      !
      ! for debugging:
      ! get some time to attach debugger to process
      !
      do
      inquire(file=trim(values(i)), exist=ex)
         if (ex) exit
         call sleep(1)
      enddo
   case ('component,ddbfile')
      ddbFile  = values(i)
   case ('remoteolv,jarpath')
      jarPath  = values(i)
      if (index(jarPath,'/') /= 0) then
         jarPath = trim(jarPath) // '/'
      else
         jarPath = trim(jarPath) // '\'
      endif
      jarPath  = trim(jarPath) // 'DelftOnline.jar'
   case ('remoteolv,jrepath')
      jrePath  = values(i)
   case ('remoteolv,wait')
      if (values(i)(1:1)=='y' .or. values(i)(1:1)=='Y') then
         rolvWait = 1
      endif
   case default
      ! error_message = 'WARNING: flow2d3d ignores key "' // trim(keys(i)) // '".'
   end select
enddo
!
! Exactly one of mdfFile and ddbFile must be defined
!
if (mdfFile==' ' .and. ddbFile==' ') then
   write(error_message,'(a)') 'ERROR: flow2d3d cannot find key "MDFfile" or "DDBfile".'
   return
endif
if (mdfFile/=' ' .and. ddbFile/=' ') then
   write(error_message,'(a)') 'ERROR: "MDFfile" and "DDBfile" are both defined.'
   return
endif
!
! Set runid based on mdffile/ddbfile
!
if (ddbFile /= ' ') then
   runid = ddbFile
else
   runid = mdfFile
endif
!
! remove extension
!
i = index(runid, '.', back=.true.)
if (i /= 0) then
   runid = runid(:i-1)
endif
runid = adjustl(runid)
!
! Set urlFile bases on runid, jarPath and jrePath
!
if (jarPath/=' ' .and. jrePath/=' ') then
   urlFile = trim(runid) // '.url'
endif
!
! Go to C/C++ code (via ddexec.cpp and hydra/hydra.cpp) to:
! - initialize for remoteOLV
! - initialize for DD
! - start all DD processes
! - call trisim to do the actual calculation
!
call dd_execute(runid, ddbFile, jarPath, jrePath, urlFile, rolvWait)
end subroutine runme
