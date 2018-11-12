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

!> This module contains the messaging system.
!! @author arjen.markus@deltares.nl
!! @author adri.mourits@deltares.nl
!! @author stef.hummel@deltares.nl
!! @author edwin.spee@deltares.nl
module m_ec_message
   use precision
   
   implicit none
   
   private
   
    public :: maxMessageLen
    public :: clearECMessage
    public :: setECMessage
    public :: getECMessage
    public :: getECMsgLenTrim
    public :: dumpECMessageStack

   integer,                 parameter :: maxMessageLen = 1000

    type TEcMessage
      character(len=:), allocatable :: message
      integer                       :: message_type
    end type TEcMessage

    type(TEcMessage), pointer :: EcMessages(:) => null()
    integer                   :: EcMsgActualSize = 0

   interface setECMessage
      module procedure setECMessage_char
      module procedure setECMessage_int
   end interface
   
   contains
      
      ! =======================================================================
      
      !> 
      subroutine clearECMessage()
         EcMsgActualSize = 0
      end subroutine clearECMessage

      subroutine IncEcMessages()
         integer :: cursize, newsize, i
         type(TEcMessage), pointer :: newStack(:)

         if (.not. associated(EcMessages)) then
            allocate(EcMessages(10))
         else
            cursize = size(EcMessages)
            newsize = EcMsgActualSize + 1

            if (newsize > cursize) then
               allocate(newStack(10+cursize))
               do i = 1, EcMsgActualSize
                  newStack(i) = EcMessages(i)
               enddo
               deallocate(EcMessages)
               EcMessages => newStack
            endif
         endif
      end subroutine IncEcMessages

      subroutine setECMessage_char(string, suffix)
         character(len=*), intent(in)           :: string
         character(len=*), intent(in), optional :: suffix
         !
         call IncEcMessages()

         EcMsgActualSize = EcMsgActualSize + 1

         if (present(suffix)) then
            ECMessages(EcMsgActualSize)%message = trim(string)  // " " // suffix
         else
            ECMessages(EcMsgActualSize)%message = trim(string)
         endif
         ECMessages(EcMsgActualSize)%message_type = -1
      end subroutine setECMessage_char

      ! =======================================================================

      !> 
      subroutine setECMessage_int(string, val)
         character(len=*), intent(in) :: string
         integer,          intent(in) :: val
         !
         character(len=8) :: cvalue

         call IncEcMessages()

         EcMsgActualSize = EcMsgActualSize + 1

         write(cvalue, '(i8)') val
         cvalue = adjustl(cvalue)

         ECMessages(EcMsgActualSize)%message = trim(string) // ' ' // trim(cvalue)
         ECMessages(EcMsgActualSize)%message_type = -1
      end subroutine setECMessage_int

      ! =======================================================================

      !> 
      function getECMessage() result(retval)
         character(len=maxMessageLen) :: retval
         integer :: i
         !
         retval    = ' '
         do i = 1, EcMsgActualSize
            retval = retval // '|' // trim(EcMessages(i)%message)
         enddo
         EcMsgActualSize = 0
      end function getECMessage

      ! =======================================================================

      function dumpECMessageStack(msglevel,messenger) result(retval)
      implicit none 
      integer, intent(in)           :: msglevel
      interface 
         subroutine messenger(lvl,msg)
         integer, intent(in)              :: lvl
         character(len=*), intent(in)    :: msg 
         end subroutine
      end interface
      character(len=maxMessageLen) :: retval
      integer                      :: i

      call messenger (msglevel,"...")! separator 
      do i=1, EcMsgActualSize
         call messenger (msglevel,EcMessages(i)%message)
      enddo
      retval = 'Fatal EC-error !!'           ! TODO: make this a meaningful return string 
      end function dumpECMessageStack

      !> 
      function getECMsgLenTrim () result(ECMsgLenTrim)
         integer :: ECMsgLenTrim

         integer :: i
         !
         ECMsgLenTrim = 0
         do i = 1, EcMsgActualSize
            ECMsgLenTrim = ECMsgLenTrim + len(ECMessages(i)%message)
         enddo
      end function getECMsgLenTrim
end module m_ec_message
