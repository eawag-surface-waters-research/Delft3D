module string_module
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                
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
!                                                                               
!-------------------------------------------------------------------------------
!  $Id$
!  $HeadURL$
!!--description-----------------------------------------------------------------
!
!    Function: - Various string processing routines
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------

private

!
! functions and subroutines
!
public string_module_info
public str_token
public str_lower
public str_upper
public strcmpi
public remove_leading_spaces
public remove_all_spaces

contains



! ------------------------------------------------------------------------------
!   Subroutine: string_module_info
!   Purpose:    Add info about this string module to the messages stack
!   Summary:    Add id string and URL
!   Arguments:
!   messages    Stack of messages to add the info to
! ------------------------------------------------------------------------------
subroutine string_module_info(messages)
    use message_module
    !
    ! Call variables
    !
    type(message_stack), pointer :: messages
    !
    !! executable statements ---------------------------------------------------
    !
    call addmessage(messages,'$Id$')
    call addmessage(messages,'$URL$')
end subroutine string_module_info



! ------------------------------------------------------------------------------
!   Subroutine: str_token
!   Purpose:    Obtain first token from string
!   Summary:    Scan string for non-space characters and return first set found.
!   Arguments:
!   string      on input : String to be scanned
!               on output: Remainder of string
!   token       on output: String containing token
!   quote       on input : Optional quote character
! ------------------------------------------------------------------------------
subroutine str_token(string, token, quote)
    implicit none
    !
    ! Call variables
    !
    character(*)          , intent(inout) :: string
    character(*)          , intent(out)   :: token
    character(1), optional, intent(in)    :: quote
    !
    ! Local variables
    !
    integer :: i
    integer :: i1
    integer :: i2
    integer :: j
    integer :: strlen
    logical :: quoted
    !
    !! executable statements ---------------------------------------------------
    !
    i1     = -1
    i2     = -1
    quoted = .false.
    strlen = len_trim(string)
    ! find start of token
    do i = 1, strlen
       j = ichar(string(i:i))
       if (j == 32 .or. j == 9 .or. j == 10 .or. j == 13) then
          ! a space
          if (i1>0 .and. .not.quoted) then
             ! token ends here
             i2 = i-1
             exit
          endif
       else
          ! not a space
          if (i1<0) then
             ! token starts here and may continue till the end of the string
             if (present(quote)) then
                if (string(i:i) == quote) then
                   quoted = .true.
                endif
             endif
             i1 = i
             i2 = strlen
          elseif (quoted) then
             if (string(i:i) == quote) then
                quoted = .false.
             endif
          endif
       endif
    enddo
    !
    if (i1<0) then
       ! empty string: no token found
       token = ' '
    else
       ! token found
       token  = string(i1:i2)
       if (present(quote)) then
          ! remove quotes
          if (string(i1:i1)==quote .and. string(i2:i2)==quote) then
             token = string(i1+1:i2-1)
          endif
       endif
       string = string(i2+1:strlen)
    endif
end subroutine str_token



! ------------------------------------------------------------------------------
!   Subroutine: str_lower
!   Purpose:    Convert upper case characters to lower case
!   Summary:    Scan string for upper case characters and
!               convert them.
!   Arguments:
!   string      String to be converted
!   lenstr      Optional length of string to be converted
! ------------------------------------------------------------------------------
subroutine str_lower(string, lenstr)
    implicit none
    !
    ! Call variables
    !
    integer     , optional, intent(in) :: lenstr
    character(*)                       :: string
    !
    ! Local variables
    !
    integer :: i
    integer :: j
    integer :: newlen
    !
    !! executable statements ---------------------------------------------------
    !
    if (present(lenstr)) then
       newlen = min(lenstr, len_trim(string))
    else
       newlen = len_trim(string)
    endif
    do i = 1, newlen
       j = ichar(string(i:i))
       if ((j>64) .and. (j<91)) then
          j = j + 32
          string(i:i) = char(j)
       endif
    enddo
end subroutine str_lower



! ------------------------------------------------------------------------------
!   Subroutine: str_upper
!   Purpose:    Convert lower case characters to upper case
!   Summary:    Scan string for lower case characters and
!               convert them.
!   Arguments:
!   string      String to be converted
!   lenstr      Optional length of string to be converted
! ------------------------------------------------------------------------------
subroutine str_upper(string, lenstr)
    implicit none
    !
    ! Call variables
    !
    integer     , optional, intent(in) :: lenstr
    character(*)                       :: string
    !
    ! Local variables
    !
    integer :: i
    integer :: j
    integer :: newlen
    !
    !! executable statements ---------------------------------------------------
    !
    if (present(lenstr)) then
       newlen = min(lenstr, len_trim(string))
    else
       newlen = len_trim(string)
    endif
    do i = 1, newlen
       j = ichar(string(i:i))
       if ((j>96) .and. (j<123)) then
          j = j - 32
          string(i:i) = char(j)
       endif
    enddo
end subroutine str_upper



! ------------------------------------------------------------------------------
!   Subroutine: remove_all_spaces
!   Purpose:    Remove all spaces from a string
!   Summary:    Scan string for space characters and if one exists, move the
!               following characters forward.
!   Arguments:
!   string      String to be converted
!   lenstr      Optional trimmed length of string after removal of spaces
! ------------------------------------------------------------------------------
subroutine remove_all_spaces(string, lenstr)
    implicit none
    !
    ! Call variables
    !
    character(*)                       :: string
    integer     , optional, intent(out):: lenstr
    !
    ! Local variables
    !
    integer :: i
    integer :: newlen
    !
    !! executable statements ---------------------------------------------------
    !
    newlen = len_trim(string)
    !
    ! loop over all characters in string
    !    if it is a space character, move remainder of string forward
    !
    i = 1
    do while (i<newlen)
       if (string(i:i)==' ') then
          string(i:newlen) = string(i+1:newlen) // ' '
          newlen = newlen-1
       else
          i = i+1
       endif
    enddo
    !
    if (present(lenstr)) then
       lenstr = newlen
    endif
end subroutine remove_all_spaces



! ------------------------------------------------------------------------------
!   Subroutine: remove_leading_spaces
!   Purpose:    Remove leading spaces from a string
!   Summary:    Scan string for space characters at beginning of string and
!               if they exist, move the actual string forward.
!   Arguments:
!   string      String to be converted
!   lenstr      Optional trimmed length of string after removal of spaces
! ------------------------------------------------------------------------------
subroutine remove_leading_spaces(string, lenstr)
    implicit none
    !
    ! Call variables
    !
    character(*)                       :: string
    integer     , optional, intent(out):: lenstr
    !
    ! Local variables
    !
    integer :: i
    integer :: newlen
    !
    !! executable statements ---------------------------------------------------
    !
    newlen = len_trim(string)
    !
    ! find first non-space character
    !
    i = 1
    do while (i<newlen)
       if (string(i:i)==' ') then
          i = i+1
       else
          exit
       endif
    enddo
    !
    ! remove leading spaces
    !
    string = string(i:newlen)
    !
    if (present(lenstr)) then
       lenstr = len_trim(string)
    endif
end subroutine remove_leading_spaces



! ------------------------------------------------------------------------------
!   Function:   strcmpi
!   Purpose:    Case-insensitive comparison of strings (upto certain length)
!   Summary:    Change strings to lower case and compare (sub)strings.
!   Arguments:
!   string1     First string to be compared
!   string2     Second string to be compared
!   lencmp      Optional length over which to compare strings
! ------------------------------------------------------------------------------
function strcmpi(string1, string2, lenreq) result(retval)
    implicit none
    !
    ! Call variables
    !
    character(*)                   , intent(in) :: string1
    character(*)                   , intent(in) :: string2
    integer              , optional, intent(in) :: lenreq
    logical                                     :: retVal  ! .true.  if strings are equal
                                                           ! .false. if strings are not equal or len1 /= len2
    !
    ! Local variables
    !
    integer                                     :: len1    ! length of string1, without trailing blanks
    integer                                     :: len2    ! length of string2, without trailing blanks
    integer                                     :: lencmp  ! length of strings to be compared
    character(999) , dimension(:) , allocatable :: locstr  ! copy of strings, to convert to lowercase
    !
    !! executable statements ---------------------------------------------------
    !
    retval = .false.
    len1   = len_trim(string1)
    len2   = len_trim(string2)
    !
    ! determine comparison length
    !
    if (present(lenreq)) then
       lencmp = lenreq
    else
       lencmp = max(len1,len2)
    endif
    !
    ! do a quick check on string length
    !
    if (len1<lencmp .or. len2<lencmp) then
       !
       ! at least one string is shorter than the comparison length
       ! they can only be equal if their length is equal
       !
       if (len1 /= len2) then
          retval = .false.
          return
       else
          !
          ! strings have equal length, but are shorter than comparison length
          ! we only have to check the strings for their actual length
          !
          lencmp = len1
       endif
    endif
    !
    ! local copy of the strings needed to switch case without changing the
    ! original version.
    !
    allocate (locstr(2))
    !
    ! strings will be compared upto lencmp
    !
    locstr(1) = string1(1:lencmp)
    call str_lower(locstr(1), lencmp)
    !
    locstr(2) = string2(1:lencmp)
    call str_lower(locstr(2), lencmp)
    !
    if (locstr(1)(1:lencmp) == locstr(2)(1:lencmp)) then
       !
       ! strings are equal upto lencmp
       !
       retval = .true.
    else
       !
       ! strings are not equal
       !
       retval = .false.
    endif
    deallocate (locstr)
end function strcmpi


end module string_module
