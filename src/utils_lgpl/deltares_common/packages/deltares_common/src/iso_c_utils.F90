module iso_c_utils
  use iso_c_binding
  implicit none

  integer(c_int), parameter :: MAXSTRINGLEN = 1024
contains
  ! Utility functions, move these to interop module
  ! Make functions pure so they can be used as input arguments.
  integer(c_int) pure function strlen(char_array)
    character(c_char), intent(in) :: char_array(MAXSTRINGLEN)
    integer :: inull, i
    strlen = 0
    do i = 1, size(char_array)
       if (char_array(i) .eq. C_NULL_CHAR) then
          strlen = i-1
          exit
       end if
    end do
  end function strlen

  pure function char_array_to_string(char_array, length)
    integer(c_int), intent(in) :: length
    character(c_char),intent(in) :: char_array(length)
    character(len=length) :: char_array_to_string
    integer :: i
    do i = 1, length
       char_array_to_string(i:i) = char_array(i)
    enddo
  end function char_array_to_string

  pure function string_to_char_array(string, length)
    character(len=length), intent(in) :: string
    integer(c_int),intent(in) :: length
    character(kind=c_char,len=1) :: string_to_char_array(length+1)
    integer :: i
    do i = 1, length
       string_to_char_array(i) = string(i:i)
    enddo
    string_to_char_array(length+1) = C_NULL_CHAR
  end function string_to_char_array
end module iso_c_utils
