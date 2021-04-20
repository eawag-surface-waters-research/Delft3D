!> Reads a key=value entry from a property block and tries to interpret the value.
!! The (single!) property block should come from an already-parsed .ini file.
!! The string value is always returned, if found, and an attempt is also made to
!! parse it into a scalar double, or alternatively to check whether it is an existing file.
subroutine read_required_property(prop_ptr, key, strvalue, dblvalue, is_double, typeandid, success)
   use properties
   use unstruc_messages
   implicit none
   type(TREE_DATA), pointer        :: prop_ptr   !< Property tree as read from a single .ini block
   character(len=*), intent(in)    :: key        !< Property key that should be read.
   character(len=*), intent(inout) :: strvalue   !< Returned string value for requested property key.
   double precision, intent(inout) :: dblvalue   !< Returned scalar double value for requested property key, IF possible.
   logical,          intent(out)   :: is_double  !< Tells whether the found value could be parsed into a scalar double value.
   character(len=*), intent(in)    :: typeandid  !< String with type and name, to be used in warning message to be printed if property key not found. Example: "gate 'Maeslant'"
   logical,          intent(out)   :: success    !< Whether value was read successfully or not.

   double precision :: tmpvalue
   integer :: ierr

   success   = .false.
   is_double = .false.

   call prop_get(prop_ptr, '', trim(key), strvalue, success)
   if (.not. success .or. len_trim(strvalue) == 0) then
      write(msgbuf, '(a,a,a,a,a)') 'Required field ''', trim(key), ''' missing in ', trim(typeandid), '.'
      call warn_flush()
      goto 888
   else
      read(strvalue, *, iostat = ierr) tmpvalue
      if (ierr == 0) then
         dblvalue = tmpvalue
         is_double = .true.
      end if
   end if

   success = .true.
888 continue

end subroutine read_required_property
