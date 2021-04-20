!> returns the index of a named lateral in the global array from this module
subroutine getLateralIndex(idlat, index)
   use m_wind

   implicit none
   character(len=*), intent(in)  :: idlat !< id of the lateral
   integer,          intent(out) :: index !< its position in the global array
   integer                       :: i

   index = 0

   i = -1
   do i = 1, numlatsg
      if (trim(lat_ids(i)) == trim(idlat)) then
         index = i
         exit
      end if
   end do

end subroutine getLateralIndex
