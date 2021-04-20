!> find index of string in array of strings
integer function findname(N, snames, sname)
   implicit none

   integer,                        intent(in) :: N
   character(len=*), dimension(N), intent(in) :: snames
   character(len=*),               intent(in) :: sname

   integer :: i

   findname = 0

   do i=1,N
      if ( trim(sname).eq.trim(snames(i)) ) then
         findname = i
         return
      end if
   end do

   return
end function findname
