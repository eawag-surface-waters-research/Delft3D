!> find common value of two-dimensional arrays i1 and i2
!>   it is assumed there is at most one common value
integer function icommonval(i1, i2)
   implicit none

   integer, dimension(2), intent(in)  :: i1
   integer, dimension(2), intent(in)  :: i2

   icommonval = 0
   if ( i1(1).eq.i2(1) .or. i1(1).eq.i2(2) ) then
      icommonval = i1(1)
   else if ( i1(2).eq.i2(1) .or. i1(2).eq.i2(2) ) then
      icommonval = i1(2)
   end if

   return
end function icommonval
