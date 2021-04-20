integer function get_isub(j, nfac1, j_loc)   !< gets the subinterval of grid layer j
   use m_spline2curvi

   implicit none

   integer,                     intent(in)  :: j       !< grid layer index
   integer, dimension(Nsubmax), intent(in)  :: nfac1   !< subinterval lengths
   integer,                     intent(out) :: j_loc   !< grid layer in the subinterval

   integer                                  :: isum, isub

   j_loc = j-1
   if ( j.gt.sum(nfac1) ) then
      isub = 0
      goto 1234
   end if

   isub = 1
   isum = 1+nfac1(isub)
   do while ( isum.le.j .and. isub.lt.Nsubmax )
      isub = isub + 1
      isum = isum + nfac1(isub)
   end do

   j_loc= j-isum+nfac1(isub)

!  error handling
1234 continue

   get_isub = isub

   return
end function
