!> get the cross splines that have valid grid height
subroutine get_index(ncs, isvalid, ndx, idx)
   implicit none

   integer,                 intent(in)  :: ncs  !< number of cross splines
   integer, dimension(ncs), intent(in)  :: isvalid !< valid (>=0) or not (<0)
   integer,                 intent(out) :: ndx  !< number of valid cross splines
   integer, dimension(ncs), intent(out) :: idx  !< valid cross splines

   integer                              :: i

   ndx = 0
   do i=1,ncs
      if ( isvalid(i).ge.0 ) then
         ndx = ndx+1
         idx(ndx) = i
      end if
   end do

   return
end subroutine
