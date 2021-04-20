!> compute total grid height for a given grow factor, first grid layer height and number of grid layers
double precision function comp_h(dgrow, dheight0, nfac)
   implicit none

   double precision, intent(in) :: dgrow     !< grow factor
   double precision, intent(in) :: dheight0  !< first grid layer height
   integer,          intent(in) :: nfac      !< number of grid layers

   if ( abs(dgrow-1d0).gt.1d-8 ) then
      comp_h = (dgrow**nfac-1d0) / (dgrow-1d0) * dheight0
   else
      comp_h = nfac * dheight0
   end if

   return
end function comp_h
