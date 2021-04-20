!> compute the number of grid layers for a given grow factor, first grid layer height and total grid height
integer function comp_nfac(h_h0, dgrow)
   implicit none

   double precision, intent(in) :: h_h0      !< ratio of first grid layer height w.r.t. total grid height, i.e. h/h0
   double precision, intent(in) :: dgrow     !< grow factor

   if ( abs(dgrow-1d0).gt.1d-8 ) then
!      comp_nfac = floor(0.999d0+ log( (dgrow-1d0)*h_h0 + 1d0 ) / log(dgrow) )
      comp_nfac = floor(log( (dgrow-1d0)*h_h0 + 1d0 ) / log(dgrow) )
   else
      comp_nfac = floor(0.999d0+ h_h0 )
   end if

   return
end function comp_nfac
