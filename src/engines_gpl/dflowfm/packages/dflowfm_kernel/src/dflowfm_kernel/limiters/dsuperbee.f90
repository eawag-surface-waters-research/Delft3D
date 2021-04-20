double precision function dsuperbee(ds1, ds2)
   implicit none
   double precision     :: ds1, ds2, r

   if (ds1*ds2>0d0) then
      r = ds1/ds2
      dsuperbee = max(0d0, min(2d0*r,1d0),min(r,2d0))
      dsuperbee = dsuperbee*ds2
   else
      dsuperbee = 0d0
   end if
end function
