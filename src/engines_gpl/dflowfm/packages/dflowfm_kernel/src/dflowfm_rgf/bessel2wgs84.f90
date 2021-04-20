!> convert from Bessel to WGS84
subroutine bessel2wgs84(phibes, lambes, phiwgs, lamwgs)
   implicit none

   double precision, intent(in)  :: phibes, lambes
   double precision, intent(out) :: phiwgs, lamwgs

   double precision, dimension(2), parameter :: A1 = (/  9.99882860000000d-01, 3.29000000000000d-06 /)
   double precision, dimension(2), parameter :: A2 = (/ -1.25000000000000d-06, 9.99853330000000d-01 /)
   double precision, dimension(2), parameter :: b  = (/  5.12891000000000d-03, 1.83250000000000d-04 /)

   phiwgs = A1(1)*phibes + A2(1)*lambes + b(1)
   lamwgs = A1(2)*phibes + A2(2)*lambes + b(2)

end subroutine bessel2wgs84
