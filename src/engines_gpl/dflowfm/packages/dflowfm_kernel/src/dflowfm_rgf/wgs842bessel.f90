!> convert from WGS84 to Bessel
subroutine wgs842bessel(phiwgs, lamwgs, phibes, lambes)
   implicit none

   double precision, intent(in)  :: phiwgs, lamwgs
   double precision, intent(out) :: phibes, lambes

   double precision, dimension(2), parameter :: A1 = (/  1.00011715371927d+00,  -3.29086810736171d-06  /)
   double precision, dimension(2), parameter :: A2 = (/  1.25032982802497d-06,   1.00014669151113d+00  /)
   double precision, dimension(2), parameter :: b  = (/ -5.12951110000526d-03,  -1.83260002653070d-04  /)

   phibes = A1(1)*phiwgs + A2(1)*lamwgs + b(1)
   lambes = A1(2)*phiwgs + A2(2)*lamwgs + b(2)

end subroutine wgs842bessel
