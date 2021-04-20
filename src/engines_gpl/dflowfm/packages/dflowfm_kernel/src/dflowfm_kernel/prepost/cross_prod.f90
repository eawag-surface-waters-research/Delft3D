!> cross product
double precision function cross_prod(a,b)
   implicit none
   double precision, dimension(2) :: a, b

   cross_prod = a(1)*b(2) - a(2)*b(1)

   return
end function cross_prod
