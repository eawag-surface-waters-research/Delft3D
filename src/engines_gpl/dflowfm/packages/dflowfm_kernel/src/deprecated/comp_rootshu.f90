subroutine comp_rootshu(Eup,aa,hu)
double precision :: Eup, aa, hu
double precision :: coeffs(5) !< coefficient vector (A,B,C,D,E)
double precision :: x(4)      !< roots
integer          :: i

coeffs(1) = 0d0
coeffs(2) = 1d0
coeffs(3) = -Eup
coeffs(4) = 0d0
coeffs(5) = aa

call comp_roots4(coeffs,x)

hu = 0d0
do i = 1,4
   hu = max(hu, x(i))
enddo

end subroutine comp_rootshu
