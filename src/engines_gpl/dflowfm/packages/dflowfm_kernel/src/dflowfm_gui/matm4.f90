  SUBROUTINE MATM4(a,b,c)
  implicit none
  integer :: i
  integer :: j
  integer :: k
  ! matrix matrix
  double precision, dimension(4,4) :: a,b,c
  do 801 i = 1,4
     do 801 k = 1,4
        c(i,k) = 0d0
        do 801 j = 1,4
           c(i,k) = a(i,j) * b(j,k) + c(i,k)
  801 continue
  end SUBROUTINE MATM4
