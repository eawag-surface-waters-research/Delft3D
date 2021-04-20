subroutine check_einstein_garcia(aref,h,z0,rs,ein)
implicit none
double precision :: aref,h,z0,rs,ein, ucrouse, z, dz
integer :: num, k

ein = 0d0
z   = aref
num = 10000
dz  = (h - z) / dble(num)
z   = z - 0.5d0*dz
do k = 1,num
   z = z + dz
   ucrouse = log(z/z0) * ( (aref/(h-aref))*( (h-z)/z) )** rs
   ein = ein + ucrouse*dz
enddo
end subroutine check_einstein_garcia
