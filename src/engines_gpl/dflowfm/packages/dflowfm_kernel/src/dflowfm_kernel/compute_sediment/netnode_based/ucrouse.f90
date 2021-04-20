double precision function ucrouse(z,z0,h,a,rs)
use m_einstein_garcia
implicit none
double precision :: z, z0, h, a, rs

ucrouse = log(z/z0) * ( (a/(h-a))*( (h-z)/z) )** rs

end function
