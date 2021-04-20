double precision function rho_Eckart(sal, temp)
! use m_physcoef

implicit none
double precision :: sal, temp
double precision :: cp1, clam1, temp2, dum
double precision :: cp0, clam0, clam, alph0

temp2      = temp*temp
cp0        = 5890.0d0 + 38.00d0*temp - 0.3750d0*temp2
clam       = 1779.5d0 + 11.25d0*temp - 0.0745d0*temp2
clam0      =    3.8d0 +  0.01d0*temp
cp1        = cp0  + 3.0d0*saL
clam1      = clam - clam0*saL
rho_Eckart = 1000.0d0*cp1/(0.698d0*cp1+clam1)  ! alph0

! rho_Eckart = 1000.0d0* ( cp1/(alph0*cp1+clam1) - 1d0)

! rho_Eckart = abs( 0.5d0*(zws(k)+zws(k-1)) )

! rho_Eckart = 0.7d0*saL + 1000d0

end function rho_Eckart
