subroutine inidensconstants()
use m_physcoef

implicit none
double precision :: temp

temp  = backgroundwatertemperature
cp0   = 5890.0d0 + 38.00d0*temp - 0.3750d0*temp*temp
clam  = 1779.5d0 + 11.25d0*temp - 0.0745d0*temp*temp
clam0 =    3.8d0 +  0.01d0*temp
end subroutine inidensconstants
