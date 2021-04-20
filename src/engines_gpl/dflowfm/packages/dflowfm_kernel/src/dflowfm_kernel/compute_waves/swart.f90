subroutine Swart(Tsig, uorbu, z00, fw, ustw2)
use m_flow,  only : rhomean

implicit none
double precision :: Tsig, uorbu, z00, fw, ustw2
double precision :: astar

if (uorbu == 0d0) then
   fw = 0d0 ; ustw2 = 0d0 ; return
endif

astar = Tsig*uorbu/z00
if (astar > 296.088d0)  then                       ! 30pipi
    fw = 0.00251d0*exp(14.1d0/(astar**0.19d0))     ! astar=Tuorb/z00
else
    fw = 0.3d0
endif

ustw2 = 0.5d0*fw*uorbu*uorbu

end subroutine Swart
