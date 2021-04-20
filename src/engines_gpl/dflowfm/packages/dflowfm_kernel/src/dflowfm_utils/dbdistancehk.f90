subroutine dbdistancehk(xa,ya,xb,yb,dist) ! easy way to get a distance in one line of code, without need for specifying 3 modules :
use m_sferic, only: jsferic, jasfer3D
use geometry_module, only: dbdistance
USE M_MISSING
double precision :: xa,ya,xb,yb,dist
dist = DBDISTANCE(xa,ya,xb,yb,jsferic, jasfer3D, dmiss)
end subroutine dbdistancehk
