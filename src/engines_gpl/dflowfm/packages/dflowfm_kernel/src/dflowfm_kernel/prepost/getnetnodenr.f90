subroutine getnetnodenr(x,y,k)
use m_missing
use gridoperations
double precision :: x,y,z
integer          :: k

z = dmiss
CALL ISNODE(K, X, Y, z )
if (k == 0) then
   CALL DSETNEWPOINT(X,Y,K)
endif
end subroutine getnetnodenr
