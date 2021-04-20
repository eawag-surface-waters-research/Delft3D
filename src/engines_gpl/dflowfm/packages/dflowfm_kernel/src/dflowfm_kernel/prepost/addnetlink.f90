subroutine addnetlink(x1,y1,x2,y2,L)
use gridoperations ! or you can not call connectdbn
double precision :: x1,y1,x2,y2
integer          :: k1,k2,L
call getnetnodenr(x1,y1,k1)
call getnetnodenr(x2,y2,k2)
call CONNECTDBN(K1,K2,L)
end subroutine addnetlink
