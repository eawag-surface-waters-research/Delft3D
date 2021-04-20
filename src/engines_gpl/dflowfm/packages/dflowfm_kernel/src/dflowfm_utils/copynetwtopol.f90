subroutine copynetwtopol( )
use m_polygon
use m_missing
use network_data
implicit none
integer :: n, L, k1, k2

call increasepol(3*numl+1000, 0)

n = 0
do L = 1,numL
   n = n + 1 ; k1 = kn(1,L) ; xpl(n) = xk(k1) ; ypl(n) = yk(k1) ; zpl(n) = zk(k1)
   n = n + 1 ; k2 = kn(2,L) ; xpl(n) = xk(k2) ; ypl(n) = yk(k2) ; zpl(n) = zk(k2)
   n = n + 1 ; k2 = kn(2,L) ; xpl(n) = dmiss  ; ypl(n) = dmiss  ; zpl(n) = dmiss
enddo
npl = n

   end subroutine copynetwtopol
