subroutine setiadvpure1D() ! set 103 on default 1D links if pure1D
use m_flowgeom
use m_flow
use m_netw, only: kc
implicit none
integer :: L, n1, n2

kc = 0
do L = 1,lnx
   n1 = ln(1,L); n2 = ln(2,L)
   if (iabs(kcu(L)) == 1) then
      kc(n1)  = kc(n1) + 1
      kc(n2)  = kc(n2) + 1
   endif
enddo

do L = 1,lnx1D
   n1 = ln(1,L); n2 = ln(2,L)
   if (iadv(L) == iadvec1D .or. iadv(L) == 6 .and. kc(n1) == 2 .and. kc(n2) == 2) then
      iadv(L) = 103  ! 103 = qucper (iadv=3)  + pure1D
   endif
enddo

do L  = lnxi+1, lnx
   n2 = ln(2,L)
   if (iabs(kcu(L)) == 1 .and. kc(n2) ==2 ) then
      iadv(L) = 103
   endif
enddo

end subroutine setiadvpure1D
