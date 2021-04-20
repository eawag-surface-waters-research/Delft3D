 subroutine dumpnetlink(tex,L)
 use m_netw
 use m_flowgeom
 use m_flow
 use unstruc_messages
 implicit none
 integer :: L,k1,k2
 character *(*) tex
 character(len = 14) tex2

 write(tex2,'(i14.0)') L
 k1 = kn(1,L) ; k2 = kn(2,L)
 call mess(LEVEL_INFO, trim(tex), trim(tex2), ' ')
 call mess(LEVEL_INFO, ' 2  2 ', ' ' , ' ')
 call mess(LEVEL_INFO, xk(k1), yk(k1))
 call mess(LEVEL_INFO, xk(k2), yk(k2))
 end subroutine dumpnetlink
