 !> compute the length of a netlink
 double precision function linksize(L)
 use m_netw
 use geometry_module, only: dbdistance
 use m_missing, only: dmiss
 use m_sferic, only: jsferic, jasfer3D

 implicit none
 integer :: L, k1, k2

 k1 = kn(1,L) ;  k2 = kn(2,L)
 linksize = dbdistance ( xk(k1), yk(k1), xk(k1), yk(k2), jsferic, jasfer3D, dmiss)
 end function linksize
