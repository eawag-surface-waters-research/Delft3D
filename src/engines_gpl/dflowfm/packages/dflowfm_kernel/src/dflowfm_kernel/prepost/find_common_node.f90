!> return common node of links L1 and L2
subroutine find_common_node(L1, L2, node)

 use m_netw
 use m_missing

 implicit none

 integer, intent(in)   :: L1, L2           !< links
 integer, intent(out)  :: node             !< common node

 integer, dimension(4) :: a                ! dummy array with nodes of L1 and L2
! integer, parameter    :: IMISS = -999999

 a(1:2)    = kn(1:2, L1)
 a(3:4)    = kn(1:2, L2)

 do
    node = IMISS

    if ( a(1).eq.a(3) .or. a(1).eq.a(4) ) node = a(1)
    if ( a(2).eq.a(3) .or. a(2).eq.a(4) ) node = a(2)

    if ( node.ne.IMISS ) exit

    write(6,*) 'find_common_node: no common node found'
    exit
 end do

end subroutine find_common_node
