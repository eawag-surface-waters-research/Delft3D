!> check if a connection from node1 to node2 exists
subroutine checkgridline(node1, node2, lconflict)

 use m_netw
 use m_grid
 use m_missing

 implicit none

 integer, intent(in ) :: node1, node2               !< nodes

 logical, intent(out) :: lconflict                  !< .false. if connected, .true. otherwise

 integer              :: ilink, link, othernode1

 logical              :: doit                       ! determines whether the link neighbors a quad (.true.) or not (.false.)

! integer, parameter   :: IMISS = -999999

 lconflict = .true.                                 ! .false. if the (i,j)-connection is a valid connection

 do ilink=1,nmk(node1)
    link       = nod(node1)%lin(ilink)
    othernode1 = kn(1,link) + kn(2,link) - node1

! select links adjacent to at least one quad only
    doit = .false.
    if ( lnn(link).gt.0 ) doit =           ( netcell(lne(1,link))%n .eq. 4 )
    if ( lnn(link).gt.1 ) doit = doit .or. ( netcell(lne(2,link))%n .eq. 4 )

    if (  doit ) then
       if ( othernode1 .eq. node2 ) then            ! valid connection
          lconflict = .false.
          return
       end if
    end if
 end do

end subroutine checkgridline
