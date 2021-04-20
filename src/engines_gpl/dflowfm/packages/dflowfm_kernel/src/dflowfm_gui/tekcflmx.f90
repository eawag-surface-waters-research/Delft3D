 subroutine tekcflmx()
 use m_flowgeom
 use m_flow
 use m_flowtimes
 implicit none
 if (kkcflmx .ne. 0) then
    call setcol(31)
    call rcirc( xz(kkcflmx), yz(kkcflmx) )
    call HTEXT( dtsc, xz(kkcflmx), yz(kkcflmx) )
 endif
 end subroutine tekcflmx
