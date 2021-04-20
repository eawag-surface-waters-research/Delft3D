!< Returns true when a 1d node can be used as a boundary. By default
!< the connecting edge should not lead to a bifurcation, unless
!< the flag jaAllowBndAtBifurcation is true
pure logical function is_1d_boundary_candidate(L,i)
    use network_data
    use m_flowgeom

    implicit none

    integer, intent(in)     :: L    !<  net link to check for boundary candidate
    integer, intent(in)     :: i    !<  node to check, equals 1 or 2

    logical                 :: isEndNode

    is_1d_boundary_candidate = nmk(kn(i,L)) == 1 .and. nmk(kn(3-i,L)) == 2 .and. lne(i,L) < 0
    if (jaAllowBndAtBifurcation == 1) then
        is_1d_boundary_candidate = nmk(kn(i,L)) == 1 .and. nmk(kn(3-i,L)) >= 2 .and. lne(i,L) < 0
    endif

    return
end function is_1d_boundary_candidate
