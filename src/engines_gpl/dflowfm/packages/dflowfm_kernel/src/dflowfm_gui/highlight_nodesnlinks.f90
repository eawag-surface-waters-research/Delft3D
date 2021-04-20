!> Highlights net/flow nodes and/or links, when specified in display parameters.
subroutine highlight_nodesnlinks()
    use unstruc_display
    use unstruc_colors
    use network_data
    use m_flowgeom
    implicit none

    integer :: L

    ! if (jaHighlight /= 1) return

    if (nhlNetNode > 0 .and. nhlNetNode <= numk) then
        call cirr(xk(nhlNetNode), yk(nhlNetNode), ncolhl)
    end if

    if (nhlNetLink > 0 .and. nhlNetLink <= numl) then
        call cirr(.5d0*(xk(kn(1,nhlNetLink))+xk(kn(2,nhlNetLink))), &
                  .5d0*(yk(kn(1,nhlNetLink))+yk(kn(2,nhlNetLink))), ncolhl)
        call teklink(nhlNetLink, ncolhl)
    end if

    if (nhlFlowNode > 0 .and. nhlFlowNode <= ndx) then
       call cirr(xz(nhlFlowNode), yz(nhlFlowNode), ncolhl)
    end if

    if (nhlFlowLink > 0 .and. nhlFlowLink <= lnx) then
        call cirr(xu(nhlFlowLink), yu(nhlFlowLink), ncolhl)
    end if

end subroutine highlight_nodesnlinks
