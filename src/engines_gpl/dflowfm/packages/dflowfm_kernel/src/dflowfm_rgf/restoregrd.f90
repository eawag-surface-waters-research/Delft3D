     subroutine restoregrd()
     use m_grid
     implicit none
     if (allocated (xch) ) then
        call increasegrid(mch,nch)
        xc = xch
        yc = ych
        zc = zch
        mc = mch
        nc = nch
     endif
     end subroutine restoregrd
