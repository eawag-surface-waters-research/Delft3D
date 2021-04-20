 subroutine setwindstress()
 use m_flowgeom
 use m_flow
 implicit none
 double precision :: uwi, cdw, tuwi, roro, wxL, wyL, uL, vL, uxL, uyL
 integer          :: L, numwav, k   ! windstuff

 windxav = 0d0
 windyav = 0d0

 if (jawindstressgiven == 1) then
    do L = 1, lnx
       wdsu(L) = ( wx(L)*csu(L) + wy(L)*snu(L) ) / rhomean
    enddo
    if (jamapwindstress > 0) then
       do L = 1, lnx
          wdsu_x(L) = wx(L) / rhomean
          wdsu_y(L) = wy(L) / rhomean
       enddo
    endif
    if (jatem == 5) then
       do L = 1, lnx
          cdwcof(L) = wdsu(L)
       enddo
    endif
 else
    roro = rhoair/rhomean
    wdsu   = 0d0
    numwav = 0
    do L = 1, lnx
       if ( wx(L) /= 0d0 .or. wy(L) /= 0d0 ) then ! only if some wind

          wxL = wx(L)
          wyL = wy(L)
          if (jarelativewind == 1) then
             uL  = U1(Ltop(L))
             vL  =  v(Ltop(L))
             uxL = uL*csu(L) - vL*snu(L)
             uyL = uL*snu(L) + vL*csu(L)
             wxL = wxL - uxL
             wyL = wyL - uyL
          endif
          uwi    = sqrt( wxL*wxL + wyL*wyL )
          if (jaspacevarcharn == 1) then
             cdb(1) = wcharnock(L)
          endif
          call setcdwcoefficient(uwi,cdw,L)
          if (jatem == 5) then
             cdwcof(L) = cdw
          endif
          if (jaroro > 0) then
             k = ln(2,L)
             if (jaroro == 1) then
                roro = rhoair   / rho(ktop(k))
             else
                roro = roair(k) / rho(ktop(k))
             endif
          endif
          tuwi    = roro*cdw*uwi
          if (kmx > 0) then
              ustw(L) = sqrt(roro*cdw)*uwi
          endif
          wdsu(L) = tuwi*( wxL*csu(L) + wyL*snu(L) )
          windxav = windxav + wxL
          windyav = windyav + wyL
          numwav  = numwav  + 1
          if (jamapwindstress > 0) then
             wdsu_x(L) = tuwi*wxL
             wdsu_y(L) = tuwi*wyL
          endif
       endif
    enddo
    if (numwav > 0) then
       windxav = windxav/numwav
       windyav = windyav/numwav
    endif
 endif
 end subroutine setwindstress
