subroutine pipe(hpr, dia, area, width, japerim, perim) ! half open part
use m_sferic
use m_flow, only : slotw1D
!
! this subroutine computes wetted circle surface as function
! of diameter d and waterdepth dpt, as an option (if jd=1) it can compute
! the derivative da(dpt)/ddpt and (if jw=1) it can also compute the wetted
! perimeter
!
!  dpt   I, water depth
!  dia   I, diameter
!  wet   O, wetted surface
!  dwdd  O, det/ddpt
!  wtp   O, wetted perimeter
!  jd    I, compute dwdd if jd=1
!  jw    I, compute wtp if jw=1
!  sl    I, slotbreedte
implicit none
integer, intent(in)            :: japerim
double precision, intent(in)   :: dia, hpr
double precision, intent(out)  :: area, width, perim

! Local variables

double precision               :: are, dacos, dsqrt, fi, r, sq

r    = 0.5*dia
are  = r - hpr
if (hpr< r) then
   fi = dacos(are/r)
   sq = dsqrt(hpr*(dia - hpr))
   area  = fi*r*r - sq*are
   width = 2*sq
   if (japerim == 1) perim = 2*fi*r
else
   area  = 0.5d0*pi*r*r+(hpr-r)*dia
   width = dia
   if (japerim == 1) then
       if (hpr < dia) then
          fi = dacos(are/r)
          sq = dsqrt(hpr*(dia - hpr))
          area  = fi*r*r - sq*are
          perim = 2*fi*r
       else
          area  = pi*r*r
          perim = twopi*r
       endif
   endif
endif
if (slotw1D > 0 .and. japerim == 0) then
   width = width + slotw1D
   area  = area  + slotw1D*hpr
endif
end subroutine pipe
