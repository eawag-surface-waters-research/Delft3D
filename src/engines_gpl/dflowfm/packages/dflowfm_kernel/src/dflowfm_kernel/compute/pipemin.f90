subroutine pipemin(hpr, dia, area, width) ! top minus part
use m_sferic
use m_flow, only : slotw1D
implicit none
double precision, intent(in)   :: dia, hpr
double precision, intent(out)  :: area, width

double precision               :: are, dacos, dsqrt, fi, r, sq

r = 0.5*dia
if (hpr< r) then
   area  = 0d0
   width = 0d0
else if (hpr < dia) then
   are   = hpr - r
   fi    = dasin(are/r)
   sq    = dsqrt(hpr*(dia - hpr))
   area  = are*dia - fi*r*r - sq*are
   width = dia - 2*sq
else
   area  = (hpr-r)*dia - 0.5d0*pi*r*r
   width = dia
endif
end subroutine pipemin
