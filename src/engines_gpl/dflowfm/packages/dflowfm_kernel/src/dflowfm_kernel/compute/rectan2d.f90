subroutine rectan2D(hpr, br, hr, area, width, japerim, perim)
use m_flow, only : slotw1D
implicit none
integer          :: japerim
double precision :: hpr                  ! hoogte   in profiel
double precision :: br                   ! breedte van profiel
double precision :: hr                   ! hoogte  van profiel
double precision :: area                 ! wet cross sectional area
double precision :: width                ! width at water surface
double precision :: perim, hp            ! wet perimeter
if (japerim == 1) then
   hp = min(hpr, hr)
else
   hp = hpr
endif
area  = hp*br
width = br
perim = br
if (slotw1D > 0 .and. japerim == 0) then
   width = width + slotw1D
   area  = area  + slotw1D*hpr
endif
end subroutine rectan2D
