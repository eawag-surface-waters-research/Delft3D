 subroutine setdtmaxavalan(dts)
 use m_fm_erosed, only: duneavalan, avaltime
 implicit none

 double precision,    intent(inout) :: dts   !timestep to use

 if (duneavalan) then
    if (dts > avaltime / 2d0 - 1d0 ) then
       dts = avaltime / 2d0 - 1d0 !make sure timestep is smaller than half the avaltime (e.g. with default avaltime 9 seconds)
    end if
 end if

 end subroutine setdtmaxavalan
