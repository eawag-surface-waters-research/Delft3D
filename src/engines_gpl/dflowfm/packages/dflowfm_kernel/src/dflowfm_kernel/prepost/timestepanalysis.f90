 subroutine timestepanalysis(dtsc_loc)
    use m_flow
    use m_flowtimes
    use m_partitioninfo
    use unstruc_model, only: md_ident
    implicit none

    double precision, intent(in) :: dtsc_loc

    integer,          save       :: mout = 0

!   check if local maximum time step is also global maximum time step
    if ( jampi.eq.1 ) then
       if ( dtsc_loc.gt.dtsc ) then
          kkcflmx = 0
       end if
    end if

    if (kkcflmx > 0) then
       numlimdt(kkcflmx) = numlimdt(kkcflmx) + 1
    endif

    if (jatimestepanalysis == 1) then
       if (mout == 0) then
          call newfil(mout, trim(md_ident)//'.steps')
          write(mout, '(A)')  'time0/60, dts, dtsc, kkcflmx, kcflmx-kbot(kkcflmx)+1, vol1(kcflmx), squ2D(kcflmx), squ(kcflmx), sqi(kcflmx) '
       endif
       if (kkcflmx > 0) then
          if (kcflmx == 0) kcflmx = kkcflmx
          if (ja_timestep_auto == 3 .or. ja_timestep_auto == 4 ) then
             write(mout, '(3F14.4,2I8,4F14.4)')  time0/60d0, dts, dtsc, kkcflmx, kcflmx-kbot(kkcflmx)+1, vol1(kcflmx), squ2D(kkcflmx), squ(kcflmx), sqi(kcflmx)
          else
             write(mout, '(3F14.4,2I8,4F14.4)')  time0/60d0, dts, dtsc, kkcflmx, kcflmx-kbot(kkcflmx)+1, vol1(kcflmx), squ  (kcflmx), squ(kcflmx), sqi(kcflmx)
          endif
       else
          write(mout, '(3F14.4, I8)')         time0/60d0, dts, dtsc, kkcflmx
       endif
    endif

    return
 end subroutine timestepanalysis
