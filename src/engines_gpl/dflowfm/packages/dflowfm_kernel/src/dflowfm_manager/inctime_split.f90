!> Increment the time-splitting upcoming time, if input time argument lies on or beyond that time.
!! Each output file with name base_timesplit0_.. will contain output for time_split0 < time1 <= time_split.
!! (Only for time1=tstart_user, time_split0 <= time1, i.e. first map file in sequence will have one more snapshot.)
subroutine inctime_split(tim)
use m_flowtimes
use unstruc_messages
implicit none
double precision, intent(in) :: tim !< Current time, used to checked whether an increment is necessary at all.

integer :: iyear, imonth, iday, ihour, imin, isec, add_seconds

    ! Do nothing if time splitting is switched off
    if (ti_split <= 0d0) then
        return
    end if

    ! Do nothing if time is still before upcoming time_split.
    if (tim <= time_split) then
        return
    end if

    time_split0 = time_split

    do ! increment time_split until tim <= time_split
       ! First, get y/M/d/h/m/s values for current time_split since refdat:
       call datetime_from_refdat(time_split, iyear, imonth, iday, ihour, imin, isec)

       ! Second, add the ti_split increment to them, based on ti_split_unit
       add_seconds = 0
       select case (ti_split_unit)
       case ('Y')
           iyear = iyear + ti_split
       case ('M')
           imonth = imonth + ti_split
           if (imonth > 12) then
               iyear = iyear + floor(real(imonth)/12.0)
               imonth = mod(imonth, 12)
           end if
       case ('D')
           add_seconds = ti_split*24*3600
       case ('h')
           add_seconds = ti_split*3600
       case('m')
           add_seconds = ti_split*60
       case ('s')
           add_seconds = ti_split
       case default
           call mess(LEVEL_WARN, 'Invalid time partitioning unit: '//ti_split_unit) ! should not be possible, handled by readMDU
           return
       end select

       ! Finally convert the new absolute date time values to a time in seconds since refdat.
       call seconds_since_refdat(iyear, imonth, iday, ihour, imin, isec+add_seconds, time_split)

       if (tim <= time_split) then
          exit
       end if
    end do ! until tim <= time_split
end subroutine inctime_split
