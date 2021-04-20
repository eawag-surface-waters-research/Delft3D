subroutine timdat(julday, timsec, idatum, itijd)
!!--description-----------------------------------------------------------------
!
!    Function:  returns date and time according actual time
!               in minutes and itdate (julian day)
!               in the form yyyymmdd  hhmmss
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
!
! Global variables
!
    integer               :: idatum !  Absolute date related to ITDATE and TIMSEC
    integer , intent(out) :: itijd  !  Absolute time related to ITDATE and TIMSEC
    integer , intent(in)  :: julday !  Description and declaration in inttim.igs
    real(fp), intent(in)  :: timsec !  Description and declaration in inttim.igs
!
! Local variables
!
    integer(long) :: icurtm
    integer       :: iday
    integer       :: ihou
    integer       :: imin
    integer       :: imo
    integer       :: isec
    integer       :: iy
    integer       :: l
    integer       :: n
!
!! executable statements -------------------------------------------------------
!
    ! Define number of days, hours, minutes and seconds from TIMSEC
    !
    icurtm = nint(timsec,long)
    iday   = int(icurtm/86400_long)
    icurtm = icurtm - int(iday,long)*86400_long
    ihou   = int(icurtm)/3600
    icurtm = icurtm - int(ihou,long)*3600_long
    imin   = int(icurtm)/60
    icurtm = icurtm - int(imin,long)*60_long
    isec   = int(icurtm)
    !
    ! Convert julian day-number
    !
    l    = julday + iday + 68569
    n    = 4*l/146097
    l    = l - (146097*n + 3)/4
    iy   = 4000*(l + 1)/1461001
    l    = l - 1461*iy/4 + 31
    imo  = 80*l/2447
    iday = l - 2447*imo/80
    l    = imo/11
    imo  = imo + 2 - 12*l
    iy   = 100*(n - 49) + iy + l
    !
    ! Define integer values for time and date
    !
    itijd  = ihou*10000 + imin*100 + isec
    idatum = abs(iy)*10000 + imo*100 + iday
    idatum = sign(idatum, iy)
end subroutine timdat
