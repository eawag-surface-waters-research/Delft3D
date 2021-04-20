 !> Calculates the relative time in seconds since refdat, given an absolute datetime.
 !! The input datetime is in separate year/month/../seconds values.
 !! \see maketimeinverse
 subroutine seconds_since_refdat(iyear, imonth, iday, ihour, imin, isec, timsec)
 use m_flowtimes
 implicit none
 integer,          intent(in)  :: iyear, iday, imonth, ihour, imin, isec !< Input absolute date time components
 double precision, intent(out) :: timsec !< Output seconds since refdate for the specified input datetime.

 integer :: jul, jul0, iyear0, imonth0, iday0
 integer, external :: julday

     read(refdat(1:4),*) iyear0
     read(refdat(5:6),*) imonth0
     read(refdat(7:8),*) iday0

     jul0 = julday(imonth0,iday0,iyear0)
     jul  = julday(imonth ,iday ,iyear )

     timsec = (jul - jul0)*24d0*3600d0      + ihour*3600d0      + imin*60d0 + isec
 end subroutine seconds_since_refdat
