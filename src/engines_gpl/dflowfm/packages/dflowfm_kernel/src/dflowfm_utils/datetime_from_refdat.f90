 !> Calculate absolute date time values, given a time in seconds since refdat.
 !! \see maketime
 subroutine datetime_from_refdat(timsec, iyear, imonth, iday, ihour, imin, isec)
 use m_flowtimes
 implicit none
 double precision, intent(in)  :: timsec !< Time in seconds since refdate
 integer,          intent(out) :: iyear, imonth, iday, ihour, imin, isec !< Actual date, split up in year/month, etc.

 integer :: jul, jul0, iyear0, imonth0, iday0
 double precision :: tnr, tsec
 integer :: ndag

 integer, external :: julday

 read(refdat(1:4),*) iyear0
 read(refdat(5:6),*) imonth0
 read(refdat(7:8),*) iday0

 jul0  = julday(imonth0,iday0,iyear0)
 tnr   = timsec / 3600d0
 ndag  = tnr / 24d0

 call caldat(jul0+ndag,imonth,iday,iyear)

 tsec  =  timsec - ndag*24d0*3600d0
 ihour =   tsec/3600d0
 imin  =  (tsec - ihour*3600d0)/60d0
 isec  =  (tsec - ihour*3600d0 - imin*60d0)

 end subroutine datetime_from_refdat
