!> Given time in seconds from refdat, fill dateandtime string
 !! NOTE: maketime and maketimeinverse are not compatible, because of minutes versus seconds, and different format string.
 subroutine maketime(dateandtime,tim)
 use m_flowtimes
 implicit none

 character,        intent(out) :: dateandtime*(*) !< Output datetime string, format '20000101_000000', note: includes seconds.
 double precision, intent(in)  :: tim             !< Input time in seconds since refdat.

 integer          :: iday, imonth, iyear, ihour, imin, isec

 dateandtime = '20000101_000000'
 ! TODO: AvD: maketime and maketimeinverse are now inconsistent since the addition of this '_'

 call datetime_from_refdat(tim, iyear, imonth, iday, ihour, imin, isec)

 write(dateandtime( 1:4 ),'(i4)')   iyear
 write(dateandtime( 5:6 ),'(i2.2)') imonth
 write(dateandtime( 7:8 ),'(i2.2)') iday
 write(dateandtime(10:11),'(i2.2)') ihour
 write(dateandtime(12:13),'(i2.2)') imin
 write(dateandtime(14:15),'(i2.2)') isec

 return
 end subroutine maketime
