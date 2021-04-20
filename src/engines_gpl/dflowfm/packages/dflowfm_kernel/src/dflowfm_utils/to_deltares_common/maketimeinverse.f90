 !> Given datetime string, compute time in seconds from refdat
 subroutine maketimeinverse(dateandtime,timsec,stat)
 use m_flowtimes
 implicit none

 character, intent(in)         :: dateandtime*(*) !< Input datetime string, format '201201010000', note that seconds are ignored.
 integer, intent(out)          :: stat

 double precision  :: timmin
 double precision, intent(out) :: timsec

 integer          :: iday ,imonth ,iyear ,ihour , imin, isec
 integer          :: iostat

 ! dateandtime = '20120101000000'

 stat = 0
 read(dateandtime( 1:4 ),'(i4)',err=666,iostat=iostat)   iyear
 read(dateandtime( 5:6 ),'(i2.2)',err=666,iostat=iostat) imonth
 read(dateandtime( 7:8 ),'(i2.2)',err=666,iostat=iostat) iday
 read(dateandtime( 9:10),'(i2.2)',err=666,iostat=iostat) ihour
 read(dateandtime(11:12),'(i2.2)',err=666,iostat=iostat) imin
 read(dateandtime(13:14),'(i2.2)',err=666,iostat=iostat) isec

666 if (iostat/=0) then
       stat=iostat
       return
    endif

 call seconds_since_refdat(iyear, imonth, iday, ihour, imin, isec, timsec)

 timmin  = timsec/60d0
 !timmin = (jul - jul0)*24d0*60d0      + ihour*60d0      + imin

 return
 end subroutine maketimeinverse
