      subroutine dateandtimenow(iyear, month, iday, ihour, minute, isecnd)
      implicit none
      integer,            intent(out)              :: iyear, month, iday, ihour, minute, isecnd
!     integer,            intent(out), optional    :: imsec
!     character(len=5),   intent(out), optional    :: zone

      character(len=8 ) ::       dat
      character(len=10) ::       tim
      character(len=5)  ::       zone
      integer           ::       imsec
      integer           ::       values(8)

      call date_and_time(dat,tim,zone,values)
      iyear  = values(1)
      month  = values(2)
      iday   = values(3)
      ihour  = values(5)
      minute = values(6)
      isecnd = values(7)
      imsec  = values(8)
      end subroutine dateandtimenow
