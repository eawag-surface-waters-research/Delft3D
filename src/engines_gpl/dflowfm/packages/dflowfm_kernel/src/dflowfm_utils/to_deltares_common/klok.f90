!> wall clock timer
   subroutine klok(t)
   use unstruc_messages
   implicit none

   double precision   :: t
   character(len=8)   :: date
   character(len=10)  :: time
   character(len=5)   :: zone
   integer            :: timing(8)

   character(len=128) :: mesg

   integer,          save :: ndays=0
   integer,          save :: dayprev=-999

   call date_and_time(date, time, zone, timing)

!  check for new day
   if ( dayprev.eq.-999 ) then
      dayprev = timing(3)    ! initialization to
   else if ( timing(3).ne.dayprev ) then
      ndays = ndays+1
      write(mesg, "('new wall clock day: previous day=', I2, ', new day=', I2)") dayprev, timing(3)
      call mess(LEVEL_INFO, trim(mesg))
      dayprev = timing(3)
   end if

   t = ndays*3600d0*24d0 + timing(5)*3600d0 + timing(6)*60d0 + timing(7) + dble(timing(8))/1000d0


   end subroutine klok
