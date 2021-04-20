 subroutine flow_setstarttime()                      ! set flow starttime
 use m_flowtimes
 implicit none

 time_user = tstart_user + dt_user
 time0     = tstart_user
 time1     = tstart_user
 dts       = dt_init
 dtprev    = dts
 dnt       = 0
 dnt_user  = 1
 time_split0 = tstart_user
 time_split  = tstart_user
 end subroutine
