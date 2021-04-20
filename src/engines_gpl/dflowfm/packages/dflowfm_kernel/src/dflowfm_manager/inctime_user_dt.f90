!> Increase the time_user with a delta t
!! Called from API.
 subroutine inctime_user_dt(dt)
 use m_flowtimes
 implicit none
 double precision, intent(in) :: dt !< increase time_user with delta t (dt)
 ! If not, current time_user was not yet reached (user interrupt in interface)
 time_user = time_user + dt                       !
 ! time_user = max(time_user, time1)              ! safety for now only, until sobektimestepping is introduced
 dnt_user  = dnt_user  + 1                        ! todo from, to
end subroutine inctime_user_dt
