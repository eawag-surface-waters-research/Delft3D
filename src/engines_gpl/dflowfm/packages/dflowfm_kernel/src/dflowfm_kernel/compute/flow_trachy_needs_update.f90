!> helper function to make sure that the check for updating cross sections is in line with the flow_trachyupdate
logical function flow_trachy_needs_update(time1)
   use precision_basics, only : hp
   use m_flowtimes,      only : tstart_user, dt_user
   use m_trachy,         only : itimtt

   real(kind=hp), intent(in) :: time1  !< current time

   real(kind=hp) :: ntrtsteps, dt_trachy

   dt_trachy = dt_user * real(itimtt, hp)
   ntrtsteps = (time1 - tstart_user) / dt_trachy
   flow_trachy_needs_update = (abs(ntrtsteps - floor(ntrtsteps)) < 1d-6 )
end function flow_trachy_needs_update
