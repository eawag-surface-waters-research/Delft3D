!> update observation station data
subroutine updateValuesOnObservationStations()
   use m_observations
   use m_partitioninfo
   use m_timer
   use m_flowtimes, only: time1
   implicit none

   ! This routine can now be called any time, but will only do the update
   ! of valobs when necessary:
   if (tlastupd_valobs == time1) then
      return
   end if
   tlastupd_valobs = time1

   call fill_valobs()

   if ( jampi.eq.1 ) then
      if ( jatimer.eq.1 ) call starttimer(IOUTPUTMPI)
      call reduce_valobs(IPNT_NUM,numobs+nummovobs,valobs,valobs_all)
      if ( jatimer.eq.1 ) call stoptimer(IOUTPUTMPI)
   end if

   return
end subroutine updateValuesOnObservationStations
