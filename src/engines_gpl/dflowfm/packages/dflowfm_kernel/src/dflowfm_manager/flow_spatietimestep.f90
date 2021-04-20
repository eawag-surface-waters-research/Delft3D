 subroutine flow_spatietimestep()                 ! do 1 flowstep
 use m_flowtimes
 use m_flowgeom, only: ndx
 use m_flowexternalforcings, only: nbndz, zbndz
 use m_flowparameters, only: janudge

 implicit none
 integer :: key, ierr
 integer :: i
 integer, external :: flow_modelinit

 if (ndx == 0) then
     ierr = flow_modelinit()
 end if

 if (ndx == 0) return                                ! No valid flow network was initialized

 call inctime_user()
 if (time0 >= time_user) then
    Tstop_user = tstop_user + dt_user
    time_user  = time_user  + dt_user
 endif
                                                     ! ipv time0
 tim1fld = max(time_user,tim1fld)
 if ( janudge.eq.1 ) call setzcs()
 call flow_setexternalforcings(tim1fld ,.false., ierr)    ! set field oriented forcings. boundary oriented forcings are in

 ! call flow_externalinput(time_user)                  ! receive RTC signals etc

 call flow_single_timestep(key, ierr)

 call updateValuesOnObservationStations()

 call flow_externaloutput(time1)                     ! receive signals etc, write map, his etc
                                                     ! these two functions are explicit. therefore, they are in the usertimestep
 end subroutine flow_spatietimestep
