!> Finalizes the current user-timestep (monitoring and I/O).
!!
!! Should be called directly after a flow_run_usertimestep.
subroutine flow_finalize_usertimestep(iresult)
   use m_flowtimes
   use Timers
   use m_timer
   use m_flow
   use m_flowgeom
   use m_fourier_analysis
   use m_trachy
   use dfm_error
   use precision_basics, only : comparereal
   use unstruc_model, only: md_fou_step
   use m_partitioninfo, only: jampi
   implicit none

   integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

   double precision :: tem_dif
   logical, external :: flow_trachy_needs_update

   iresult = DFM_GENERICERROR

!   call fm_wq_processes_step(dt_user,time_user)
   if (ti_waqproc > 0) then
     if (comparereal(time_user, time_waqproc, eps10) == 0) then
         if ( jatimer == 1 ) call starttimer(IFMWAQ)
         call fm_wq_processes_step(ti_waqproc, time_user)
         if ( jatimer == 1 ) call stoptimer (IFMWAQ)
         tem_dif = (time_user - tstart_user)/ti_waqproc
         time_waqproc = tstart_user + (floor(tem_dif + 0.001d0)+1)*ti_waqproc
     endif
   endif

!   call mba_update(time_user)
   if (ti_mba > 0) then
     if (comparereal(time_user, time_mba, eps10) == 0) then
         call mba_update(time0)
         tem_dif = time_user/ti_mba
         tem_dif = (time_user - tstart_user)/ti_mba
         time_mba = min(tstart_user + (floor(tem_dif + 0.001d0)+1)*ti_mba, tstop_user)
     endif
   endif

   if (comparereal(time1, time_user, eps10)>=0)  then
      if (comparereal(time1, time_user, eps10) <=0) then
         time1 = time_user
         time0 = time1
      endif
      if ( jatimer == 1 ) call starttimer(IOUTPUT)

      call timstrt('Output', handle_extra(53)) ! output

!       only update values at the observation stations when necessary
!          alternative: move this to flow_externaloutput
      if (ti_his > 0) then
         if (comparereal(time1, time_his, eps10)>=0) then
            call updateValuesOnObservationStations()
            if (jampi == 1) then
               call updateValuesOnCrossSections_mpi(time1)
               call updateValuesOnRunupGauges_mpi()
               call reduce_particles()
            endif
            if (jahisbal > 0) then ! Update WaterBalances etc.
               call updateBalance()
            endif
            if ( jacheckmonitor == 1 ) then
!              compute "checkerboard" monitor
               call comp_checkmonitor()
            endif
         endif
      endif

!       in case of water level or discharge dependent roughness,
!       the observations and cross sections must be up todate each DtTrt s (if they are actually used)
      if (jatrt > 0) then
         if (flow_trachy_needs_update(time1)) then
            if (trachy_fl%gen%ntrtobs > 0) then
               call updateValuesOnObservationStations()
            endif
            if (trachy_fl%gen%ntrtcrs > 0) then
               if (jampi == 1) then
                  call updateValuesOnCrossSections_mpi(time1)
               endif
            endif
         endif
      endif

      call flow_externaloutput(time1)

      call timstop(handle_extra(53)) ! output
      if ( jatimer == 1 ) call stoptimer(IOUTPUT)

   endif

   if (fourierIsActive() .and. md_fou_step == 0) then
      if (fourierWithUc()) then
         call getucxucyeulmag(ndkx, workx, worky, ucmag, jaeulervel, 1)
      endif
      call postpr_fourier(time0, dt_user)
   endif

 iresult = DFM_NOERR
 return ! Return with success.

 end subroutine flow_finalize_usertimestep
