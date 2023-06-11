!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

!> Finalizes the current user-timestep (monitoring and I/O).
!!
!! Should be called directly after a flow_run_usertimestep.
subroutine flow_finalize_usertimestep(iresult)
   use m_flowtimes
   use Timers
   use m_timer
   use m_flow
   use m_flowgeom
   use m_trachy
   use dfm_error
   use precision_basics, only : comparereal
   use unstruc_model, only: md_fou_step
   use m_partitioninfo, only: jampi
   use unstruc_channel_flow, only : network
   use m_oned_functions, only: updateFreeboard, updateDepthOnGround, updateVolOnGround
   use m_update_fourier, only : update_fourier
   implicit none

   integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

   double precision :: tem_dif, ti_fou
   logical          :: do_fourier
   logical, external :: flow_trachy_needs_update

   iresult = DFM_GENERICERROR

   do_fourier = (md_fou_step == 0)

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
      call timstrt('update HIS data DtUser', handle_extra(75))
      if (ti_his > 0) then
         if (comparereal(time1, time_his, eps10)>=0) then
            do_fourier = do_fourier .or. (md_fou_step == 2)
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
      call timstop(handle_extra(75))

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

      call timstrt('call flow_externaloutput', handle_extra(79))
      call flow_externaloutput(time1)
      call timstop(handle_extra(79))

      call timstop(handle_extra(53)) ! output
      if ( jatimer == 1 ) call stoptimer(IOUTPUT)

   endif

   if (do_fourier) then
      call update_fourier(merge(dt_user, ti_his, md_fou_step == 0))
   endif

   iresult = DFM_NOERR
   return ! Return with success.

end subroutine flow_finalize_usertimestep
