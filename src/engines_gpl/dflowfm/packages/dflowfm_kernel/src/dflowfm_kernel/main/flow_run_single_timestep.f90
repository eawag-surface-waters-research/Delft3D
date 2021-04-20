!> Performs a single computational timestep, but not the init and finalize of the timestep.
subroutine flow_run_single_timestep(key, iresult)                ! do only 1 flow timestep
use m_flow
use timers
use m_flowgeom
use m_flowtimes
use unstruc_model, only : jawritebalancefile
use unstruc_netcdf
use m_timer
use unstruc_display, only : jaGUI
use dfm_error
implicit none

integer :: key
integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful. DFM_TIMESETBACK if succesful, but with timestep setbacks.

integer :: N, L

 iresult = DFM_GENERICERROR

 if (itstep >= 2) then
    call timstrt('step_reduce', handle_extra(51)) ! step_reduce
    call step_reduce(key)                            ! set a computational timestep implicit, reduce, elim conj grad substi
    call timstop(handle_extra(51)) ! step_reduce

    if (dsetb > 0) then
       iresult = DFM_TIMESETBACK ! Warning about setbacks, but don't return directly, continue function normally
    end if
 else
    call velocities_explicit()                       ! progress without pressure coupling
    call transport()                                 ! progress without pressure coupling
    call update_part()
    time1  = time0 + dts                             ! progress without pressure coupling
 endif

 if (jaeverydt > 0) then
    if ((comparereal(time1, ti_maps, eps10) >= 0) .and. (comparereal(time1, ti_mape, eps10) <= 0)) then
       call wrimap(time1)
    end if
 end if
   ! Finalize timestep code used to be here, now flow_finalize_single_timestep()

   if (iresult /= DFM_TIMESETBACK) then
      iresult = DFM_NOERR
   end if

   return ! Return with success

888 continue
   ! Error

end subroutine flow_run_single_timestep
