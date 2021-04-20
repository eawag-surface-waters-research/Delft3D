!> Initializes a single computational timestep, call this prior to flow_perform_single_timestep.
subroutine flow_init_single_timestep(iresult)
use timers
use m_flow
use m_flowgeom
use m_flowtimes
use m_timer
use dfm_error
implicit none

integer :: key
integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

integer :: N, L

 iresult = DFM_GENERICERROR

 if (lnx == 0) then
    iresult = DFM_MODELNOTINITIALIZED
    goto 888
 end if

 call timstrt('Time steps', handle_steps)

 if ( jatimer.eq.1 ) call starttimer(ITIMESTEP)

 call flow_initimestep(0, iresult)                   ! initialise timestep

 if (iresult /= DFM_NOERR) then
    goto 888
 end if

   iresult = DFM_NOERR
   return ! Return with success

888 continue
   ! Error

end subroutine flow_init_single_timestep
