!> A complete single user time step (init-run-finalize).
 subroutine flow_usertimestep(key, iresult)                   ! do computational flowsteps until timeuser
 use m_flowtimes
 use timers
 use unstruc_messages
 use m_partitioninfo
 use unstruc_display, only: jaGUI
 use m_timer
 use dfm_error
 implicit none
   integer, intent(out) :: key     !< Key number if any key was pressed in GUI.
 integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

   call timstrt('User time loop', handle_user)

   iresult = DFM_GENERICERROR
   key = 0

   call flow_init_usertimestep(iresult)
   if (iresult /= DFM_NOERR) then
      goto 888
   end if

   call flow_run_usertimestep(key, iresult)
   if (iresult /= DFM_NOERR) then
      goto 888
   end if

   call flow_finalize_usertimestep(iresult)
   if (iresult /= DFM_NOERR) then
      goto 888
   end if

   call timstop(handle_user)

   iresult = DFM_NOERR
   return ! Return with success.

888 continue
end subroutine flow_usertimestep
