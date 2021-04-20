 !> A complete single computational time step (init-perform-finalize).
 subroutine flow_single_timestep(key, iresult)                ! do only 1 flow timestep
 use m_flow
 use m_flowgeom
 use m_flowtimes
 use unstruc_model, only : jawritebalancefile
 use unstruc_netcdf
 use m_xbeach_netcdf
 use m_timer
 use unstruc_display, only : jaGUI
 use dfm_error
 implicit none

 integer :: key
 integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

 integer :: N, L

 iresult = DFM_GENERICERROR

! double precision :: t
! call checkspeed(t)

   call flow_init_single_timestep(iresult)
   if (iresult /= DFM_NOERR) then
      goto 888
   end if

   call flow_run_single_timestep(key, iresult)
   if (iresult /= DFM_NOERR .and. iresult /= DFM_TIMESETBACK) then
      goto 888
   end if

   call flow_finalize_single_timestep(iresult)
   if (iresult /= DFM_NOERR) then
      goto 888
   end if

   ! JRE avoid annoying dt_user interference
    call xbeach_write_stats(time1)
    call sedmor_write_stats(time1)
   iresult = DFM_NOERR
   return ! Return with success

888 continue
   ! Error
end subroutine flow_single_timestep
