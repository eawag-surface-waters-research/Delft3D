!> Runs a user-timestep (do computational flowsteps until timeuser), but not the init and finalize.
!!
!! Should be preceded by a flow_run_usertimestep and followed by a flow_finalize_usertimestep.
subroutine flow_run_usertimestep(key, iresult)                   ! do computational flowsteps until timeuser
   use m_flowtimes
   use unstruc_messages
   use m_partitioninfo
   use unstruc_display, only: jaGUI
   use dfm_error
   implicit none
   integer, intent(out) :: key
   integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

   key = 0
   iresult = DFM_GENERICERROR

 do while (time0 < time_user)                        ! nb, outside flow_singletimestep, time0=time1 !

    call flow_single_timestep(key, iresult)
    if (iresult /= DFM_NOERR .and. iresult /= DFM_TIMESETBACK) then
       goto 888
    end if

    if ( jaGUI.eq.1 ) then ! TODO: MICHAL Another Gui call

       call get_s_key(key)

       if ( jampi.eq.1 ) then
          call reduce_key(key)
       end if

       if (key == 1 ) then
           call mess(LEVEL_INFO, 'User interrupt')
           iresult = DFM_NOERR
           return
       end if
    end if
   enddo

   iresult = DFM_NOERR
   return ! Return with success.

888 continue
end subroutine flow_run_usertimestep
