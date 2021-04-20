!> Initializes a new user-timestep (advances user time, sets new meteo forcing)
!!
!! Should be followed by a flow_run_usertimestep and a flow_finalize_usertimestep.
subroutine flow_init_usertimestep(iresult)
   use m_flowtimes
   use dfm_error
   use MessageHandling
   use m_flowparameters, only: janudge
   use m_partitioninfo, only: jampi, abort_all

   implicit none
   integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

 iresult = DFM_GENERICERROR

 call inctime_user()

 tim1fld = max(time_user, tim1fld )
 if ( janudge.eq.1 ) call setzcs()
 call flow_setexternalforcings(tim1fld ,.false. , iresult)    ! set field oriented forcings. boundary oriented forcings are in
 if (iresult /= DFM_NOERR) then
    goto 888
 end if

 ! call flow_externalinput (time_user)                 ! receive signals etc
   iresult = DFM_NOERR
   return ! Return with success.

888 continue

 if (iresult /= DFM_NOERR) then
    write (msgbuf,*) ' Error found in EC-module ' ; call err_flush()
    if (jampi == 1) then
       write(msgbuf,*) 'Error occurs on one or more processes when setting external forcings on boundaries at time=', tim1bnd;
       call err_flush()
       ! Terminate all MPI processes
       call abort_all()
    endif
    goto 888
 end if

end subroutine flow_init_usertimestep
