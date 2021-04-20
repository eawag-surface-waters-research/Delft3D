!> Writes current state immediately to files, typically used in
!! case of 'emergencies', without checking output intervals.
!!
!! Writes his/map/rst data to the (existing) files.
!! Note: no timings/waq output.
subroutine flow_externaloutput_direct()
   use m_flowtimes
   use unstruc_messages
   use time_module
   implicit none
   integer :: iyear, imonth, iday, ihour, imin, isec

   call mess(LEVEL_INFO, 'Performing direct write of solution state...')

   ! Compute current absolute date time, based on time1 since refdat
   call datetime_from_refdat(time1, iyear, imonth, iday, ihour, imin, isec)
   write (msgbuf, '(a,i0,a,f12.2,a,a,a,a)') 'Simulation current time: nt = ', int(dnt, 8), ', time1 = ', time1, 's ', &
                             '(', trim(datetime_to_string(iyear, imonth, iday, ihour, imin, isec)), ').'
   call msg_flush()

   call wrimap(time1)

   call unc_write_his(time1)

   call wrirst(time1)

   call mess(LEVEL_INFO, 'Done writing solution state.')

end subroutine flow_externaloutput_direct
