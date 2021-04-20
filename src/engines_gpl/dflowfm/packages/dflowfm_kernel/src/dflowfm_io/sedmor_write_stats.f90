   subroutine sedmor_write_stats(tim)
   use m_sediment, only: stm_included, stmpar
   use m_flowparameters, only: eps10
   use m_flowtimes, only: ti_sed, ti_seds, ti_sede, tstop_user, time_sed
   use precision_basics
   use m_fm_morstatistics

   implicit none

   double precision, intent(in)      :: tim
   integer                           :: ierr
   double precision                  :: tem_dif

   if (.not.stm_included) return
   if (.not. stmpar%morpar%moroutput%morstats) return

   ierr = 1
   if (stmpar%morpar%moroutput%morstats .and. ti_sed > 0) then
     if (comparereal(tim, time_sed, eps10) >= 0) then
          call unc_write_sed(tim)
          call morstats_clearstats()
         if (comparereal(time_sed, ti_sede, eps10) == 0) then
            time_sed = tstop_user + 1
         else
            tem_dif = (tim - ti_seds)/ti_sed
            time_sed = max(ti_seds + (floor(tem_dif + 0.001d0) +1)*ti_sed,ti_seds)

            if (comparereal(time_sed, ti_sede, eps10) == 1) then
            ! next time_map would be beyond end of map-window, write one last map exactly at that end.
                time_sed = ti_sede
            endif
         endif
     endif
   endif

   ierr = 0

1234 continue
   return
end subroutine sedmor_write_stats
