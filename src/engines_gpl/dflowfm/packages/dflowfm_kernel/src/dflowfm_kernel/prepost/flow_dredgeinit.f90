subroutine flow_dredgeinit()
   use m_dad
   use dredge_data_module,   only: initdredge
   use m_fm_dredge,   only: fm_rddredge
   use unstruc_model, only: md_dredgefile
   use m_sediment, only: stm_included, jased
   use m_flowparameters, only: jatransportmodule
   use MessageHandling, only: mess, LEVEL_FATAL

   implicit none

   logical                   :: error

   if (.not.stm_included) return
   dad_included = len_trim(md_dredgefile) /= 0
   if (.not. dad_included) return

   if ( stm_included .and. jased.ne.0 .and. jatransportmodule.eq.0 ) then
      call mess(LEVEL_FATAL, 'unstruc::flow_dredgeinit - Please use transport module with sediment model 4.')
   end if

   call initdredge(dadpar)
   call fm_rddredge(dadpar, md_dredgefile, error)
   if (error) then
      call mess(LEVEL_FATAL, 'unstruc::flow_dredgeinit - Error in initialisation of dredging module.')
   end if

end subroutine flow_dredgeinit
