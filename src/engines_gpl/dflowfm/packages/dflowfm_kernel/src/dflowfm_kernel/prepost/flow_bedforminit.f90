!
! Bedform prediction routines
!
subroutine flow_bedforminit(stage)
   use m_bedform
   use m_bedform_io, only: fm_rdbedformpar, fm_initbedformpar
   use unstruc_model, only: md_bedformfile
   use MessageHandling, only: mess, LEVEL_FATAL

   implicit none

   logical                      :: error
   integer, intent(in)          :: stage

   if (stage==1) then

      call fm_initbedformpar(bfmpar, error)              ! need to initialize the data structure for
                                                         ! eg dredge, tauwave and bed roughness, even if no bedformfile there..
                                                         ! this resets bfmpar%lfbedfrmrou = .true. to .false., so need two stages:
                                                         ! one before sedmorinit, and one after
      if (error) then
         call mess(LEVEL_FATAL, 'unstruc::flow_bedforminit - Error in initialisation of bedform module.')
         return
      end if

   else if (stage==2) then

      !
      bfm_included = len_trim(md_bedformfile) /= 0
      if (.not. bfm_included) return
      !
      call fm_rdbedformpar(bfmpar, md_bedformfile, error)
      if (error) then
         call mess(LEVEL_FATAL, 'unstruc::flow_bedforminit - Error in reading of bedform file.')
         return
      end if
   end if

end subroutine flow_bedforminit
