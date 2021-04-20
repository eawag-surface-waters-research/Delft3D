!> Convert qid (from .ext file) to tracer name (split in generic qidname and specific tracer name).
!! If the input qid is not tracer, then the same qid is returned (and no tracer name)
subroutine get_tracername(qid, trname, qidname)
   use m_transport, only: DEFTRACER
   implicit none

   character(len=*), intent(in)  :: qid     !< Original quantityid, e.g., 'tracerbndfluor'.
   character(len=*), intent(out) :: trname  !< The trimmed tracer name, e.g., 'fluor'.
   character(len=*), intent(out) :: qidname !< The base quantity name for further use in external forcing, e.g., 'tracerbnd'.

   trname = ''
   qidname = qid

   if ( qid(1:9).eq.'tracerbnd' ) then
      qidname = qid(1:9)
      if ( len_trim(qid).gt.9 ) then
         trname = trim(qid(10:))
      else
         trname = trim(DEFTRACER)
      end if
   else if (qid(1:13).eq.'initialtracer' ) then
      qidname = qid(1:13)
      if ( len_trim(qid).gt.13 ) then
         trname = trim(qid(14:))
      else
         trname = trim(DEFTRACER)
      end if
   end if

   return
end subroutine get_tracername
