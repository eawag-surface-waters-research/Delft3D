   !============================================================================================
   ! SEDIMENT CONCENTRATION BOUNDARY SPUL VOOR STM_INCLUDED
   ! Volgt werkwijze tracerranden, met aanpassingen (bv aantal fracties gekend op voorhand)
   !============================================================================================
   subroutine get_sedfracname(qid, sfname, qidname)
   implicit none

   character(len=*), intent(in)  :: qid       !< Original quantityid, e.g., 'sedfracbndsediment1'.
   character(len=*), intent(out) :: sfname    !< The trimmed tracer name, e.g., 'sediment1'.
   character(len=*), intent(inout) :: qidname !< The base quantity name for further use in external forcing, e.g., 'sedfracbnd'.

   sfname = ''

   if ( index(qid,'sedfracbnd') == 1 ) then
      qidname = qid(1:10)
      if ( len_trim(qid)>10 ) then
         sfname = trim(qid(11:))
      else
         sfname = trim('unknown_sediment_fraction')
      end if
   else if (index(qid,'initialsedfrac') == 1 ) then
      qidname = qid(1:14)
      if ( len_trim(qid)>14 ) then
         sfname = trim(qid(15:))
      else
         sfname = trim('unknown_sediment_fraction')
      end if
   else if (index(qid,'initialverticalsedfracprofile') == 1 ) then
      qidname = qid(1:29)
      if ( len_trim(qid)>29 ) then
         sfname = trim(qid(30:))
      else
         sfname = trim('unknown_sediment_fraction')
      end if
   else if (index(qid,'initialverticalsigmasedfracprofile') == 1 ) then
      qidname = qid(1:34)
      if ( len_trim(qid)>34 ) then
         sfname = trim(qid(35:))
      else
         sfname = trim('unknown_sediment_fraction')
      end if
   end if
   end subroutine get_sedfracname
