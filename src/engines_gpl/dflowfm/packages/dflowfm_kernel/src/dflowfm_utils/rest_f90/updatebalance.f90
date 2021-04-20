subroutine updateBalance()
   use m_flow
   use m_partitioninfo
   implicit none

   integer :: ivar

   if (jampi == 1) then
      call reduce_bal(cumvolcur, MAX_IDX)
!     only need to reduce the first two entries of volcur, but we do the reduce for the whole array here
      call reduce_bal(volcur,    MAX_IDX)
   endif
   do ivar = 1,MAX_IDX
      if (ivar == IDX_STOR) then
         voltot(IDX_STOR)    = volcur(IDX_STOR) - vol1ini
      else if (ivar == IDX_VOLTOT) then
         voltot(IDX_VOLTOT)  = volcur(IDX_VOLTOT)
      else if (ivar == IDX_ICEPT) then
         voltot(IDX_ICEPT)  = volcur(IDX_ICEPT)
      else
         ! All other variables are simply cumlative total in time:
         voltot(ivar)  = voltot(ivar)  + cumvolcur(ivar)
      end if
   end do

   cumvolcur = 0d0
end subroutine updateBalance
