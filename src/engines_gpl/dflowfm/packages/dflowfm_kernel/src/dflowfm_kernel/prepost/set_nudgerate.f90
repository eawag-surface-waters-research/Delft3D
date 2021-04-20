!>  set nudge rates [1/s] from input in following order of preference:
!>     1. nudge time [s]
!>     2. nudge rate [NUDGE_RATE_UNIT_TO_SECI]
!>     3. uniform nudge time [s]
!>
!>  caution: will overwrite nudge_rate in 1/s
   subroutine set_nudgerate()
      use m_flowgeom, only: Ndx
      use m_flowparameters, only: Tnudgeuni
      use m_nudge
      use m_missing
      implicit none

      integer :: k

      do k=1,Ndx
         if ( nudge_time(k).eq.DMISS ) then
            if ( nudge_rate(k).ne.DMISS ) then
               nudge_rate(k) = NUDGE_RATE_UNIT_TO_SECI * nudge_rate(k)
            else if ( Tnudgeuni.gt.0d0 ) then
               nudge_rate(k) = 1d0 / Tnudgeuni
            else
               nudge_rate(k) = 0d0
            end if
         else if ( nudge_time(k).gt.0d0 ) then
            nudge_rate(k) = 1d0 / nudge_time(k)
         else
            nudge_rate(k) = 0d0
         end if
      end do

      return
   end subroutine set_nudgerate
