subroutine updateValuesOnSourceSinks(tim1)
use m_flowexternalforcings, only: qsrc, qsrcavg, vsrccum, vsrccum_pre, numsrc
use m_missing
use m_flowtimes, only: ti_his, time_his
use precision
use m_flowparameters, only: eps10
implicit none
   double precision, intent(in) :: tim1 !< Current (new) time

   double precision,                 save        :: timprev = -1d0 ! TODO: save is unsafe, replace by using time1 and time0, also two other occurrences
   double precision                              :: timstep
   integer                                       :: i

   if (timprev < 0d0) then
      allocate(qsrcavg(numsrc))
      allocate(vsrccum(numsrc))
      allocate(vsrccum_pre(numsrc))
      vsrccum = 0d0
      vsrccum_pre = 0d0
      qsrcavg = 0d0
   else
      timstep = tim1 - timprev
      ! cumulative volume from Tstart
      do i = 1, numsrc
         vsrccum(i) =vsrccum(i) + timstep*qsrc(i)
      enddo

      if (comparereal(tim1, time_his, eps10)== 0) then
         do i = 1, numsrc
            qsrcavg(i) = (vsrccum(i) - vsrccum_pre(i)) / ti_his ! average discharge in the past His-interval
            vsrccum_pre(i) = vsrccum(i)
         enddo
      endif
   end if

   timprev = tim1
end subroutine updateValuesOnSourceSinks
