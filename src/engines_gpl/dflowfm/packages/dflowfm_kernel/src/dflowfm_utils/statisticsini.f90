 subroutine statisticsini()
 use m_statistics
 implicit none
 call statisticsnewstep()
 cumavedif = 0d0    ! for now only, cum dif with analytic sol
 cumrmsdif = 0d0    ! for now only, cum dif with analytic sol
 cumdmxdif = 0d0    ! for now only, cum dif with analytic sol
 numcum    = 0
 end subroutine statisticsini
