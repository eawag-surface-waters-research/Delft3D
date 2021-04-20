!> Initializes all administration necessary for writing lateral discharge output to his-files.
subroutine init_lateral_his()
use m_wind
use m_flowparameters, only: jahislateral
use m_alloc
use m_partitioninfo, only: jampi
implicit none
   ! At the starting time of history output, initialize variables
   if (jahislateral > 0.and. numlatsg > 0) then
      call realloc(qplatCum,       numlatsg, keepExisting = .false., fill = 0d0)
      call realloc(qplatCumPre,    numlatsg, keepExisting = .false., fill = 0d0)
      call realloc(qplatAve,       numlatsg, keepExisting = .false., fill = 0d0)
      call realloc(qLatReal,       numlatsg, keepExisting = .false., fill = 0d0)
      call realloc(qLatRealCum,    numlatsg, keepExisting = .false., fill = 0d0)
      call realloc(qLatRealCumPre, numlatsg, keepExisting = .false., fill = 0d0)
      call realloc(qLatRealAve,    numlatsg, keepExisting = .false., fill = 0d0)

      if (jampi == 1) then
         call realloc(qLatRealMPI, numlatsg, keepExisting = .false., fill = 0d0)
      end if
   end if
   end subroutine init_lateral_his
