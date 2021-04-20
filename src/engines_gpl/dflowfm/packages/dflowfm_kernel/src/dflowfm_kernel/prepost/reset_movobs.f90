 !> Reset moving observation stations.
 !! Necessary because moving stations are always reread from .ext file
 !! (i.e. *after* MDU read, as opposed to static stations
 subroutine reset_movobs()
    use m_observations
    implicit none

    integer :: i

    do i=numobs+1,numobs+nummovobs
        call deleteObservation(i)
    end do
    call purgeObservations()
 end subroutine reset_movobs
