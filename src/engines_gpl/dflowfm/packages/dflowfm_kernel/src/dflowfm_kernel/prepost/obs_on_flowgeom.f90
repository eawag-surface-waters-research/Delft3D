subroutine obs_on_flowgeom(iobstype)

    use m_observations
    use unstruc_messages
    use m_partitioninfo
    use m_flowgeom, only : xz,yz,ndx2D,ndxi
    use unstruc_caching

    implicit none

    integer, intent(in) :: iobstype !< Which obs stations to update: 0=normal, 1=moving, 2=both
    integer :: i, k, n, n1, n2, k1b, iobs
    double precision           :: d1, d2
    logical                    :: cache_success

    integer                    :: jakdtree
    jakdtree = 1  ! use kdtree (1) or not (other)

    ! Include normal stations?
    if (iobstype == 0 .or. iobstype == 2) then
        n1 = 1
    else
        n1 = numobs+1
    end if

    ! Include moving stations?
    if (iobstype == 1 .or. iobstype == 2) then
        n2 = numobs+nummovobs
    else
        n2 = numobs
    end if

    ! Try to read normal (non-moving) stations from cache file
    cache_success = .false.
    if (iobstype == 0 .or. iobstype == 2) then
       if ( cacheRetrieved() ) then
          call copyCachedObservations( cache_success )
       endif
    end if

    if (cache_success) then
       ! When necessary, process also the moving observation points (which are not in cache file)
       if ((iobstype == 1 .or. iobstype == 2) .and. nummovobs > 0) then
          call find_flownode_for_obs(numobs+1, numobs+nummovobs)
       end if
    else
       ! No cache, so process all requested observation points.
       call find_flownode_for_obs(n1, n2)
    endif

    if (loglevel_StdOut == LEVEL_DEBUG) then
       do iobs = n1,n2
          if (kobs(iobs)<ndx2D) then
             write(msgbuf, '(a,i0,a,i0,a)') "Obs #",iobs,":"//trim(namobs(iobs))//" on node ",kobs(iobs)," (2D)"
          else if (kobs(iobs)<=ndxi) then
             write(msgbuf, '(a,i0,a,i0,a)') "Obs #",iobs,":"//trim(namobs(iobs))//" on node ",kobs(iobs)," (1D)"
          endif
          call mess(LEVEL_INFO, msgbuf)
       enddo
    endif

    return
end subroutine obs_on_flowgeom
