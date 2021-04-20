!> Sums all monitored data on all cross sections, including time-integrated values.
!! for sequential/non-MPI models: stored in crs()%sumvalcur/sumvalcum
!! for parallel/MPI models: stored in sumvalcur_tmp, and needs later mpi_allreduce:
!! @see updateValuesOnCrossSections_mpi
subroutine updateValuesOnCrossSections(tim1)
use m_monitoring_crosssections
use m_missing
use m_transport , only: NUMCONST_MDU
use m_partitioninfo, only: jampi
use m_sediment, only: jased, stmpar

implicit none
    double precision, intent(in) :: tim1 !< Current (new) time

    double precision,                 save        :: timprev = -1d0
    double precision,                 save        :: timstart
    double precision                              :: timstep, timtot
    integer                                       :: iv, icrs, numvals

    ! This routine can now be called any time, but will only do the update
    ! of sumval* when necessary:
    if (tlastupd_sumval == tim1) then
       return
    end if

    numvals  = 5 + NUMCONST_MDU

    if( jased == 4 .and. stmpar%lsedtot > 0 ) then
       numvals = numvals + 1
       if( stmpar%lsedsus > 0 ) then
          numvals = numvals + 1
       endif
       numvals = numvals + stmpar%lsedtot
    endif

    if (.not. allocated(sumvalcum_timescale)) then
       allocate(sumvalcum_timescale(numvals))
       sumvalcum_timescale = 1d0
    endif

    if (.not. allocated(sumvalcur_tmp)) then
       allocate(sumvalcur_tmp(numvals,ncrs))
       sumvalcur_tmp = 0d0
       if (jampi==1) then
          if (.not. allocated(sumvalcumQ_mpi)) then
             allocate(sumvalcumQ_mpi(ncrs))
             sumvalcumQ_mpi = 0d0
          endif
       endif
    endif

    if (timprev == -1d0) then
        timstep  = 0d0
        timstart = tim1 ! Generally tstart_user
        timtot   = 0d0
    else
        timstep = tim1 - timprev
        timtot  = tim1 - timstart
    end if

!   compute cross-section data for all cross-sections
    call sumvalueOnCrossSections(sumvalcur_tmp, numvals)

    if (jampi == 0) then
      tlastupd_sumval = tim1 ! Only when jampi==0 the sumval arrays are directly correct after filling. See also updateValuesOnCrossSections_mpi()
      ! NOTE: when jampi==1, the cross section sumvals on GUI screen are *not* correct, except at each ti_his interval.
      do icrs=1,ncrs
         do iv = 1, numvals ! Nu nog "5+ Numconst" standaard grootheden, in buitenlus
            crs(icrs)%sumvalcur(iv) = sumvalcur_tmp(iv,icrs)
            crs(icrs)%sumvalcum(iv) = crs(icrs)%sumvalcum(iv) + max(sumvalcum_timescale(iv),1d0)*timstep*sumvalcur_tmp(iv,icrs)
            if (timtot > 0d0) then
                crs(icrs)%sumvalavg(iv) = crs(icrs)%sumvalcum(iv)/timtot/max(sumvalcum_timescale(iv),1d0)
            else
                crs(icrs)%sumvalavg(iv) = crs(icrs)%sumvalcur(iv)
            end if
          end do
      end do
    else
       do icrs=1,ncrs ! Compute time-integrated discharge in current history output interval
          sumvalcumQ_mpi(icrs) = sumvalcumQ_mpi(icrs) + max(sumvalcum_timescale(IPNT_Q1C),1d0)*timstep*sumvalcur_tmp(IPNT_Q1C,icrs)
       enddo
    endif

    timprev = tim1
end subroutine updateValuesOnCrossSections
