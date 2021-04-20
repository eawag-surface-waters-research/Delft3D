!> compute cross-section data, summed across all flow links for each cross-section.
!! In parallel models, only summed across flow links in own domain.
!! @see updateValuesOnCrossSections @see updateValuesOnCrossSections_mpi
subroutine sumvalueOnCrossSections(resu, numvals)
    use m_flow
    use m_flowgeom
    use m_monitoring_crosssections
    use m_partitioninfo
    use m_timer
    use m_transport, only: NUMCONST_MDU, ISALT, ITEMP, ISED1, ITRA1, constituents
    use m_sediment, only: jased, stmpar, sedtra


    implicit none
    integer, intent(in)           :: numvals             !< Which values to sum (1=discharge)
    double precision, intent(out) :: resu(numvals,ncrs)  !< cross-section data, note: ncrs from module m_monitoring_crosssections

    integer                       :: i, Lf, L, k1, k2, IP, num, LL
    integer                       :: icrs
    double precision              :: val
    integer                       :: lsed

    if ( ncrs.lt.1 ) return   ! nothing to do

    resu = 0d0

    do icrs=1,ncrs
       do i=1,crs(icrs)%path%lnx
           Lf = crs(icrs)%path%ln(i)
           if (Lf == 0) cycle ! Closed wall
           L  = abs(Lf)
           k1 = ln(1,L); k2 = ln(2,L)

           resu(IPNT_Q1C,icrs) = resu(IPNT_Q1C,icrs) + dble(sign(1, Lf)) * q1(L)                     ! discharge

           resu(IPNT_AUC,icrs) = resu(IPNT_AUC,icrs) + au(L)                                         ! area

           ! NOTE: IPNT_U1A is now not included.

           resu(IPNT_S1A,icrs) = resu(IPNT_S1A,icrs) + 0.5d0*( s1(k1) + s1(k2) ) * au(L)             ! weigted waterlevel

           resu(IPNT_HUA,icrs) = resu(IPNT_HUA,icrs) + hu(L) * au(L)                                 ! upwind waterdepth

           if( jatransportmodule.ne.0 ) then
              IP = IPNT_HUA
              do num = 1,NUMCONST_MDU
                 IP = IP + 1
                 do LL = Lbot(L), Ltop(L)
                    k1 = ln(1,LL); k2 = ln(2,LL)
                    resu(IP,icrs) = resu(IP,icrs) + dble(sign(1, Lf)) * ( max(q1(LL),0d0) * constituents(num,k1) &
                                                                        + min(q1(LL),0d0) * constituents(num,k2) )
                 enddo
              enddo
           endif

           if( jased == 4 .and. stmpar%lsedtot > 0 ) then ! todo, loop korter tot lsedsus.
              IP = IPNT_HUA + NUMCONST_MDU + 1 ! TODO: mourits/dam_ar: check whether all uses of NUMCONST versus NUMCONST_MDU are now correct.
              do lsed = 1,stmpar%lsedtot
                 resu(IP,icrs) = resu(IP,icrs) + sedtra%e_sbn(L,lsed) * wu_mor(L) * dble(sign(1, Lf))
              enddo
              if( stmpar%lsedsus > 0 ) then
                 IP = IP + 1
                 do lsed = 1,stmpar%lsedsus
                    resu(IP,icrs) = resu(IP,icrs) + sedtra%e_ssn(L,lsed) * wu(L) * dble(sign(1, Lf))
                 enddo
              endif
              do lsed = 1,stmpar%lsedtot    ! Making bedload on crosssections per fraction
                 IP = IP + 1
                 resu(IP,icrs) = resu(IP,icrs) + sedtra%e_sbn(L,lsed) * wu_mor(L) * dble(sign(1, Lf))
              enddo
           endif
       end do
    end do   ! do icrs=1,ncrs

    if( jased == 4 .and. stmpar%lsedtot > 0 ) then
       IP = IPNT_HUA + NUMCONST_MDU + 1
       sumvalcum_timescale(IP) = stmpar%morpar%morfac
       if( stmpar%lsedsus > 0 ) then
          IP = IP + 1;
          sumvalcum_timescale(IP) = stmpar%morpar%morfac
       endif
    endif

    if (jampi == 0 ) then
      ! NOTE: if jampi==1, it is incorrect to compute quantities that require division by AU values
      ! since these are not mpi_reduced yet. So, don't compute them at all in parallel runs.
      do icrs=1,ncrs
         if (resu(IPNT_AUC,icrs) > 0) then
             resu(IPNT_U1A,icrs) = resu(IPNT_Q1C,icrs) / resu(IPNT_AUC,icrs)                              ! average velocity
             resu(IPNT_S1A,icrs) = resu(IPNT_S1A,icrs) / resu(IPNT_AUC,icrs)                              ! average waterlevel
             resu(IPNT_HUA,icrs) = resu(IPNT_HUA,icrs) / resu(IPNT_AUC,icrs)                              ! average waterdepth
         endif
      end do
    endif

!!   BEGIN DEBUG
!    do icrs=1,ncrs
!       write(6,"('icrs=', I0, ', my_rank=', I0, ', Q=', G15.5)") icrs, my_rank, resu(2,icrs)
!    end do
!!   END DEBUG

end subroutine sumvalueOnCrossSections
