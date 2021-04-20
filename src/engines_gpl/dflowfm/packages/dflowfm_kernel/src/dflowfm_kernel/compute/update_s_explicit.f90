 subroutine update_s_explicit()
    use m_flow
    use m_flowgeom
    use m_flowtimes
    use m_partitioninfo
    use m_timer
    use m_sobekdfm
    implicit none

    double precision            :: qwave
    integer                     :: k, k1, k2, L
    integer                     :: numchanged
    integer                     :: iter, ierror

    double precision, parameter :: dtol = 1d-16

!!   check if upwinddirection has changed
!    numchanged = 0
!    do L=1,Lnx
!       if ( u0(L)*u1(L).lt.-dtol ) then
!          numchanged = numchanged+1
!       end if
!    end do
!    if ( numchanged.gt.0 ) then
!!       write(6,*) numchanged
!       continue
!    end if

!    do iter=1,1
!
!!   recompute hu
!    call sethu(0)
!
!!   recompute Au
!    call setau()
!
!!   recompute q1, qa (as in u1q1)
!!$OMP PARALLEL DO           &
!!$OMP PRIVATE(L,k1,k2)
!    do L=1,Lnx
!       if ( hu(L).gt.0 ) then
!          k1 = ln(1,L)
!          k2 = ln(2,L)
!          q1(L) = au(L)*u1(L)
!          qa(L) = au(L)*u1(L)
!       else
!          q1(L) = 0d0
!          qa(L) = 0
!       end if
!    end do
!!$OMP END PARALLEL DO
!
!    do L = 1,lnx
!
!       if (q1(L) > 0) then
!          k1 = ln(1,L) ; k2 = ln(2,L)
!          squ(k1) = squ(k1) + q1(L)
!          sqi(k2) = sqi(k2) + q1(L)
!       else if (q1(L) < 0) then
!          k1 = ln(1,L) ; k2 = ln(2,L)
!          squ(k2) = squ(k2) - q1(L)
!          sqi(k1) = sqi(k1) - q1(L)
!       endif
!
!    enddo
!
!    sq = sqi-squ
!
!
!    sqwave = 0d0
!    do L=1,Lnx
!       k1 = ln(1,L); k2 = ln(2,L)
!       qwave = 2d0*sqrt(hu(L)*ag)*Au(L)   ! 2d0: safety
!       sqwave(k1) = sqwave(k1) + max(q1(L)+qwave,0d0)
!       sqwave(k2) = sqwave(k2) - min(q1(L)-qwave,0d0)
!    end do

    do k=1,Ndx
       s1(k) = s0(k) + sq(k)*bai(k)*dts
    end do
    call sets01zbnd(1, 0) ! expl

!   synchronise all water-levels
    if ( jampi.eq.1 ) then
       if ( jatimer.eq.1 ) call starttimer(IUPDSALL)
       call update_ghosts(ITYPE_SALL, 1, Ndx, s1, ierror)
       if ( jatimer.eq.1 ) call stoptimer(IUPDSALL)
    end if

!    end do

    return
 end subroutine update_s_explicit
