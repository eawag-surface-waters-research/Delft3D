 subroutine a1vol1tot()
 use m_flowgeom
 use m_flow
 use m_partitioninfo
 use m_flowtimes
 use precision_basics
 implicit none

 double precision, dimension(1) :: dum

 integer :: k

 if ( jampi /= 1 ) then
    a1tot     = sum(a1  (1:ndxi))
    vol1tot   = sum(vol1(1:ndxi))
    if (interceptionmodel == DFM_HYD_INTERCEPT_LAYER) then
       vol1icept = sum(bare(1:ndxi)*InterceptHs(1:ndxi))
    endif
 else
    a1tot  = 0d0
    vol1tot = 0d0
    vol1icept = 0d0

    do k=1,Ndxi
       if ( idomain(k) == my_rank ) then
          a1tot   = a1tot + a1(k)
          vol1tot = vol1tot + vol1(k)
          if (interceptionmodel == DFM_HYD_INTERCEPT_LAYER) then
             vol1icept = vol1icept + bare(k)*InterceptHs(k)
          endif
       end if
    end do

    !a1tot     = sum(a1  (1:ndxi), mask=idomain(1:Ndxi).eq.my_rank)
    !vol1tot   = sum(vol1(1:ndxi), mask=idomain(1:Ndxi).eq.my_rank)

!   begin debug
!    call reduce_double_sum(vol1tot)
!   end debug
 end if

 if (jagrw > 0) then
    if (volgrw > 0 .and. volgrwini <= 0) then
       volgrwini = volgrw
    endif
 endif

 if (comparereal(time1, tstart_user, eps10)== 0) then
    volcur(IDX_VOLTOT) = vol1tot
    volcur(IDX_STOR)   = vol1tot
    vol1ini = vol1tot
!   vol1ini needs to be global
    if ( jampi.eq.1 ) then
       call reduce_double_sum(1, (/ vol1ini /), dum)
       vol1ini = dum(1)
    end if
 endif

! begin debug
!  if ( my_rank.eq.0 ) write(6,*) 'vol1tot =', vol1tot
! end debug

 end subroutine a1vol1tot
