 subroutine inifcori()
 use m_flowgeom
 use m_flow
 use m_sferic
 implicit none
 integer :: ierr, L, k, i, LL, LLL, LLLL, k1, k2, k3, n, j

 if (jsferic > 0) then
    if (allocated(fcori) ) then
       deallocate(fcori)
    endif

    if (icorio <= 6) then
       allocate ( fcori(lnx), stat = ierr )
       call aerr('fcori(lnx)', ierr, lnx  )
       do L = 1,lnx
          fcori(L) = 2d0*omega*sin(yu(L)*dg2rd)
       enddo
    else
       allocate ( fcori(ndx), stat = ierr )
       call aerr('fcori(ndx)', ierr, ndx  )
       do k = 1,ndx
          fcori(k) = 2d0*omega*sin(yz(k)*dg2rd)
       enddo
    endif

    if (jacorioconstant == 1) then
       fcori = 2d0*omega*sin(anglat*dg2rd)
    endif

    if ( jasecflow > 0 .and. kmx == 0 ) then

      ! Corilios in flow node, added by Nabi
       if (allocated(fcoris) ) then
          deallocate(fcoris)
       endif
       allocate ( fcoris(ndx), stat = ierr )
       call aerr('fcoris(ndx)', ierr, ndx  )
       do k = 1,ndx
          fcoris(k) = 2d0*omega*sin(yz(k)*dg2rd)
       enddo
    endif
 endif

 if (icorio > 40) then
    !if (allocated(LLkkk) ) then
    !   deallocate(LLkkk)
    !endif

    n = 0
    do j = 1,2  ! 1=count, 2=allocate and use
       if (j == 2) then
           allocate ( LLkkk(5,n) , stat = ierr ); LLkkk = 0
       endif
       n = 0
       do L = 1,lnx
          do i = 1,2
             k = ln(i,L)
             do LL   = 1, nd(k)%lnx                            ! loop over all attached links  k1,L1,k2,L2,k3
                LLL  = nd(k)%ln(LL)                            !                              ( 3  1  4  2  5, L)
                LLLL = iabs(LLL)
                if (L < LLLL) then
                   n = n + 1
                   if (j == 2) then
                      if (i == 2) then
                          k1 = ln(1,L)
                          k2 = ln(2,L)
                      else
                          k1 = ln(2,L)
                          k2 = ln(1,L)
                      endif
                      k3 = ln(1,LLLL) + ln(2,LLLL) - k2
                      LLkkk(1,n) = L
                      LLkkk(2,n) = LLLL
                      LLkkk(3,n) = k1
                      LLkkk(4,n) = k2
                      LLkkk(5,n) = k3
                   endif
                endif
             enddo
          enddo
       enddo
    enddo
 endif

 end subroutine inifcori
