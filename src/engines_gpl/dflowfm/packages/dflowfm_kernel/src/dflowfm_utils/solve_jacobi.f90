 subroutine solve_jacobi(s1,ndx,itsol)               ! uses both s0 and s1
 use m_flowgeom, only : lnx, ln, kfs, kcs, nd
 use m_flowtimes
 use m_jacobi
 use m_reduce

 implicit none

 double precision                  :: ds, rrn        ! max error
 integer                           :: L, n, k1, k2, ndx, itsol, nn, La, n1, n2, ni
 double precision                  :: s1(ndx)

 !$OMP PARALLEL DO                                          &
 !$OMP PRIVATE(n)
 do n = 1,ndx
    if (kfs(n) == 1) then
       bbi(n)   = 1d0/bbr(n)
       db(n)    = ddr(n)*bbi(n)
    endif
 enddo
 !$OMP END PARALLEL DO

 itmxjac = 100000
 itsol   = 0
 ds      = 1d10

 do while (ds > epscg)                              ! Jacobi

    if (mod(itsol, 2) == 0) then
       n1 = 1 ; n2 = ndx; ni =  1
    else
       n2 = 1 ; n1 = ndx; ni = -1
    endif


    !$OMP PARALLEL DO                                          &
    !$OMP PRIVATE(n,nn,L,La)
    do n = n1,n2,ni
       if (kfs(n) == 1) then
          s1(n) = db(n)
          do nn = 1,nd(n)%lnx
             L  = nd(n)%ln(nn) ; La = iabs(L)
             if ( ccr(Lv2(La)) < 0d0) then
                if (L > 0) then
                   s1(n) = s1(n) - ccr(Lv2(La))*s1(ln(1,La))*bbi(n)
                else
                   s1(n) = s1(n) - ccr(Lv2(La))*s1(ln(2,La))*bbi(n)
                endif
             endif
          enddo
       endif
    enddo
    !$OMP END PARALLEL DO

    ds = 1e10                                      ! some big nr
    if (mod(itsol,100) == 0) then
       !$xOMP PARALLEL DO                                          &
       !$xOMP PRIVATE(n,nn,L,La,rrn)
       do n = 1,ndx
          if (kfs(n) == 1) then
             rrn   = ddr(n) - bbr(n)*s1(n)                               ! For explicit points db = s0, so this does won't hurt
             do nn = 1,nd(n)%lnx
                L  = nd(n)%ln(nn) ; La = iabs(L)
                if ( ccr(Lv2(La)) < 0d0) then
                   if (L > 0) then
                      rrn = rrn - ccr(Lv2(La))*s1(ln(1,La))
                   else
                      rrn = rrn - ccr(Lv2(La))*s1(ln(2,La))
                   endif
                endif
             enddo
             ds = abs(rrn)
             if (ds > epscg) exit
          endif
       enddo
       !$xOMP END PARALLEL DO
    endif

    itsol = itsol + 1
    if (itsol == itmxjac) then
       exit
    endif

 enddo
 end subroutine solve_jacobi
