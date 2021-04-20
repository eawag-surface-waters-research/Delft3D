 subroutine doaddksources() ! add k sources
 use m_flow
 use m_flowtimes
 implicit none

 integer          :: n, k, kk, kk2
 double precision :: qsrck, dvoli, dtol = 1d-4

 do n  = 1,numsrc
    if (ksrc(2,n) == 0 .and. ksrc(5,n) == 0) cycle  ! due to initialisation

    if (arsrc(n) == 0) cycle
    kk    = ksrc(1,n)                   ! 2D pressure cell nr FROM
    kk2   = ksrc(4,n)                   ! 2D pressure cell nr TO
    qsrck = qsrc(n)

    if (kk > 0) then                    ! FROM Point
       k = ksrc(2,n) ; dvoli = 1d0/max(vol1(k),dtol)
       if (qsrck > 0) then              ! FROM k to k2
          turkinepsws(1,k) = turkinepsws(1,k) - dts*qsrck*dvoli*turkinepsws(1,k)
       else if  (qsrck  < 0) then       ! FROM k2 to k
          turkinepsws(1,k) = turkinepsws(1,k) - dts*qsrck*dvoli*0.5D0*(qsrck/arsrc(n))**2
       endif
    endif

    if (kk2 > 0) then                   ! TO Point
       k = ksrc(5,n) ; dvoli = 1d0/max(vol1(k),dtol)
       if (qsrck > 0) then
          turkinepsws(1,k) = turkinepsws(1,k) + dts*qsrck*dvoli*0.5D0*(qsrck/arsrc(n))**2
       else if  (qsrck  < 0) then
          turkinepsws(1,k) = turkinepsws(1,k) + dts*qsrck*dvoli*turkinepsws(1,k)
       endif
    endif

 enddo
 end subroutine doaddksources
