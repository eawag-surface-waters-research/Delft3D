 subroutine setkfs()                                 ! set kfs
 use m_flow
 use m_flowgeom
 use m_flowtimes

 implicit none

 integer :: i, L, LL
 integer :: n, kb, ki, ndn

 kfs = 0

 ! open all grid points with positive lateral inflow
 do ndn = 1, ndx
   if (qin(ndn)>1d-12) then
     kfs(ndn) = 1
   endif
 enddo

 if (ivariableteta<=1) then                          ! fully implicit and teta=constant

    do L=1,lnx                                       ! implicit points
       if (hu(L)> 0) then
           kfs(ln(1,L))=1
           kfs(ln(2,L))=1
       endif
    enddo

 else                                                ! set kfs ic. teta; 0=not, 1 =impl, 2 = expl

    do L=1,lnx                                       ! explicit points
       if (hu(L)> 0) then
           if (teta(L) == 0) then
              kfs(ln(1,L))=2
              kfs(ln(2,L))=2
           else if (teta(L) > 0) then
              kfs(ln(1,L))=1                         ! todo: or bnd, randjes ook altijd impliciet
              kfs(ln(2,L))=1
          endif
       endif
    enddo

 endif

! water-level Neumann boundaries: add boundary cells whose corresponding internal cell is wet (but boundary face is inactive)
 do n=1, nbndz
    kb = kbndz(1,n)
    ki = kbndz(2,n)
    if ( kfs(ki).eq.1 ) then
         kfs(kb) = 1
    end if
 end do

 ! velocity boundaries: Neumann water-level boundaries are applied
 do n=1,nbndu
    kb = kbndu(1,n)
    ki = kbndu(2,n)
    if ( kfs(ki).eq.1 ) then
       kfs(kb) = 1
    end if
 end do

 end subroutine setkfs
