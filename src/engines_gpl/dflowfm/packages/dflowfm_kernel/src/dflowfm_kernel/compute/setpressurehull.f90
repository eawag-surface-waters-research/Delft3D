 subroutine setpressurehull()
 use m_ship
 use m_flowgeom
 use m_flow
 implicit none
 integer :: L, LL, k1, k2
 double precision :: r1,r2
 do LL = 1,Lnx
    k1 = ln(1,LL) ; k2 = ln(2,LL)
    if (zsp(k1) .ne. 0d0 .or. zsp(k2) .ne. 0d0) then
       do L = Lbot(LL), Ltop(LL)
          adve(L) = adve(L) + ag*( zsp(k2) - zsp(k1) )*dxi(LL)          ! impose ship hull
       enddo
    endif
 enddo
 end subroutine setpressurehull
