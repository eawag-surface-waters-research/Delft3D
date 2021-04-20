 subroutine volsur()                                 ! volsur entirely in s1 because of s1 iteration
 use timers
 use m_flowgeom
 use m_flow
 use m_ship

 implicit none
 ! locals
 integer           :: japerim
 integer           :: L, n, k1, k2, k
 double precision  :: ha, hh
 integer, save    	    :: handle = 0

 call timstrt('Volume calculation', handle)
 japerim = 0

! call sets01zbnd(1) ! set s1 on z-boundaries   SPvdP: not necessary, values at the boundaries were already properly filled in solve_matrix, as the boundary nodes are included in the solution vector

 if (nonlin2D == 0) then

   !$OMP PARALLEL DO                              &
   !$OMP PRIVATE(n,hh)
    do n = 1,ndx2d
       hh = max(0d0, s1(n)-bl(n) )
       vol1(n) = ba(n)*hh
       a1(n)   = ba(n)
    enddo
   !$OMP END PARALLEL DO    !   TODO OMP|

 else

    vol1(1:ndx2d) = 0d0
    a1  (1:ndx2d) = 0d0

 endif

 if (nonlin == 0) then

    do n = ndx2d+1, ndxi
       hh = max(0d0, s1(n)-bl(n) )
       vol1(n) = ba(n)*hh
       a1(n)   = ba(n)
    enddo

 else
    vol1  (ndx2D+1:ndxi) = 0d0
    a1    (ndx2D+1:ndxi) = 0d0
 endif

 if (nonlin >= 2) then
    a1m = 0d0
 endif

 !call checkvolnan(1)
 call VOL12D(japerim)                                   ! and add area's and volumes of 1D links
 !call checkvolnan(2)

 do L = lnxi+1,Lnx
    k1 = ln(1,L) ; k2 = ln(2,L)
    a1  (k1) = ba(k2)                                   ! set bnd a1 to ba of inside point
    vol1(k1) = vol1(k2) ! a1(k1)*(s1(k1) - bl(k1))
 enddo

 call timstop(handle)

 end subroutine volsur
