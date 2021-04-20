 subroutine copyzlintornod() ! for smooth plotting only
 use m_flowgeom
 use m_flow
 use m_netw
 implicit none
 integer           :: L, k, k1, k2, ierr, ja
 real, allocatable, save :: rn(:)
 double precision  :: zL, aL
 double precision  :: zlin

 ja = 0
 if (.not. allocated(rn) ) then
     ja = 1
 else if (size(rn) < numk) then
     deallocate(rn) ; ja = 1
 endif
 if (ja == 1) then
     allocate ( rn(numk) , stat = ierr)
     call aerr('rn(numk)', ierr , numk)
 endif

 rnod = 0d0; rn = 0d0
 do L   = lnx1D + 1, lnxi            ! regular 2D flow links
    k1  = lncn(1,L)                  ! netnode 1
    k2  = lncn(2,L)                  ! netnode 2
    zL  = zlin(L)
    aL  = dx(L)*wu(L)
    rnod(k1) = rnod(k1) + zL*aL
    rn  (k1) = rn  (k1) + aL
    rnod(k2) = rnod(k2) + zL*aL
    rn  (k2) = rn  (k2) + aL
 enddo

 do k = 1,numk
    if (rn(k)  >  0) then
       rnod(k) = rnod(k)/rn(k)
    endif
 enddo

 end subroutine copyzlintornod
