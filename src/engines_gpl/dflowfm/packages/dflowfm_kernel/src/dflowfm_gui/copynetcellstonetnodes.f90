subroutine copynetcellstonetnodes() ! for smooth plotting only
 use m_flowgeom
 use m_flow
 use m_netw
 implicit none
 integer           :: k, kk, kkk, n, nn, nn4, ierr, ja
 real, allocatable, save :: rn(:)
 double precision  :: znn
 double precision  :: znod

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
 do n = 1, ndx2d
    nn4  = netcell(n)%n

    znn  = rlin(n)

    do kk = 1, nn4
       kkk       = netcell(n)%nod(kk)
       rnod(kkk) = rnod(kkk) + znn*ba(n)
       rn  (kkk) = rn(kkk)   + ba(n)
    enddo
 enddo

 do k = 1,numk
    if (rn(k)  >  0) then
       rnod(k) = rnod(k)/rn(k)
    endif
 enddo

 end subroutine copynetcellstonetnodes !in afwachting van isosmoothflownodes
