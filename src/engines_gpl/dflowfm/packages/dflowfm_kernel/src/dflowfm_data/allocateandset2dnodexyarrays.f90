 subroutine allocateandset2Dnodexyarrays(n)
 use m_netw
 use m_flowgeom
 use m_sferic

 implicit none
 integer          :: n, IERR
 ! locals
 integer          :: m,k,nn
 double precision :: xmn, xmx

 nn = netcell(n)%n

! GD: memory leak
 if(allocated(nd(n)%x)) deallocate(nd(n)%x)
 if(allocated(nd(n)%y)) deallocate(nd(n)%y)
 if(allocated(nd(n)%nod)) deallocate(nd(n)%nod)

 allocate ( nd(n)%x(nn), nd(n)%y(nn), nd(n)%nod(nn), stat=ierr )
 call aerr('nd(n)%x(nn), nd(n)%y(nn), nd(n)%nod(nn)', ierr, nn*3)
 do m  = 1,nn
    k  = netcell(n)%NOD(m)
    nd(n)%x(m) = xk(k)
    nd(n)%y(m) = yk(k)
    nd(n)%nod(m) = k;
 enddo

 if (jsferic == 1) then ! jglobe
    xmn   = minval( nd(n)%x )
    xmx   = maxval( nd(n)%x )
    if (xmx - xmn > 180d0) then
       do m  = 1,nn
          k  = netcell(n)%NOD(m)
          if ( xmx - nd(n)%x(m) > 180d0) then
             nd(n)%x(m) = nd(n)%x(m) + 360d0
          endif
       enddo
    endif
 endif

 end subroutine allocateandset2Dnodexyarrays
