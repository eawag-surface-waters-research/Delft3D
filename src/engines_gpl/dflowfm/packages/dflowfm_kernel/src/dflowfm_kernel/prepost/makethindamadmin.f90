subroutine makethindamadmin()
   use m_flowgeom
   use network_data
   use m_alloc
   use m_sferic, only: jsferic, jasfer3D
   use geometry_module, only: getdxdy, duitpl, dlinedis
   use m_missing, only: dmiss

   implicit none

   integer                      :: n, kk, L, k1, k3, k4, ja, ierr
   double precision             :: cs, sn, a, b, sig, dis, xn, yn, rrr

   ! count thin dam points
   nthd = 0
   do n=1, nump
      do kk=1,netcell(n)%n
         L = netcell(n)%lin(kk)
         if (lne2ln(L)==0) then
            nthd = nthd+1
         end if
      end do
   end do

   ! set up thin dam structure
   if ( allocated(thindam) ) deallocate(thindam)
   allocate(thindam(6,nthd), stat=ierr)
   call aerr( 'thindam(6,nthd)', ierr, 6*nthd)

   nthd = 0
   do n=1, nump
      do kk=1,netcell(n)%n
         L = netcell(n)%lin(kk)

         if (kn(3,L) .eq. 0) then
            nthd = nthd+1
            k1 = n
            k3 = kn(1,L)     ! netnode 1
            k4 = kn(2,L)     ! netnode 2

            thindam(1,nthd) = k1
            thindam(2,nthd) = k3
            thindam(3,nthd) = k4

            call duitpl(xzw(k1), yzw(k1), xk(k3), yk(k3), xzw(k1), yzw(k1), xk(k4), yk(k4), sig, jsferic)
            call dlinedis(xzw(k1), yzw(k1), xk(k3), yk(k3), xk(k4), yk(k4),JA,DIS,XN,YN, jsferic, jasfer3D, dmiss)
            a = 0d0; b = 0d0
            call getdxdy( xk(k3), yk(k3), xk(k4), yk(k4), a, b, jsferic)
            rrr = sqrt(a*a + b*b)
            cs  = 0d0 ; sn = 0d0
            if (rrr .ne. 0d0) then
               cs = sig*a/rrr
               sn = sig*b/rrr
            endif
            thindam(4,nthd) = cs
            thindam(5,nthd) = sn
            thindam(6,nthd) = rrr
         end if
      end do
   end do
end subroutine makethindamadmin
