!> prepares a matrix for solver test (as in "mpitest")
subroutine make_matrix(CFL, s1)
      use m_reduce
      use m_flowgeom

      implicit none

      double precision,                   intent(in)  :: CFL     !< CFL-number
      double precision, dimension(Ndx),   intent(in)  :: s1      !< exact solution

      double precision                                :: aufu

      integer                                         :: k1, k2, n, L

      bbr = 1/CFL**2
      ccr = 0d0
      do L = 1,lnx
         aufu        = 1d0
         k1 = ln(1,L)
         k2 = ln(2,L)
         bbr(k1)     = bbr(k1)     + aufu
         bbr(k2)     = bbr(k2)     + aufu
         ccr(Lv2(L)) = ccr(Lv2(L)) - aufu
      enddo

!      dd = 0d0
!      do n=1,Ndx
!         dd(n) = dd(n) + bb(n)*s1(n)
!      end do

      ddr = bbr*s1
      do L=1,Lnx
         k1 = ln(1,L)
         k2 = ln(2,L)
         ddr(k1) = ddr(k1) + ccr(Lv2(L))*s1(k2)
         ddr(k2) = ddr(k2) + ccr(Lv2(L))*s1(k1)
      enddo

      return
   end subroutine make_matrix
