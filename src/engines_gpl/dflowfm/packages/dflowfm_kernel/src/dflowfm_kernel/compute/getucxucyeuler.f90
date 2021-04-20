!> Computes the Eulerian horizontal velocities.
!! In absence of waves, these are equal to the Lagrangian ucx/ucy.
!! The Stokes drift on links is averaged to cell centers using the Perot weights.
subroutine getucxucyeuler(N, ucxeu, ucyeu)
   use m_flowgeom
   use m_flow
   use m_waves, only: ustokes            ! available for all wave models

   implicit none

   integer,          intent(in   ) :: N        !< Length of cell arrays (probably ndkx)
   double precision, intent(  out) :: ucxeu(N) !< Target array in which to store Eulerian x-velocities
   double precision, intent(  out) :: ucyeu(N) !< Target array in which to store Eulerian y-velocities

   integer          :: i, Lb, Lt, L, LL, k, k1, k2
   double precision :: u1l, wcxu, wcyu, ueul

   ucxeu(1:ndkx) = ucx(1:ndkx) ; ucyeu(1:ndkx) = ucy(1:ndkx)
   if (jawave > 0) then
      do LL = 1,lnx
         Lb = Lbot(LL) ; Lt = Lb - 1 + kmxL(LL)
         do L = Lb, Lt
            if (ustokes(L) .ne. 0d0) then                    ! link flows
               k1 = ln(1,L)
               k2 = ln(2,L)
               ucxeu(k1) = ucxeu(k1) - wcx1(LL)*ustokes(L)
               ucyeu(k1) = ucyeu(k1) - wcy1(LL)*ustokes(L)
               ucxeu(k2) = ucxeu(k2) - wcx2(LL)*ustokes(L)
               ucyeu(k2) = ucyeu(k2) - wcy2(LL)*ustokes(L)
            endif
         enddo
      enddo
   endif
end subroutine getucxucyeuler
