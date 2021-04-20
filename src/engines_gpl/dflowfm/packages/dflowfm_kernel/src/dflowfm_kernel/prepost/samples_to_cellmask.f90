! update cellmask from samples
subroutine samples_to_cellmask()

   use network_data
   use m_samples
   use m_missing, only: jins, dmiss
   use geometry_module, only: pinpok

   implicit none

   integer :: i, in, k, kk, n, nn
   double precision :: xx(6), yy(6)

   if ( allocated(cellmask) ) deallocate(cellmask)
   allocate(cellmask(nump1d2d)) ; cellmask = 0

   zs(1:ns) = 1

   do k = 1,nump
      nn = netcell(k)%N
      if (nn .lt.1 ) cycle

      do n = 1,nn
         kk = netcell(k)%nod(n)
         xx(n) = xk(kk)
         yy(n) = yk(kk)
      enddo

      in = -1

      do i=1,NS    !  generate cell mask

         if (zs(i) == -1) cycle

         call pinpok(xs(i), ys(i), nn, xx, yy, in, jins, dmiss)

         if ( in.gt.0 ) then
!           mask cell
            cellmask(k) = 1; zs(i) = -1
            exit
         end if

      end do
   end do


   return
end subroutine samples_to_cellmask
