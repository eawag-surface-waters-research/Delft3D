! update cellmask from samples
subroutine samples_to_cellmask2()

   use network_data
   use m_samples
   use m_missing, only: jins, dmiss
   use geometry_module ! , only: pinpok

   implicit none

   integer :: i, in, k, kk, n, nn
   double precision :: xx(6), yy(6)

   if ( allocated(cellmask) ) deallocate(cellmask)
   allocate(cellmask(nump1d2d)) ; cellmask = 0

   zs(1:ns) = 1

   call increasepol(6*nump, 0)
   npl = 0

   do k = 1,nump
      nn = netcell(k)%N
      if (nn .lt.1 ) cycle

      do n = 1,nn
         kk = netcell(k)%nod(n)
         npl = npl + 1
         xpl(npl) = xk(kk)
         ypl(npl) = yk(kk)
         zpl(npl) = 1d0
      enddo
      npl = npl + 1 ; xpl(npl) = dmiss ; ypl(npl) = dmiss ; zpl(npl) = dmiss

   enddo

   in = -1

   do i=1,NS    !  generate cell mask

      call dbpinpol(xs(i), ys(i), in, dmiss, 1, NPL, xpl, ypl, zpl) ! ALS JE VOOR VEEL PUNTEN MOET NAGAAN OF ZE IN POLYGON ZITTEN

      ! call pinpok(xs(i), ys(i), nn, xx, yy, in, jins, dmiss)

      if ( ipolyfound > 0 ) then
           cellmask(ipolyfound) = 1
      end if

   end do


   return
end subroutine samples_to_cellmask2
