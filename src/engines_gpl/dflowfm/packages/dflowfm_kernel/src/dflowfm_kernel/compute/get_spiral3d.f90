   subroutine get_spiral3d
      use m_flow, only: ucx, ucy, spirint, czssf, zws, hs, dzslay, spiratx, spiraty
      use m_flowgeom, only: ndx

      implicit none
      integer :: k, kk, k1, kb, kt
      double precision :: uav, vav, utot, unx, uny, ut, un, zn, fn
      double precision :: sumff, sumfu

      do k = 1,ndx
         call getkbotktop( k, kb, kt )
         uav = 0d0
         vav = 0d0
         do kk = kb,kt
            k1 = kk - kb + 1
            uav = uav + ucx(kk) * dzslay(k1,1)
            vav = vav + ucy(kk) * dzslay(k1,1)
         enddo
         utot = sqrt( uav**2 + vav**2 )
         if( utot == 0d0 ) then
            spirint(k) = 0d0
            cycle
         endif
         spiratx(k) = uav / utot
         spiraty(k) = vav / utot
         sumff = 0d0
         sumfu = 0d0
         do kk = kb,kt
            zn = ( ( zws(kk) + zws(kk-1) ) * 0.5d0 - zws(kb-1) )/ hs(k)
            call findfn( czssf(k), zn, fn )
            ut  = ucx(kk) * spiratx(k) + ucy(kk) * spiraty(k)
            unx = ucx(kk) - ut * spiratx(k)
            uny = ucy(kk) - ut * spiraty(k)
            un  = -unx * spiraty(k) + uny * spiratx(k)   ! transverse component
            sumff = sumff + fn * fn
            sumfu = sumfu + fn * un
         enddo
         spirint(k) = 0d0
         if( sumff < 1d-6 ) cycle
         spirint(k) = sumfu / sumff
      enddo

   end subroutine get_spiral3d
