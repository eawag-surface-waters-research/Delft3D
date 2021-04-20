      subroutine findnearwallpoint(k1,k2)

      use m_samples
      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none
      integer          :: k1,k2,k

      k2 = 0
      do k = 1,ns
         if (k .ne. k1) then
            if (dbdistance(xs(k), ys(k), xs(k1), ys(k1), jsferic, jasfer3D, dmiss) < 0.25d0) then
               if (zs(k) == zs(k1) ) then
                  k2 = k ; return
               endif
            endif
         endif
      enddo
      end subroutine findnearwallpoint
