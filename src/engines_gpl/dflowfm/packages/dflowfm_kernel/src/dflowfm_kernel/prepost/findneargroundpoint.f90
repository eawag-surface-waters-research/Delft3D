      subroutine findneargroundpoint(k1,k2)

      use m_samples
      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none
      integer          :: k1,k2,k,kk,n1,n2


      k2 = 0
      n1 = max(1,k1-2000)
      n2 = min(ns,k1+2000)
      do k = n1,n2
         if (k .ne. k1) then
            if (dbdistance(xs(k), ys(k), xs(k1), ys(k1), jsferic, jasfer3D, dmiss)< 0.5d0) then
               if ( zs(k1) - zs(k) > 0.9d0)  then
                  k2 = k ; return
               endif
            endif
         endif
      enddo
      end subroutine findneargroundpoint
