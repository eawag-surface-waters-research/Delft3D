      subroutine removewallfromsamples()
      use m_samples
      use m_polygon

      implicit none
      integer :: k, k2, k3, kk, mout

      call newfil(mout, 'wall.xyz')

      call savesam()

      kk = 0
      do k = 1,ns
!         call findnearwallpoint(k,k2)
         call findneargroundpoint(k,k3)
         if (k3 .ne. 0) then ! .and. k2 .ne. 0) then
            npl = npl + 1
            xpl(npl) = xs(k)
            ypl(npl) = ys(k)
            write(mout,*) xs(k), ys(k), zs(k)
         else
            kk = kk + 1
            xs2(kk) = xs(k)
            ys2(kk) = ys(k)
            zs2(kk) = zs(k)
         endif
      enddo

      close (mout)

      ns2 = kk
      call restoresam()

      end subroutine removewallfromsamples
