      subroutine insertsamples(L1,L2)
      use m_samples
      use m_gridsettings, only: mfac
      implicit none
      integer :: L1, L2
      integer :: k
      double precision :: aa, bb

      do k = 1,mfac
         ns = ns + 1
         call increasesam(ns)
         aa = dble(k)/dble(mfac+1) ; bb = 1d0-aa
         xs(ns) = bb*xs(L1) + aa*xs(L2)
         ys(ns) = bb*ys(L1) + aa*ys(L2)
         zs(ns) = bb*zs(L1) + aa*zs(L2)
      enddo

!     user is editing samples: mark samples as unstructured
      MXSAM = 0
      MYSAM = 0
      IPSTAT = IPSTAT_NOTOK

      end subroutine insertsamples
