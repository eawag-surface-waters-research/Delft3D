      subroutine write_flowdiff()
      use m_flow
      use m_samples
      implicit none

      COMMON /DIAGNOSTICFILE/ MDIAG
      integer mdiag

      double precision :: avdiffm, avdifwq, fm, wq
      integer          :: k, kk, num
      double precision, external :: znod

      avdiffm = 0d0 ; avdifwq = 0d0; num = 0
      do k = 1,ns
         call in_flowcell(xs(k), ys(k), KK)
         if (kk > 0) then
            fm = znod(kk)
            wq = plotlin(kk)
            if (fm > 0d0 .and. wq > 0d0) then
               write(mdiag, *) zs(k), fm, wq
               avdiffm = avdiffm + abs( fm - zs(k) )
               avdifwq = avdifwq + abs( wq - zs(k) )
               num = num + 1
            endif
         endif
      enddo
      if (num > 0) then
         avdiffm = avdiffm / num
         avdifwq = avdifwq / num
      endif
      write(mdiag,*) ' avdiffm, avdifwq,num ' , avdiffm, avdifwq,num
      end subroutine write_flowdiff
