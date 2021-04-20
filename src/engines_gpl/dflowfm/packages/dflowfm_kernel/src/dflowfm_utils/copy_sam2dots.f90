!  copy samples to dots
   subroutine copy_sam2dots()
      use m_samples
      use m_plotdots
      implicit none

      integer :: i

      if ( NS.lt.1 ) return

      do i=1,Ns
         call adddot(xs(i),ys(i),zs(i))
      end do

!     clear samples
      Ns = 0

      return
   end subroutine copy_sam2dots
