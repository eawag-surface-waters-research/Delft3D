!  copy dots to samples
   subroutine copy_dots2sam()
      use m_samples
      use m_plotdots
      implicit none

      integer :: i

      if ( numdots.lt.1 ) return

      call increasesam(Ns+numdots)

      do i=1,numdots
         Ns = Ns+1
         xs(Ns) = xdots(i)
         ys(Ns) = ydots(i)
         zs(Ns) = zdots(i)
      end do

!     clear dots
      numdots = 0

      return
   end subroutine copy_dots2sam
