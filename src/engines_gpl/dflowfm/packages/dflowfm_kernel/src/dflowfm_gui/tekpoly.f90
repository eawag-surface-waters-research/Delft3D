      subroutine tekpoly(n,x,y,ncol)
      implicit none
      integer,                        intent(in) :: N    !< polygon dimension
      double precision, dimension(n), intent(in) :: x,y  !< polygon coordinates
      integer,                        intent(in) :: ncol !< color number
      integer                                    :: i

      if ( N.lt.3 ) return

      call setcol(ncol)
      call movabs(x(N),y(N))

      do i = 1,N
         call lnabs(x(i),y(i))
      end do

      return
      end
