      SUBROUTINE RSORT3new (X, Y, Z, N) ! 1 !!  second faster than
      use sorting_algorithms, only: indexx
      implicit none
      double precision              :: X(N), Y(N), Z(N)
      integer, allocatable          :: ind(:)
      double precision, allocatable :: h(:)
      integer :: k, n

      allocate(ind(n), h(n))

      call indexx(n,x,ind)

      h = x
      do k = 1,n
         x(k) = h(ind(k))
      enddo

      h = y
      do k = 1,n
         y(k) = h(ind(k))
      enddo

      h = z
      do k = 1,n
         z(k) = h(ind(k))
      enddo

      deallocate(ind,h)

      end SUBROUTINE RSORT3new
