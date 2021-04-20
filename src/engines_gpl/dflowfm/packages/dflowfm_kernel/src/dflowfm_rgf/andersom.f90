      SUBROUTINE ANDERSOM(X,N)

      use m_alloc

      implicit none
      integer :: n
      double precision              :: X(N)
      integer :: i, ierr
      double precision, ALLOCATABLE :: XH(:)
      ALLOCATE ( XH(N), stat=ierr )
      call aerr('XH(N)', ierr, N)
      XH = X
      DO I= 1,N
         X(I) = XH(N-I+1)
      ENDDO

      DEALLOCATE (XH)
      call aerr('XH', ierr, -N)
      END SUBROUTINE ANDERSOM
