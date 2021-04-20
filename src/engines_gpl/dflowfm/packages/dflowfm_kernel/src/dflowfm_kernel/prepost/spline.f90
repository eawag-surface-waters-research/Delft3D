      SUBROUTINE SPLINE(Y,N,Y2)
      implicit none
      integer :: i
      integer :: k
      integer :: n
      DOUBLE PRECISION              :: Y(N),Y2(N)
      DOUBLE PRECISION, ALLOCATABLE :: U(:)
      DOUBLE PRECISION              :: P

      ALLOCATE (U(N))

      Y2(1) = 0.D0
      U(1)  = 0.D0

      DO I = 2,N-1
         P     =  0.5D0*Y2(I-1) + 2D0
         Y2(I) = -0.5D0/P
         U(I)  = (6D0*( (Y(I+1)-Y(I)) - (Y(I)-Y(I-1)) ) / 2D0 - 0.5D0*U(I-1))/P
      ENDDO

      Y2(N) = 0.D0

      DO K = N-1,1,-1
        Y2(K) = Y2(K)*Y2(K+1) + U(K)
      ENDDO

      DEALLOCATE (U)
      RETURN
      END SUBROUTINE SPLINE
