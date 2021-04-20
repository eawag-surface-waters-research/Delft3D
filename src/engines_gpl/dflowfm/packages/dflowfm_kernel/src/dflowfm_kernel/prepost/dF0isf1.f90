      SUBROUTINE DF0ISF1(X0,X1,KMAX)
      implicit none
      DOUBLE PRECISION X0(KMAX), X1(KMAX)
      integer :: KMAX

      integer :: K

      DO 10 K = 1,KMAX
         X0(K) = X1(K)
   10 CONTINUE
      RETURN
      END SUBROUTINE DF0ISF1
