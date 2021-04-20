      SUBROUTINE DX0ISX1(X0,Y0,Z0,X1,Y1,Z1,KMAX)
      implicit none
      integer :: k
      DOUBLE PRECISION X0(KMAX), X1(KMAX),  &
             Y0(KMAX), Y1(KMAX),  &
             Z0(KMAX), Z1(KMAX)
      integer :: KMAX
      DO 10 K = 1,KMAX
         X0(K) = X1(K)
         Y0(K) = Y1(K)
         Z0(K) = Z1(K)
   10 CONTINUE
      RETURN
      END SUBROUTINE DX0ISX1
