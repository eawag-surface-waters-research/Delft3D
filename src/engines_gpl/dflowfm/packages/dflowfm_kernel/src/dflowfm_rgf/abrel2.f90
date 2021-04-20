      SUBROUTINE ABREL2(X,Y,D,NN,T)

      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none
      integer :: j
      integer :: nn
      double precision :: X(NN), Y(NN), D(NN)
      DOUBLE PRECISION :: T
      D(1) = 0
      DO 10 J = 2,NN
         D(J) = D(J-1) + DBDISTANCE( X(J-1),Y(J-1), X(J), Y(J), jsferic, jasfer3D, dmiss)
    10 CONTINUE
      T = D(NN)

      DO 20 J = 1,NN
         D(J) = D(J)/T
    20 CONTINUE
      RETURN
      END SUBROUTINE ABREL2
