      SUBROUTINE ARROWrcir(X0,Y0,cs,sn)
      USE M_WEARELT
      implicit none
      double precision :: cs
      integer :: i
      double precision :: sn
      double precision :: x0
      double precision :: y0
      double precision :: X(3), Y(3), XR(3), YR(3)
      DATA X(1)  /0.8d0/, X(2) /1d0/, X(3) /0.8d0/,  &
           Y(1) /-0.1d0/, Y(2) /0d0/, Y(3) /0.1d0/

      DO 10 I = 1,3
         XR(I) = X0 + 3*rcir*(X(I)*CS - Y(I)*SN)
         YR(I) = Y0 + 3*rcir*(Y(I)*CS + X(I)*SN)
   10 CONTINUE

      CALL MOVABS(X0,Y0)
      CALL LNABS(XR(2),YR(2))
      CALL LNABS(XR(1),YR(1))

      CALL MOVABS(XR(2),YR(2))
      CALL LNABS(XR(3),YR(3))
      RETURN
      END
