      SUBROUTINE MAKES(X,Y,X2,Y2,T,S,S2,imax, N,NT,H)
!     maak X,Y splines + afstandsarray en splines S op basis
!     van NT snijpunten
      !USE DIMENS
      implicit none
      integer :: imax, n, nt
      double precision :: X(IMAX), Y(IMAX), X2(IMAX), Y2(IMAX), T(IMAX), S(IMAX), S2(IMAX)
      double precision, intent(in) :: H   !< for curvature adapted meshing

      integer :: i

      CALL SPLINXY(X,Y,X2,Y2,N)

      DO 10 I = 1,NT
          CALL GETDIS(X,Y,X2,Y2,N,T(I),S(I),H)
    10 CONTINUE
      CALL SPLINE(S,NT,S2)
      RETURN
      END subroutine makes
