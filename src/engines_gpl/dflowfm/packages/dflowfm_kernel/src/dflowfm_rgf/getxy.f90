      SUBROUTINE GETXY(T,X,X2,Y,Y2,imax,N,NT,SSQ,XT,YT,TT,H)
!     zoek TT in X,Y, en XT,YT met dezelfde afstand geeft als
!     SSQ
      !USE DIMENS
      implicit none
      integer :: imax, n, nt
      double precision :: ssq, xt, yt
      double precision :: X(imax), Y(imax), X2(imax), Y2(imax), T(imax)
      double precision, intent(in) :: H   !< for curvature adapted meshing

      double precision, intent(out) :: TT

      double precision :: ax, bx, cx, tol, dis

      AX = T(1)
      CX = T(NT)
      BX = (AX+CX)/2
      TOL = 0.00001d0
!     Dan bijhorende T zoeken
      CALL GOLDDIS(AX,BX,CX,TOL,X,X2,Y,Y2,T,N,NT,TT,DIS,SSQ,H)

!     EN punt invullen
      CALL SPLINTXY(X,Y,X2,Y2,N,TT,XT,YT)

      RETURN
      END
