      SUBROUTINE BILIN(X, Y, Z, XP, YP, ZP)
      implicit none
      double precision :: r1
      double precision :: r2
      double precision :: x1
      double precision :: x2
      double precision :: xa
      double precision :: xp
      double precision :: xr
      double precision :: xrm
      double precision :: y1
      double precision :: y2
      double precision :: ya
      double precision :: yp
      double precision :: yr
      double precision :: yrm
      double precision :: zp
      double precision :: X(4), Y(4), Z(4)
!     Bepaal relatieve ligging in cel.
!     Twee coordinaten (xr,yr) van 0 tot 1.
!     Bilineaire interpolatie
      X1 = X(2) - X(1)
      Y1 = Y(2) - Y(1)
      R1 = X1*X1 + Y1*Y1
      X2 = X(4) - X(1)
      Y2 = Y(4) - Y(1)
      R2 = X2*X2 + Y2*Y2
      XA = XP - X(1)
      YA = YP - Y(1)
      XR = (X1*XA + Y1*YA)/R1
      YR = (X2*XA + Y2*YA)/R2

      XRM = 1 - XR
      YRM = 1 - YR
      ZP  = XRM*YRM*Z(1) + XR*YRM*Z(2) + XR*YR*Z(3) + XRM*YR*Z(4)
      RETURN
      END
