      SUBROUTINE DISMIN(X,X2,Y,Y2,XX,YY,N,DIS,TV,XV,YV)
      implicit none
      integer :: n
      double precision :: rn
!     ZOEK MEEST NABIJE PUNT OP SPLINE
!     START ZOEKEN ROND TV, ZOEK MET GULDEN SNEDE ROUTINE
!     N IS MAXIMUM INDEX ZOEKGEBIED
      DOUBLE PRECISION :: X(N), X2(N), Y(N), Y2(N), XV, YV, XX, YY, TV
      DOUBLE PRECISION :: AX, BX, CX, TOL, DIS

!     RLEN = SQRT((X(1)-X(2))**2+(Y(1)-Y(2))**2)
      TOL  = 0.000001d0
!     TOL  = 0.000005*RLEN
      RN  = dble(N)
      AX  = 0d0
      BX  = TV
      CX  = RN
      CALL GOLD(AX,BX,CX,TOL,TV,X,X2,Y,Y2,XX,YY,N,DIS)

      CALL SPLINT(X,X2,N,TV,XV)
      CALL SPLINT(Y,Y2,N,TV,YV)

      RETURN
      END SUBROUTINE DISMIN
