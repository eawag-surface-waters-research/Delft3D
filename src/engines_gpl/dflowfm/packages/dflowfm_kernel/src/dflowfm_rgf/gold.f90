      SUBROUTINE GOLD(AX,BX,CX,TOL,XMIN,P,P2,Q,Q2,XX,YY,N,DIS)
      implicit none
      double precision :: c
      double precision :: f0
      double precision :: f1
      double precision :: f2
      double precision :: f3
      integer :: n
      double precision :: r
      double precision :: x0
      double precision :: x3
      double precision :: spldist
      PARAMETER (R=.61803399,C=.38196602)

      DOUBLE PRECISION :: AX, BX, CX, TOL, DIS, XMIN, XX, YY, X1, X2

!     EENDIMENSIONAAL ZOEKEN VAN 'GEBRACKED' MINIMUM
      DOUBLE PRECISION :: P(N), P2(N), Q(N), Q2(N)
      X0=AX
      X3=CX
      IF(ABS(CX-BX).GT.ABS(BX-AX))THEN
        X1=BX
        X2=BX+C*(CX-BX)
      ELSE
        X2=BX
        X1=BX-C*(BX-AX)
      ENDIF
!     F1=F(X1)
      F1=SPLDIST(P,P2,Q,Q2,XX,YY,X1,N)
!     F2=F(X2)
      F2=SPLDIST(P,P2,Q,Q2,XX,YY,X2,N)
1     IF(ABS(X3-X0).GT.TOL*(ABS(X1)+ABS(X2)))THEN
!     IF(ABS(X3-X0).GT.TOL) THEN
        IF(F2.LT.F1)THEN
          X0=X1
          X1=X2
          X2=R*X1+C*X3
          F0=F1
          F1=F2
          F2=SPLDIST(P,P2,Q,Q2,XX,YY,X2,N)
!         F2=F(X2)
        ELSE
          X3=X2
          X2=X1
          X1=R*X2+C*X0
          F3=F2
          F2=F1
!         F1=F(X1)
          F1=SPLDIST(P,P2,Q,Q2,XX,YY,X1,N)
        ENDIF
      GOTO 1
      ENDIF
      IF(F1.LT.F2)THEN
        DIS =F1
        XMIN=X1
      ELSE
        DIS =F2
        XMIN=X2
      ENDIF
      RETURN
      END SUBROUTINE GOLD
