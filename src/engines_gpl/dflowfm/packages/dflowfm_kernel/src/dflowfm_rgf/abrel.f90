      SUBROUTINE ABREL(X1,Y1,B1R,NFAC)
      implicit none
      integer :: nfac
      double precision :: X1(NFAC+1), Y1(NFAC+1), B1R(NFAC+1)
      integer :: J
      double precision :: B1
      B1 = 0
      DO 10 J = 2,NFAC+1
         B1     = B1 + SQRT( (X1(J)-X1(J-1))**2 + (Y1(J)-Y1(J-1))**2 )
         B1R(J) = B1
    10 CONTINUE

      DO 20 J = 2,NFAC+1
         B1R(J) = B1R(J)/B1R(NFAC+1)
    20 CONTINUE
      RETURN
      END subroutine abrel
