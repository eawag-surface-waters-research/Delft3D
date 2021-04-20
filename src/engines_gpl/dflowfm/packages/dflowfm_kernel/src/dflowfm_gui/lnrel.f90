!
      SUBROUTINE LNREL(X,Y)
      implicit none
      double precision :: x
      double precision :: y
      CALL IGRLINETOREL(real(X),real(Y))
      END
