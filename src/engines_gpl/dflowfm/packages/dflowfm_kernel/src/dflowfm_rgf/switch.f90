      SUBROUTINE SWITCH(X, Y, mmax, nmax, JN, NUMPJ)
!      USE DIMENS
      implicit none
      integer :: mmax, nmax, jn, numpj
      double precision :: X(MMAX,NMAX),   Y(MMAX,NMAX)

       integer :: j
       double precision :: xh, yh

      DO 10 J = 1,NUMPJ/2
         XH              = X(JN,J)
         X(JN,J)         = X(JN,NUMPJ-J+1)
         X(JN,NUMPJ-J+1) = XH
         YH              = Y(JN,J)
         Y(JN,J)         = Y(JN,NUMPJ-J+1)
         Y(JN,NUMPJ-J+1) = YH
    10 CONTINUE
      RETURN
      END subroutine switch
