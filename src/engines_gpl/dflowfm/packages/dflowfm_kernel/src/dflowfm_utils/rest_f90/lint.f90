      SUBROUTINE LINT(X,Y,N,TV,XV,YV)
      implicit none
      integer :: n
      integer :: n1
      integer :: n2
      integer :: ntv
      double precision :: t
      double precision :: tv
      double precision :: xv
      double precision :: yv
!     Lineaire interpolatie op TV in lijn
      double precision :: X(N), Y(N)
      NTV  = INT(TV)
      T    = TV  - NTV
      N1   = NTV + 1
      N2   = N1  + 1
      XV   = (1-T)*X(N1) + T*X(N2)
      YV   = (1-T)*Y(N1) + T*Y(N2)
      RETURN
      END subroutine LINT
