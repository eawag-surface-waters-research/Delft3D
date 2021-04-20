      SUBROUTINE DISPF2closed(X,Y,N,NMAX,NCOL)
      implicit none
      integer :: i
      integer :: n
      integer :: ncol
      integer :: nmax
!     closed polygon
      double precision :: X(NMAX), Y(NMAX)
      CALL SETCOL(NCOL)
      CALL MOVABS(X(1),Y(1))
      DO I = 2,N
         CALL LNABS(X(I),Y(I))
      enddo
      call lnABS(X(1),Y(1))
      RETURN
      END
