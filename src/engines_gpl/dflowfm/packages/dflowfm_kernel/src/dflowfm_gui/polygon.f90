    SUBROUTINE POLYGON(X,Y,N,NCOL)
      implicit none
      integer :: i
      integer :: n
      integer :: ncol
      integer :: ncolnow
      double precision :: X(N), Y(N)
      COMMON /COLNOW/ NCOLNOW
      CALL SETCOL(NCOL)
      call PTABS(X(1),Y(1))
      DO 10 I = 2,N
         call LNABS(X(I),Y(I))
   10 CONTINUE
      call LNABS(X(1),Y(1))
      RETURN
    END
