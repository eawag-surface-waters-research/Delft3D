      SUBROUTINE cLNABS(X,Y,ncol)
      implicit none
      double precision :: x,y
      integer          :: ncol
      call setcol(ncol)
      CALL LNABS(X,Y)
      END
