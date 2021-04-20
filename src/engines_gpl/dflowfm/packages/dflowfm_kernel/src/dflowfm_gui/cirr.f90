      SUBROUTINE CIRR(X,Y,NCOL)
      use m_wearelt
      implicit none
      integer :: ncol
      double precision :: x
      double precision :: y
      CALL SETCOL(NCOL)
      CALL MOVABS(X,Y)
      CALL CIR(RCIR)
      RETURN
      END
