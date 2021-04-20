      SUBROUTINE CIRR2(X,Y,NCOL,R)
      use m_wearelt
      implicit none
      integer :: ncol
      double precision :: x,y,r
      CALL SETCOL(NCOL)
      CALL MOVABS(X,Y)
      CALL CIR(RCIR*R)
      RETURN
      END
