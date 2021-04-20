      subroutine plotCross(x, y)
      use m_wearelt
      implicit none
      double precision :: x
      double precision :: y

      CALL MOVABS(X-.5*RCIR,Y-.5*RCIR)
      CALL LNABS(X+.5*RCIR, Y+.5*RCIR)
      CALL MOVABS(X-.5*RCIR,Y+.5*RCIR)
      CALL LNABS(X+.5*RCIR, Y-.5*RCIR)
      RETURN
      END
