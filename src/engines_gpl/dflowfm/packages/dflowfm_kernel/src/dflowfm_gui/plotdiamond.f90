      subroutine plotDiamond(x, y)
      use m_wearelt
      implicit none
      double precision :: x
      double precision :: y

      CALL MOVABS(X+.5*RCIR,Y)
      CALL LNABS(X, Y+.5*RCIR)
      CALL LNABS(X-.5*RCIR,Y)
      CALL LNABS(X, Y-.5*RCIR)
      CALL LNABS(X+.5*RCIR,Y)

      RETURN
      END
