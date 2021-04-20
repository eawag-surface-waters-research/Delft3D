      SUBROUTINE RCIRC(X,Y)
      use m_wearelt
      implicit none
      double precision :: x
      double precision :: y
      CALL MOVABS(X,Y)
      CALL CIR(RCIR)
      RETURN
      END
