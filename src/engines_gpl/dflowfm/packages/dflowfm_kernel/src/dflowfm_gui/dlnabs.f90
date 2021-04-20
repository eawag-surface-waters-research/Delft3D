      SUBROUTINE DLNABS(XD,YD,ZD)
      USE m_oldz
      USE m_missing
      use gridoperations
      implicit none
      double precision :: x
      double precision :: y
      double precision :: z
      DOUBLE PRECISION XD,YD,ZD
      CALL DRIETWEE(XD,YD,ZD,X,Y,Z)
      !IF (OZ .NE. DMISS .AND. Z .NE. DMISS) THEN
       CALL LNABS(X,Y)
      !ENDIF
      OZ = Z
      END
