      SUBROUTINE KCIR(X,Y,Z)
      use unstruc_colors
      USE M_MISSING
      use m_wearelt
      implicit none
      integer :: ncol
      double precision :: x
      double precision :: y
      double precision :: z

      IF (Z .NE. dmiss) THEN
         CALL ISOCOL(Z,NCOL)
         CALL MOVABS(X,Y)
         CALL CIR(RCIR)
      ELSE
         CALL SETCOL(ncolhl)
         CALL MOVABS(X,Y)
         CALL CIR(RCIR)
      ENDIF
      RETURN
      END
