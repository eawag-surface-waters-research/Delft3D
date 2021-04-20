      SUBROUTINE DISVAL1(DEP)
      use unstruc_colors
      implicit none
      double precision :: DEP
      CHARACTER TEX*8
      IF (ABS(DEP) .LT. 10) THEN
         WRITE(TEX(1:),'(F8.5)') DEP
      ELSE IF (ABS(DEP) .LT. 100) THEN
         WRITE(TEX(1:),'(F8.4)') DEP
      ELSE IF (ABS(DEP) .LT. 1000) THEN
         WRITE(TEX(1:),'(F8.3)') DEP
      ELSE IF (ABS(DEP) .LT. 10000) THEN
         WRITE(TEX(1:),'(F8.2)') DEP
      ELSE IF (ABS(DEP) .LT. 100000) THEN
         WRITE(TEX(1:),'(F8.1)') DEP
      ELSE
         WRITE(TEX(1:),'(E8.1)') DEP
      ENDIF
      CALL KTEXT(TEX,IWS-7,4,15)
      RETURN
      END SUBROUTINE DISVAL1
