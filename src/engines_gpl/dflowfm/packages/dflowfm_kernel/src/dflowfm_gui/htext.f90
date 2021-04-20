      SUBROUTINE HTEXT(VAL,X,Y)
      implicit none
      integer :: ncolnow
      double precision :: val
      double precision :: x
      double precision :: y
!     getal value op grafisch scherm in current color
      CHARACTER TEXT*6, TEXT2*10
      COMMON /COLNOW/ NCOLNOW
      IF (NCOLNOW .GE. 0) THEN
         IF (-1.000d0 .LT. VAL .AND. VAL .LT. 10.000d0) THEN
            WRITE(TEXT(1:6),'(F6.3)') VAL
            CALL DRAWTEXT(real(X),real(Y), TEXT)
         ELSE IF (-10.000d0 .LT. VAL .AND. VAL .LT. 100.000d0) THEN
            WRITE(TEXT(1:6),'(F6.2)') VAL
            CALL DRAWTEXT(real(X),real(Y), TEXT)
         ELSE IF (-100.000d0 .LT. VAL .AND. VAL .LT. 1000.000d0) THEN
            WRITE(TEXT(1:6),'(F6.1)') VAL
            CALL DRAWTEXT(real(X),real(Y), TEXT)
         else
            WRITE(TEXT2,'(e10.3)') VAL
            CALL DRAWTEXT(real(X),real(Y), TEXT2)
         ENDIF
      ENDIF

      RETURN
      END
