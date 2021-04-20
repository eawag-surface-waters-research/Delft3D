!
      SUBROUTINE CLS1()
      use unstruc_display
      implicit none
      integer :: ndraw
      COMMON /DRAWTHIS/  ndraw(50)

      Call IGRAREACLEAR()

      IF (NDRAW(10) .EQ. 2) THEN
         CALL IGRPALETTERGB(  2,NREDP,NGREENP,NBLUEP)
      ELSE
         CALL IGRPALETTERGB(  2,NREDS,NGREENS,NBLUES)
      ENDIF

      CALL SETCOL(2)

      CALL FBOXnop(X1,Y1,X2,Y2)

      RETURN
      END
