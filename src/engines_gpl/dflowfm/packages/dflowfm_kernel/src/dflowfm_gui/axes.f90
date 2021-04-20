      SUBROUTINE AXES()
      use unstruc_colors
      implicit none
      integer :: jaxis
      double precision :: xleft
      double precision :: ybot
      COMMON /SCREENAREA/ XLEFT,YBOT,JAXIS
      IF (JAXIS .EQ. 1) THEN
         CALL SETCOL(KLAXS)
         CALL viewport(0.0,0.0,1.0,1.0)
         CALL IPGBORDER()
         CALL IPGXTICKPOS(Y1,Y2)
         CALL IPGXSCALE     ('TN')
         CALL IPGXSCALETOP  ('TN')
         CALL IPGYTICKPOS(X1,X2)
         CALL IPGYSCALELEFT ('TN')
         CALL IPGYSCALERIGHT('TN')
         CALL SMALLSCREEN()
      ENDIF
      RETURN
      END
