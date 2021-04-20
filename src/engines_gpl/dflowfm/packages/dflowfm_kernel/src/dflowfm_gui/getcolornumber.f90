      SUBROUTINE GETCOLORNUMBER(XP,YP,NUMCOL,N1O,N2O,N3O)
      implicit none
      integer :: i
      integer :: n1
      integer :: n1o
      integer :: n2
      integer :: n2o
      integer :: n3
      integer :: n3o
      integer :: numcol
      double precision :: xp
      double precision :: yp
      CALL IGRGETPIXELRGB(real(XP),real(YP),N1O,N2O,N3O)
      DO 10 I = 0,255
         CALL SETCOL(I)
         CALL PTABS(XP,YP)
         CALL IGRGETPIXELRGB(real(XP),real(YP),N1,N2,N3)
         IF (N1 .EQ. N1O .AND. N2 .EQ. N2O .AND. N3 .EQ. N3O) THEN
            NUMCOL = I
            CALL DISVALCOLORS(NUMCOL,N1,N2,N3,1)
            RETURN
         ENDIF
   10 CONTINUE
      RETURN
      END
