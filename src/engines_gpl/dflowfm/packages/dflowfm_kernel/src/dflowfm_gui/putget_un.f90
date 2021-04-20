      SUBROUTINE putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)
      implicit none
      integer :: ja
      integer :: key
      integer :: ndraw
      integer :: nput
      integer :: num
      integer :: numb
      integer :: nwhat
      double precision :: xp
      double precision :: yp
      COMMON /DRAWTHIS/  ndraw(50)

!
      CALL DISPUT(NPUT)

!     IF (KEY .EQ. 3) THEN
         CALL MENUH(0,NUM,NWHAT)
         CALL BOTLIN(0,NUMB,KEY)
         CALL FRAMES(31)
!     ENDIF

!
   20 CONTINUE
      CALL READLOCATOR(XP,YP,KEY)
!
      IF (KEY .GE. 24 .AND. KEY .LE. 26) THEN
         CALL FKEYS(KEY)
         IF (KEY .EQ. 3) RETURN
      ELSE IF (KEY .EQ. 1) THEN
!        BOVEN
         JA = KEY
         CALL MENUH(JA,NUM,NWHAT)
         CALL BOTLIN(0,NUMB,KEY)
         IF (JA .NE. 0) RETURN
      ELSE IF (KEY .EQ. 2) THEN
!        ONDER
         JA = KEY
         CALL BOTLIN(JA,NUMB,KEY)
         IF (JA .NE. 0) RETURN
      ELSE IF (KEY .EQ. 90 .OR. KEY .EQ. 90+32) THEN
!        Z(oomin)
         CALL ZOOMIN(KEY,NPUT)
         RETURN
      ELSE IF (KEY .EQ. 65 .OR. KEY .EQ. 65+32) THEN
!        A(nchor)
         CALL ANCHOR(XP,YP)
       ELSE IF (KEY .EQ. 170 .OR. KEY .EQ. 80 .OR. KEY .EQ. 80+32) THEN
         NDRAW(10) = 1
         KEY = 3
         RETURN
      ELSE
         RETURN
      ENDIF
      GOTO 20
      END
