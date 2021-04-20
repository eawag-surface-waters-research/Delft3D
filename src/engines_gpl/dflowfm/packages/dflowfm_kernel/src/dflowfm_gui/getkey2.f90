      SUBROUTINE GETKEY2(KEY)
      implicit none
      integer :: i
      integer :: key
      integer :: keynum
      integer :: nkey
      integer :: numc
      integer :: numkeys
      COMMON /NKEYS/ NUMKEYS, NKEY(20), NUMC(20)
      KEYNUM = -999
      DO 10 I = 1,NUMKEYS
         IF (KEY .EQ. NKEY(I)) KEYNUM = I
   10 CONTINUE
      IF (KEYNUM .NE. -999) KEY = NUMC(KEYNUM)
      RETURN
      END
