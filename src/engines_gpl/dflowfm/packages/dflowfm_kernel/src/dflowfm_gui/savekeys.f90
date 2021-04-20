      SUBROUTINE SAVEKEYS()
      implicit none
      integer :: i
      integer :: infoinput
      integer :: keycod
      integer :: maxkey
      PARAMETER (MAXKEY = 50)
      COMMON /KEYCODES/ KEYCOD(MAXKEY)
      DO 10 I = 1,MAXKEY
         KEYCOD(I) = INFOINPUT(I)
   10 CONTINUE
      RETURN
      END
