      SUBROUTINE RESTOREKEYS()
      implicit none
      integer :: i
      integer :: keycod
      integer :: maxkey
      PARAMETER (MAXKEY = 50)
      COMMON /KEYCODES/ KEYCOD(MAXKEY)
      DO 10 I = 1,MAXKEY
         CALL INCONTROLKEY(I,KEYCOD(I))
   10 CONTINUE
      RETURN
      END
