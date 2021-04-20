!
      SUBROUTINE SETXOR(I)
      implicit none
      integer :: i
!
      IF (I .EQ. 1) THEN
         CALL IGRPLOTMODE('E')
      ELSE IF (I .EQ. 0) THEN
         CALL IGRPLOTMODE('N')
      ENDIF
!
      RETURN
      END
