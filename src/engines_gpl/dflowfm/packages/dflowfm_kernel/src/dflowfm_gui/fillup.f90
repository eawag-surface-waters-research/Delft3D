      SUBROUTINE FILLUP(TEXT,CHAR,LEN)
      implicit none
      integer :: i, len
      CHARACTER TEXT*(*), CHAR*1
      DO 10 I = 1,LEN
         WRITE(TEXT(I:I),'(A)') CHAR
   10 CONTINUE
      RETURN
      END
