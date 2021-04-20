      SUBROUTINE GIVEKEY(KEY)
      implicit none
      integer :: key
      CHARACTER TEX*14
      TEX = ' KEYPRESS=    '
      WRITE(TEX(11:14),'(I4)') KEY
      CALL KTEXT(TEX,1,3,15)
      RETURN
      END
