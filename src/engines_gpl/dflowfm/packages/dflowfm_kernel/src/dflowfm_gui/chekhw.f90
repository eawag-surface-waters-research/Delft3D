      SUBROUTINE CHEKHW()
      implicit none
      integer :: infogrscreen
      integer :: key

!  check the hardware in use - must have graphics

      LOGICAL  NOGRAF

      NOGRAF = InfoGrScreen(1).EQ.0
      IF (NOGRAF) THEN
          CALL IOutError('Sorry, this program requires a display '      &
                    //'with graphics capability - Press a key')
          CALL InKeyEvent(KEY)
!         exit tidily, clearing the screen
          CALL IScreenQuit('C')
      ENDIF
      RETURN
      END
