!
      SUBROUTINE OKAY(JA)
      use m_devices
      use unstruc_display, only: jaGUI
      implicit none
      integer, intent(in) :: ja

      if ( jaGUI.ne.1 ) return

      CALL ISCREENBELL('ON')
      IF (JA .EQ. 1) then
        CALL ISCREENBELL(' ')
      end if
      CALL ISCREENBELL('OFF')
      RETURN

      END
