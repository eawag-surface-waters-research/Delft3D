      SUBROUTINE DISLINK (MP)
      use m_devices
      implicit none
      integer :: mp
      CHARACTER TEX*23

      IF (MP .LE. 0) THEN
         TEX = 'LINK NOT FOUND        '
      ELSE
         TEX = 'LINK NR:              '
         WRITE(TEX (10:),'(I10)') MP
      ENDIF
      CALL KTEXT(TEX,IWS-22,4,15)

      RETURN
      END
