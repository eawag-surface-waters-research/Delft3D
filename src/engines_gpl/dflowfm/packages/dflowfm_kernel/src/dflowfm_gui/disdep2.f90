      SUBROUTINE DISDEP2(DEP)
      use m_devices
      implicit none
      double precision :: dep
      CHARACTER DISTAN*23

      DISTAN = 'D2:                    '
      WRITE(DISTAN (5:),'(F8.3)') DEP
      CALL KTEXT(DISTAN,IWS-22,5,15)

      RETURN
      END
