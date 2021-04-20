      SUBROUTINE DISAREAN(AREAN)
      use m_devices
      implicit none
      double precision :: arean
      CHARACTER DISTAN*23
      DISTAN = 'CR. AR. N            M2'
      WRITE(DISTAN (11:20),'(E10.4)') AREAN
      CALL KTEXT(DISTAN,IWS-22,5,15)
      RETURN
      END
