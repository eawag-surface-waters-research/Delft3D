      SUBROUTINE DISAREAM(AREAM)
      use m_devices
      implicit none
      double precision :: aream
      CHARACTER DISTAN*23
      DISTAN = 'CR. AR. M            M2'
      WRITE(DISTAN (11:20),'(E10.4)') AREAM
      CALL KTEXT(DISTAN,IWS-22,6,15)
      RETURN
      END
