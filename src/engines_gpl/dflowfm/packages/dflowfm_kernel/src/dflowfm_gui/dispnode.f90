      SUBROUTINE DISPNODE (MP)
      use m_devices
      use m_netw, only : zk
      implicit none
      integer :: mp
      CHARACTER TEX*23

      IF (MP .LE. 0) THEN
         TEX = 'NODE NOT FOUND        '
         CALL KTEXT(TEX,IWS-22,4,15)
      ELSE
         TEX = 'NODE NR:              '
         WRITE(TEX (10:),'(I10)') MP
         CALL KTEXT(TEX,IWS-22,4,15)

!         TEX = 'ZK Lev :           (m)'
!         WRITE(TEX (10:18),'(F9.3)') zk(mp)
!         CALL KTEXT(TEX,IWS-22,5,15)
      ENDIF

      RETURN
      END
