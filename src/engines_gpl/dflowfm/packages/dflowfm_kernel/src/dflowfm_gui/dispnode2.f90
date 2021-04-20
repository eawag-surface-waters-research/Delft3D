      SUBROUTINE DISPNODE2 (MP, NP)
      use m_grid, only : zc
      use m_devices
      implicit none
      integer :: mp, np
      CHARACTER TEX*23

      IF (MP .LE. 0) THEN
         TEX = 'NODE NOT FOUND        '
         CALL KTEXT(TEX,IWS-22,4,15)
      ELSE
         TEX = 'NODE NR:              '
         WRITE(TEX (10:),'(I4,A1,I4)') MP, ',', NP
         CALL KTEXT(TEX,IWS-22,4,15)
!         TEX = 'ZC Lev :           (m)'
!         WRITE(TEX (10:18),'(F9.3)') zc(mp,np)
!         CALL KTEXT(TEX,IWS-22,5,15)
      ENDIF

      RETURN
      END
