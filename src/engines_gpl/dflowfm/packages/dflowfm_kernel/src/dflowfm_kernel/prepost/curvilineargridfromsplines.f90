      SUBROUTINE curvilinearGRIDfromsplines()
      USE M_SPLINES
      implicit none
      IF (MCS .EQ. 0) THEN
          CALL QNERROR('First Create or Open Splines',' ',' ')
          !NUM = 0
          RETURN
      ENDIF
      CALL SPLRGFR()
      end subroutine curvilinearGRIDfromsplines
