      SUBROUTINE DISVALCOLORS(NUMCOL,N1,N2,N3,IC)
      USE M_DEVICES
      implicit none
      integer :: ic
      integer :: n1
      integer :: n2
      integer :: n3
      integer :: numcol
      CHARACTER TEXT*47
      IF (IC .EQ. 1) THEN
         TEXT = 'COLOR NUMBER:     RED:     g    :     b   :    '
      ELSE IF (IC .EQ. 2) THEN
         TEXT = 'COLOR NUMBER:     r  :     GREEN:     b   :    '
      ELSE
         TEXT = 'COLOR NUMBER:     r  :     g    :     BLUE:    '
      ENDIF
      WRITE(TEXT(15:17) ,'(I3)') NUMCOL
      WRITE(TEXT(23:25) ,'(I3)') N1
      WRITE(TEXT(34:36) ,'(I3)') N2
      WRITE(TEXT(44:46) ,'(I3)') N3
      CALL KTEXT(TEXT,IWS-46,4,15)
      RETURN
      END
