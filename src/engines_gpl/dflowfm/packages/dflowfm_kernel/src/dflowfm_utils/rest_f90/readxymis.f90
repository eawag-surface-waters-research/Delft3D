      SUBROUTINE READXYMIS(MINP)
      USE M_MISSING
      implicit none
      integer :: ja
      integer :: l
      integer :: minp
!     snelheidsdrempel
      CHARACTER REC*132, TEX*8
      CALL ZOEKAL(MINP,REC,'MISSING VALUE XY',JA)
      XYMIS = 0d0
      IF (JA .EQ. 1) THEN
         L = INDEX(REC,'XY') + 4
         READ(REC(L:),*,ERR = 888) XYMIS
         WRITE(TEX,'(F8.3)') XYMIS
         CALL MESSAGE('MISSING VALUE XY = ', TEX,' ')
      ENDIF
      RETURN
  888 CALL READERROR('READING MISSING VALUE XY, BUT GETTING', REC, MINP)
      END
