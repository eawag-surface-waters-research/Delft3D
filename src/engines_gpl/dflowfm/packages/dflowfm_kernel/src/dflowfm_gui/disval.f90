      SUBROUTINE DISVAL(M,N,DEP)
      use m_devices
      implicit none
      double precision :: dep
      integer :: m
      integer :: n
      integer :: ndraw
      COMMON /DRAWTHIS/ ndraw(50)
      CHARACTER DISTAN*23
      IF (NDRAW(14) .LE. 1) THEN
         DISTAN = 'M:     N:              '
      ELSE IF (NDRAW(14) .EQ. 2) THEN
         DISTAN = 'M:    N:    ZC:        '
      ELSE IF (NDRAW(14) .EQ. 3) THEN
         DISTAN = 'M:    N:    RES:       '
      ELSE IF (NDRAW(14) .EQ. 4) THEN
         DISTAN = 'M:    N:    MSM:       '
      ELSE IF (NDRAW(14) .EQ. 5) THEN
         DISTAN = 'M:    N:    NSM:       '
      ELSE IF (NDRAW(14) .EQ. 6) THEN
         DISTAN = 'M:    N:    MCU:       '
      ELSE IF (NDRAW(14) .EQ. 7) THEN
         DISTAN = 'M:    N:    NCU:       '
      ELSE IF (NDRAW(14) .EQ. 8) THEN
         DISTAN = 'M:    N:    MSZ:       '
      ELSE IF (NDRAW(14) .EQ. 9) THEN
         DISTAN = 'M:    N:    NSZ:       '
      ELSE IF (NDRAW(14) .EQ.10) THEN
         DISTAN = 'M:    N:    ASP:       '
      ELSE IF (NDRAW(14) .EQ.11) THEN
         DISTAN = 'M:    N:               '
      ELSE IF (NDRAW(14) .EQ.12) THEN
         DISTAN = 'M:    N:    DEP:       '
      ELSE IF (NDRAW(11) .EQ. 1) THEN
         DISTAN = 'M:    N:    CNM:       '
      ELSE IF (NDRAW(11) .EQ. 2) THEN
         DISTAN = 'M:    N:    CRM:       '
      ELSE IF (NDRAW(11) .EQ. 3) THEN
         DISTAN = 'M:    N:    CRN:       '
      ENDIF

      IF (M .EQ. 0) THEN
         DISTAN = 'NO POINT FOUND         '
      ELSE
         WRITE(DISTAN (3:6),'(I4)') M
         WRITE(DISTAN (10:13),'(I4)') N
         IF (NDRAW(14) .GE. 2 .AND. NDRAW(14) .LE. 10) THEN
            WRITE(DISTAN (16:23),'(F8.3)') DEP
         ELSE IF (NDRAW(14) .EQ. 11) THEN
            WRITE(DISTAN (17:23),'(F7.1)') DEP
         ELSE IF (NDRAW(11) .GE. 1 .AND. NDRAW(11) .LE. 3) THEN
            WRITE(DISTAN (17:23),'(F7.2)') DEP
         ENDIF
      ENDIF

      CALL KTEXT(DISTAN,IWS-22,4,15)

      RETURN
  END
