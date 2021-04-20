      SUBROUTINE DISCOUR(M,N,DEP)
      use m_devices
      implicit none
      double precision :: dep
      integer :: m
      integer :: n
      CHARACTER DISTAN*23

      DISTAN = 'M:    N:    CRT:       '
      WRITE(DISTAN (3:5),'(I3)') M
      WRITE(DISTAN (9:11),'(I3)') N
      WRITE(DISTAN (17:23),'(F7.2)') DEP
      CALL KTEXT(DISTAN,IWS-22,4,15)

      RETURN
      END
