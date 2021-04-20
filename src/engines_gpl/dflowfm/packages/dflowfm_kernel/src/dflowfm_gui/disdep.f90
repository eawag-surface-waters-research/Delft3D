      subroutine DISDEP (m,n,dep)
      use m_devices
      implicit none
      double precision :: dep
      integer :: m
      integer :: n
      character distan*23
      character fmt*6

      DISTAN = 'M:    N:    D:         '
      WRITE(DISTAN (3:5),'(I3)') M
      WRITE(DISTAN (9:11),'(I3)') N
      fmt = '(f9.3)'
      call dispform (dep, fmt)
      WRITE(DISTAN (15:23),fmt) DEP
      CALL KTEXT(DISTAN,IWS-22,4,15)

      RETURN
      END
