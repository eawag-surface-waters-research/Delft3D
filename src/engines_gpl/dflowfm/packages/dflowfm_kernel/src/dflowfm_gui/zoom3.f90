      SUBROUTINE ZOOM3(KEY,NPUT)
      use m_wearelt
      implicit none
      integer :: jashow
      integer :: jmouse
      integer :: key
      integer :: nput
      double precision :: xa
      double precision :: xlc
      double precision :: ya
      double precision :: ylc
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW
      XLC = (X1+X2)/2
      YLC = (Y1+Y2)/2
      CALL IMOUSECURSORXYG(real(XLC),real(YLC))
      CALL ZOOMIN(KEY,NPUT)
      RETURN
      END
