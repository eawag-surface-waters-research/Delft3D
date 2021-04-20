      SUBROUTINE INILCA()
      use m_wearelt
      implicit none
      integer :: jashow
      integer :: jmouse
      double precision :: XLC,YLC,XA,YA
      double precision :: xla,yla
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW
       !CALL ORGLOCATOR(XLA,XLB)
      XLA    = 0.05*xmax +0.95*xmin
      yLA    = 0.05*ymax +0.95*ymin
      CALL ANCHOR(XLA,yla)
      RETURN
      END
