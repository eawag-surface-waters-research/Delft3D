      SUBROUTINE ORGLOCATOR(XL,YL)
      use m_devices
      implicit none
      integer :: jashow
      integer :: jmouse
      integer :: ml
      integer :: nl
      double precision :: xa
      double precision :: xl
      double precision :: xlc
      double precision :: ya
      double precision :: yl
      double precision :: ylc
!     INITIATE CURSOR LOCATION
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW

      IF (XL .EQ. 0 .AND. YL .EQ. 0) THEN
         ML  = NPX/2
         NL  = NPY/2
         CALL TOWOR(ML,NL,XLC,YLC)
      ELSE
         XLC = XL
         YLC = YL
      ENDIF

      CALL IMOUSECURSORXYG(real(XLC),real(YLC))
      RETURN
      END
