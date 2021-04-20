      SUBROUTINE FBOX(X1,Y1,X2,Y2)
      implicit none
      integer :: ndraw
      double precision :: x1 , x2 , y1 , y2
      double precision :: xb1, xb2, yb1, yb2

      COMMON /DRAWTHIS/  ndraw(50)
      CALL DPROJECT(X1,Y1,XB1,YB1,1)
      CALL DPROJECT(X2,Y2,XB2,YB2,1)
      if (ndraw(10) == 0) then
         call RECTANGLE(real(XB1),real(YB1),real(XB2),real(YB2))
      else
         call fboxold(XB1,YB1,XB2,YB2)
      endif
      RETURN
      END
