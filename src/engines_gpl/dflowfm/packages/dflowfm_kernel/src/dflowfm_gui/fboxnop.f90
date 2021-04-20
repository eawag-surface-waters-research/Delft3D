      SUBROUTINE FBOXNOP(XB1,YB1,XB2,YB2)
      implicit none
      integer :: ndraw
      double precision :: xb1, xb2, yb1, yb2

      COMMON /DRAWTHIS/  ndraw(50)
      if (ndraw(10) == 0) then
         call RECTANGLE(real(XB1),real(YB1),real(XB2),real(YB2))
      else
         call fboxold(XB1,YB1,XB2,YB2)
      endif
      RETURN
      END
