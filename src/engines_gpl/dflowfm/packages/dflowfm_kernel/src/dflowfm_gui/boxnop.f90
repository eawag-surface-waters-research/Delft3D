      SUBROUTINE BOXnop(XB1,YB1,XB2,YB2)
      implicit none
      double precision :: xb1
      double precision :: xb2
      double precision :: yb1
      double precision :: yb2
      call MOVABSnop(XB1,YB1)
      call LNABSnop(XB2,YB1)
      call LNABSnop(XB2,YB2)
      call LNABSnop(XB1,YB2)
      call LNABSnop(XB1,YB1)
      RETURN
      END
