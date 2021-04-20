!
      SUBROUTINE SETWY(X1,Y1,X2,Y2)
      use unstruc_display, only : rcir, cr, dsix
      use m_sferic
      implicit none
      double precision :: x1, x2, y1, y2
      double precision :: yw, asp, xw, x, y, dy
!     SET WORLD COORDINATES WITH Y2 AS 1.0 ASPECT RATIO VALUE
!     AND RETURN Y2
      call INQASP(asp)
      x  = 0.5*(x1+x2)
      dy = (x2-x1)*asp
      y  = y1 + dy/2
      call SETWYnew(x,y,dy)
      RETURN
      END
