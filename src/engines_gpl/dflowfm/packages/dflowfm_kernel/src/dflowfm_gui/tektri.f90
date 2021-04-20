      SUBROUTINE TEKTRI(XL,YL,NCOL)
      implicit none
      integer :: ncol
      double precision :: XL(3),YL(3)
      CALL SETCOL(NCOL)
      CALL MOVABS (XL(1),YL(1))
      CALL LNABS (XL(2),YL(2))
      CALL LNABS (XL(3),YL(3))
      CALL LNABS (XL(1),YL(1))
      RETURN
      END
