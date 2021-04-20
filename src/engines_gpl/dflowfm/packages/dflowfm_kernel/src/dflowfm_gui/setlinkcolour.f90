  SUBROUTINE SETLINKCOLOUR(L,NCOL)
  use m_netw
  use unstruc_colors
  implicit none
  integer :: L, NCOL, NCL
  IF (NCOL == 0) THEN      ! ERASE
     NCL = 0
  ELSE IF (NCOL == 1) THEN ! 1 MEANS: DRAW IN KN3 PREDEFINED COLOUR
     if (KN(3,L) == 0) then
        NCL = 31
     else IF (KN(3,L) == 1) THEN ! 1D
        NCL = NCOLRG
     else IF (KN(3,L) == 2) THEN ! 2D
        NCL = NCOLDN
     else IF (KN(3,L) == 3) THEN ! 1d2d internal
        NCL = NCOLNN
     else IF (KN(3,L) == 4) THEN ! 1d2d longitudinal
        NCL = NCOLRN
     else IF (KN(3,L) == 5) THEN ! 1d2d internal pipe streetinlet
        NCL = NCOLSP
     else IF (KN(3,L) == 6) THEN ! 1d mainbranch
        NCL = KLSAM
     else IF (KN(3,L) == 7) THEN ! 1d2d internal pipe roofgutter
        NCL = NCOLSP + 5
     ENDIF
  ELSE
     NCL = NCOL
  ENDIF
  CALL SETCOL(NCL)
  RETURN
  END
