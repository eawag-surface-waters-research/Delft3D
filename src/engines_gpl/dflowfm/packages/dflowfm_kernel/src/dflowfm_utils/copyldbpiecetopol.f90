  SUBROUTINE COPYLDBPIECETOPOL(M1,M2)
  USE M_POLYGON
  use m_missing
  USE M_LANDBOUNDARY
  implicit none
  integer :: M1,M2

  integer :: m
  NPL = M2-M1+1

  call increasepol(npl, 0)
  DO M = M1,M2
     XPL(M-M1+1) = XLAN(M)
     YPL(M-M1+1) = YLAN(M)
  ENDDO
  RETURN
  END SUBROUTINE COPYLDBPIECETOPOL
