  SUBROUTINE COPYPOLTOLDB()
  use m_polygon
  USE M_LANDBOUNDARY
  USE M_MISSING
  implicit none

  integer :: k
  integer :: l

  L = MXLAN
  if ( L.gt.0) then
       if (xlan(L).ne.XYMIS ) then
          L = L + 1
       endif
  end if

  CALL INCREASELAN(L+NPL)
  if ( L.gt.0 ) then
     XLAN(L) = XYMIS ; YLAN(L) = XYMIS ; ZLAN(L) = XYMIS
  end if
  DO K = 1,NPL
     L = L + 1
     XLAN(L) = XPL(K)
     YLAN(L) = YPL(K)
     ZLAN(L) = ZPL(K)
  ENDDO
  MXLAN = L
  CALL DELPOL()
  RETURN
  END SUBROUTINE COPYPOLTOLDB
