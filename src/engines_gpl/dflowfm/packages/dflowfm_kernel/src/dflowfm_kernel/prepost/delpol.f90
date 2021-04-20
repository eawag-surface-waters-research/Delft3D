      SUBROUTINE DELPOL()
      USE M_POLYGON
      USE M_MISSING
      implicit none

      if ( allocated(xpl) ) XPL = XYMIS
      if ( allocated(ypl) ) YPL = XYMIS
      NPL = 0

      MP   = 0
      MPS  = 0
      RETURN
      END SUBROUTINE DELPOL
