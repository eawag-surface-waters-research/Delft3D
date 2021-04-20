      SUBROUTINE TRAROT(XX,YY,XG,YG)
      USE M_MAPPROPARAMETERS
      implicit none

      double precision :: XX,YY,XG,YG
      XX = (XX - XCE)*XF
      YY = (YY - YCE)*YF
      XG = DELTX + XX*CSE - YY*SNE + XCE
      YG = DELTY + XX*SNE + YY*CSE + YCE
      RETURN
      END SUBROUTINE TRAROT
