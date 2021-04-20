      SUBROUTINE MAKEF1D(XR,YR,MNMAX) ! naar rekenvlak SUBROUTINE MAKEF1D
      USE M_SFERIC
      USE M_MISSING
      implicit none
      integer :: mnmax
      DOUBLE PRECISION :: XR(MNMAX), YR(MNMAX), FI2
      integer :: i

      DO I = 1,MNMAX
         IF (XR(I) .NE. DXYMIS) THEN
            FI2     = DG2RD*YR(I)
            YR(I) = ( 1D0 + SIN(FI2) ) / COS(FI2)
            YR(I) = LOG(YR(I))
            XR(I) = DG2RD*XR(I)
         ENDIF
      ENDDO
      RETURN
      END SUBROUTINE MAKEF1D
