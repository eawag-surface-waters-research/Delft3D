      SUBROUTINE MAKEY1D(XR,YR,MNMAX)  ! terug naar graden SUBROUTINE MAKEY1D
      USE M_SFERIC
      USE M_MISSING
      implicit none
      integer :: mnmax
      DOUBLE PRECISION :: XR(MNMAX), YR(MNMAX), FI2
      integer :: i

      DO I = 1,MNMAX
         IF (XR(I) .NE. DXYMIS) THEN
            FI2   = ATAN(SINH(YR(I) ) )
            YR(I) = RD2DG*FI2
            XR(I) = RD2DG*XR(I)
         ENDIF
      ENDDO
      RETURN
      END SUBROUTINE MAKEY1D
