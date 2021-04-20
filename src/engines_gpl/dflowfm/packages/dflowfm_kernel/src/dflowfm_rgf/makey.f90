      SUBROUTINE MAKEY(XR,YR,MMAX,NMAX)  ! terug naar graden SUBROUTINE MAKEY
      USE M_SFERIC
      USE M_MISSING
      implicit none
      integer :: mmax,nmax

      DOUBLE PRECISION :: XR(MMAX,NMAX), YR(MMAX,NMAX), FI2
      integer :: i,j

      DO I = 1,MMAX
         DO J = 1,NMAX
            IF (XR(I,J) .NE. DXYMIS) THEN
               FI2     = ATAN(SINH(YR(I,J) ) )
               YR(I,J) = RD2DG*FI2
               XR(I,J) = RD2DG*XR(I,J)
            ENDIF
         ENDDO
      ENDDO
      RETURN
      END SUBROUTINE MAKEY
