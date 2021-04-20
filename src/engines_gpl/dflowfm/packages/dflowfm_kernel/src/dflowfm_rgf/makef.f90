      SUBROUTINE MAKEF(XR,YR,MMAX,NMAX) ! naar rekenvlak SUBROUTINE MAKEF
      USE M_SFERIC
      USE M_MISSING
      implicit none
      integer :: mmax,nmax
      DOUBLE PRECISION :: XR(MMAX,NMAX), YR(MMAX,NMAX), FI2
      integer :: i,j
      DO I = 1,MMAX
         DO J = 1,NMAX
            IF (XR(I,J) .NE. DXYMIS) THEN
               FI2     = DG2RD*YR(I,J)
               YR(I,J) = ( 1D0 + SIN(FI2) ) / COS(FI2)
               YR(I,J) = LOG(YR(I,J))
               XR(I,J) = DG2RD*XR(I,J)
            ENDIF
         ENDDO
      ENDDO
      RETURN
      END SUBROUTINE MAKEF
