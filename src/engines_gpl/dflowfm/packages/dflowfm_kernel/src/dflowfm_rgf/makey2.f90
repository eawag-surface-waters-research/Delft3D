      SUBROUTINE MAKEY2(XR,YR,XO,YO,MMAX,NMAX)  ! Voor tekenen bij JSFERIC SUBROUTINE MAKEY2
      USE M_SFERIC
      USE M_MISSING
      implicit none
      DOUBLE PRECISION :: XR(MMAX,NMAX), YR(MMAX,NMAX), &
                          XO(MMAX,NMAX), YO(MMAX,NMAX), FI2
      integer :: mmax,nmax
      integer :: i,j



      DO I = 1,MMAX
         DO J = 1,NMAX
            IF (XR(I,J) .NE. XYMIS) THEN
               FI2     = ATAN(SINH(YR(I,J) ) )
               YO(I,J) = RD2DG*FI2
               XO(I,J) = RD2DG*XR(I,J)
            ELSE
               XO(I,J) = XYMIS
               YO(I,J) = XYMIS
            ENDIF
         ENDDO
      ENDDO
      RETURN
      END SUBROUTINE MAKEY2
