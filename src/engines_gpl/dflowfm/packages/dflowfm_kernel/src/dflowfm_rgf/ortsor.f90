      SUBROUTINE ORTSOR(XR,YR,A,B,C,D,E,ATP,M1,N1,M2,N2,     &
                        XI2,YI2,XJ2,YJ2,XO,YO,             &
                        RJAC)
      use unstruc_colors
      use m_sferic
      use m_grid
      use m_gridsettings
      use m_orthosettings
      implicit none
      integer :: i
      integer :: key
      integer :: m1
      integer :: m2
      integer :: n1
      integer :: n2
      integer :: ndraw
      double precision :: rjac

      DOUBLE PRECISION :: XR(MMAX,NMAX), YR(MMAX,NMAX),   &
                         XI2(MMAX,NMAX),XJ2(MMAX,NMAX),                                &
                         YI2(MMAX,NMAX),YJ2(MMAX,NMAX),                                &
                          XO(MMAX,NMAX), YO(MMAX,NMAX),                                &
                           A(MMAX,NMAX),  B(MMAX,NMAX), C(MMAX,NMAX),                  &
                           D(MMAX,NMAX),  E(MMAX,NMAX),ATP(MMAX,NMAX)
      COMMON /DRAWTHIS/ ndraw(50)

      DO 10 I = 1,ITBND

         IF (NDRAW(8) .EQ. 0) CALL READYY(' ',0.25d0 + 0.75d0*( dble(I-1 + 0.20d0)/dble(ITBND) ))
         CALL SOR(A,B,C,D,E,XR,RJAC,M1,N1,M2,N2)

         IF (NDRAW(8) .EQ. 0) CALL READYY(' ',0.25d0 + 0.75d0*( dble(I-1 + 0.60d0)/dble(ITBND) ))
         CALL SOR(A,B,C,D,E,YR,RJAC,M1,N1,M2,N2)

         IF (NDRAW(8) .EQ. 0) CALL READYY(' ',0.25d0 + 0.75d0*( dble(I-1 + 1.00d0)/dble(ITBND) ))

         CALL BNDSMT(XR,YR,XI2,YI2,XJ2,YJ2,ATP,M1,N1,M2,N2)

         IF (NDRAW(8) .EQ. 1 .AND. MDESIGN .NE. 5) THEN
            IF (JSFERIC .EQ. 1) THEN
               CALL MAKEY2(XR,YR,XO,YO,MMAX,NMAX)
               CALL TEKGRD(XO,YO,MMAX,NMAX,M1,N1,M2,N2,NCOLDG,NDRAW(38),KEY,MC)
            ELSE
               CALL TEKGRD(XR,YR,MMAX,NMAX,M1,N1,M2,N2,NCOLDG,NDRAW(38),KEY,MC)
            ENDIF
         ENDIF
    10 CONTINUE
      RETURN
      END SUBROUTINE ORTSOR
