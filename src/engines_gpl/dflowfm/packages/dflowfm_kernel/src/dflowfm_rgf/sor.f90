      SUBROUTINE SOR(A,B,C,D,E,U,RJAC,M1,N1,M2,N2)
      use m_grid
      use m_gridsettings
      use m_orthosettings, only: ITIN
      implicit none
      double precision :: anorm
      double precision :: anormf
      double precision :: half
      integer :: j
      integer :: l
      integer :: m1
      integer :: m2
      integer :: maxits
      integer :: n
      integer :: n1
      integer :: n2
      double precision :: one
      double precision :: qtr
      double precision :: rjac
      double precision :: zero
!     IMPLICIT double precision ::(A-H,O-Z)
      DOUBLE PRECISION :: A(MMAX,NMAX),B(MMAX,NMAX),C(MMAX,NMAX), D(MMAX,NMAX),E(MMAX,NMAX),U(MMAX,NMAX)

      PARAMETER(ZERO=0D0,HALF=.5D0,QTR=.25D0,ONE=1D0)
      DOUBLE PRECISION :: RESID, OMEGA
!     WRITE (MDIA,*) 'MEGS AVAILABLE SOR ', N4*4.096*0.001,
!      (N1+N2)*4.096*0.001d0
      MAXITS=ITIN
      ANORMF=ZERO
      OMEGA =ONE

      DO 15 N=1,MAXITS
        ANORM=ZERO
        DO 14 J=MAX(2,M1),MIN(M2,MC-1)
          DO 13 L=MAX(2,N1),MIN(N2,NC-1)
            IF (IJC (J,L) .EQ. 10) THEN
!              IF(MOD(J+L,2).EQ.MOD(N,2))THEN
                 RESID=A(J,L)*U(J+1,L)+B(J,L)*U(J-1,L)+    &
                     C(J,L)*U(J,L+1)+D(J,L)*U(J,L-1)+ E(J,L)*U(J,L)
                 U(J,L)=U(J,L)-OMEGA*RESID/E(J,L)
!              ENDIF
            ENDIF
13        CONTINUE
14      CONTINUE
        IF(N.EQ.1) THEN
          OMEGA=ONE/(ONE-HALF*RJAC**2)
        ELSE
          OMEGA=ONE/(ONE-QTR*RJAC**2*OMEGA)
        ENDIF
!       write(mdia,*) omega, rjac

15    CONTINUE
      RETURN
      END SUBROUTINE SOR
