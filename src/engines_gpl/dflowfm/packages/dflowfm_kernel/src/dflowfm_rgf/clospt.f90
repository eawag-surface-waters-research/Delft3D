      SUBROUTINE CLOSPT(    X,      Y,     mmax, nmax, MC,     NC, &
                           XL,     YL,     MV,     NV)
      use m_missing
      implicit none

      integer :: mmax, nmax, mc, nc, mv, nv
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)
      double precision :: xl, yl

      double precision :: rmin, r
      integer :: i, j
      RMIN  = 1d+20

      DO 10 I = 1,MC
         DO 10 J = 1,NC
            IF (X(I,J) .NE. XYMIS) THEN
               R = ABS(XL - X(I,J) ) + ABS(YL - Y(I,J) )
               IF (R .LT. RMIN) THEN
                  RMIN = R
                  MV   = I
                  NV   = J
               ENDIF
            ENDIF
   10 CONTINUE
      RETURN
      END subroutine clospt
