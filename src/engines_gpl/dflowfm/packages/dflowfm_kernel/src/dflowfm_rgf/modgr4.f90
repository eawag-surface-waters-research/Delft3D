      SUBROUTINE MODGR4(NUMP,LANDORSPLINE)
      use m_grid
      use m_landboundary
      USE M_SPLINES, only : mcs, splnump=>nump
      implicit none
      integer :: nump, landorspline

      integer :: MB,NB,MB2,NB2,NPT,NPT2,NPUTO,ITYPE
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      integer :: m1, m2, n1, n2, i, j, in, jn, ncs, jdum
      double precision :: EPS, X0, Y0, XN, YN, DIS, RL
!     TO LAND
      DATA EPS /0.00001d0/
      IF (LANDORSPLINE .EQ. 1) THEN
         IF (MXLAN .EQ. 0) THEN
            CALL QNERROR('FIRST LOAD A LANDBOUNDARY',' ',' ')
            RETURN
         ENDIF
      ELSE
         call splnump(1, ncs)
         IF (MCS .LT. 1 .OR. NCS .LT. 2) THEN
            CALL QNERROR('FIRST DRAW SPLINE NR 1',' ',' ')
            RETURN
         ENDIF
      ENDIF
      M1    = MB(1)
      N1    = NB(1)
      M2    = MB(2)
      N2    = NB(2)
      IN    = MIN(1,N2-N1)
      JN    = MIN(1,M2-M1)
      DO 10 I = M1,M2
         DO 10 J = N1,N2
            X0 = Xch(I,J)
            Y0 = Ych(I,J)
            IF (LANDORSPLINE .EQ. 1) THEN
               CALL TOLAND(X0, Y0, 1, MXLAN, 1, XN, YN, DIS, JDUM, RL)
            ELSE
               CALL TOSPLINE(X0, Y0, XN, YN)
            ENDIF
            Xc(I,J) = XN
            Yc(I,J) = YN
            IF (ABS(Xch(I,J)-Xc(I,J)) .GT. EPS .OR.                      &
                ABS(Ych(I,J)-Yc(I,J)) .GT. EPS    ) THEN
                CALL MODFLD(     Xc,    Yc,     Xch,    Ych, mmax, nmax,           &
                                 MC,     NC,      I,      J,           &
                               NUMP,      1,     IN,     JN)
            ENDIF
    10 CONTINUE
      RETURN
       END subroutine modgr4
