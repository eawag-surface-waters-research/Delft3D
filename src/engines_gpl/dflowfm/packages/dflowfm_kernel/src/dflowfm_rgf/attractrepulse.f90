      SUBROUTINE ATTRACTREPULSE(     XH,     YH,      X,      Y,   mmax, nmax, MC,     NC,   NUMP,     JA)
      use m_missing
      use m_gridsettings
      use m_sferic
      use m_wearelt
      use geometry_module, only: dbdistance
      implicit none
      integer :: mmax, nmax, mc, nc, nump, ja
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX), XH(MMAX,NMAX), YH(MMAX,NMAX)


      integer :: MB,NB,MB2,NB2,NPT,NPT2,NPUTO,ITYPE
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE
!     ATTRACTIE, REPULSIE

      integer :: M1, N1, M2, N2, IN, JN, I, J, II, JJ, ii1, ii2, jj1, jj2, JANU, numpi, numpj
      double precision :: rsx, teken, dx, dy, dxy, dxy0, x0, y0, xn, yn, rn, fr

      M1    = MB(1)
      N1    = NB(1)
      M2    = MB(2)
      N2    = NB(2)
!     IN    = MIN(1,M2-M1)
!     JN    = MIN(1,N2-N1)
      JN    = MIN(1,M2-M1)
      IN    = MIN(1,N2-N1)
      NUMPI = IN*NUMP
      NUMPJ = JN*NUMP
!     RSX   = DSIX
      RSX = dbDISTANCE(X1,Y1,X2,Y2, jsferic, jasfer3D, dmiss)
      RSX = RSX/6
      JANU  = JA
      DO 10 I = M1,M2
         DO 10 J = N1,N2
            X0 = X(I,J)
            Y0 = Y(I,J)
            IF (X0 .NE. XYMIS) THEN
               IF (NPT .LE. 2) THEN
                  II1 = MAX(1,I-NUMPI)
                  II2 = MIN(I+NUMPI,MC)
                  JJ1 = MAX(1,J-NUMPJ)
                  JJ2 = MIN(J+NUMPJ,NC)
               ELSE
                  II1 = MAX(MB(3),I-NUMPI*1000)
                  II2 = MIN(I+NUMPI*1000,MB(4))
                  JJ1 = MAX(NB(3),J-NUMPJ*1000)
                  JJ2 = MIN(J+NUMPJ*1000,NB(4))
               ENDIF
               DO 20 II = II1,II2
                  DO 20 JJ = JJ1,JJ2
                     XN = X(II,JJ)
                     IF (XN .NE. XYMIS .AND. .NOT. (II .EQ. I .AND. JJ .EQ. J) ) THEN
                        YN = Y(II,JJ)
                        IF (NPT .LE. 2) THEN
                           RN = dbDISTANCE(XN,YN,X0,Y0, jsferic, jasfer3D, dmiss)
!                          RN = SQRT( (XN - X0)**2 + (YN - Y0)**2 )
                           IF (RN .LT. RSX) THEN
                              FR = (RSX - RN)/RSX
                              IF (IN .EQ. 1) THEN
                                 TEKEN = dble(SIGN(1,II - I))
                              ELSE IF (JN .EQ. 1) THEN
                                 TEKEN = dble(SIGN(1,JJ - J))
                              ENDIF
                              CALL DXYB(      X,      Y,     mmax, nmax, MC,         &
                                             NC,     II,     JJ,     IN, &
                                             JN,   DXY0                )
                              DXY = RFAC*TEKEN*FR*JANU*DXY0
                              IF (JSFERIC .EQ. 1) DXY = RD2DG*DXY/RA
                              DX  = DXY*IN
                              DY  = DXY*JN
                              CALL TOLOCL(   DX,     DY,      X,      Y, mmax, nmax, &
                                                     II,     JJ,      1)
                              XH(II,JJ) = XN + DX
                              YH(II,JJ) = YN + DY
                           ENDIF
                        ELSE
                           CALL SMEERFUNCTIE(II,JJ,I,J,FR,IN,JN)
                           IF (IN .EQ. 1) THEN
                              TEKEN = dble(SIGN(1,II - I))
                           ELSE IF (JN .EQ. 1) THEN
                              TEKEN = dble(SIGN(1,JJ - J))
                           ENDIF
                           CALL DXYB(      X,      Y,     mmax, nmax, MC,            &
                                          NC,     II,     JJ,     JN,    &
                                          IN,   DXY0                )
                           DXY = RFAC*TEKEN*FR*JANU*DXY0
                           IF (JSFERIC .EQ. 1) DXY = RD2DG*DXY/RA
                           DX  = DXY*IN
                           DY  = DXY*JN
                           CALL TOLOCL(   DX,     DY,      X,      Y,    mmax, nmax, &
                                                  II,     JJ,      1)
                           XH(II,JJ) = XN + DX
                           YH(II,JJ) = YN + DY
                        ENDIF
                     ENDIF
    20         CONTINUE
            ENDIF
    10 CONTINUE
      RETURN
      END subroutine attractrepulse
