   ! SPvdP: TIELDB never called
      SUBROUTINE TIELDB()
      use m_netw
      USE m_missing
      use geometry_module, only: dpinpok, cross
      use m_sferic, only: jsferic
      use gridoperations

      implicit none
      double precision :: crp
      integer :: in1, in2, ja, jacros, k, k1, k2, k3, ku, L, Lnu
      double precision :: sl, sm, xcr, ycr, z, zcr, x1, x2, y1, y2
      DO L = 1,NUML
         K1 = KN(1,L)
         K2 = KN(2,L)
         IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
            CALL DPINPOK( XK(K1), YK(K1), ZK(K1), NPL, XPL, YPL, IN1, jins, dmiss)
            CALL DPINPOK( XK(K2), YK(K2), ZK(K2), NPL, XPL, YPL, IN2, jins, dmiss)
            IF (IN1 .EQ. 1 .AND. IN2 .EQ. 1) THEN
              CALL DRIETWEE(XK(K1),YK(K1),ZK(K1),x1, y1 ,Z)
              CALL DRIETWEE(XK(K2),YK(K2),ZK(K2),x2, y2,Z)
              K = 0
   10           K = K + 1
                KU = K + 1 ; IF (KU == MXLAN+1) KU = 1
                IF (XLAN(K) .NE. XYMIS .AND. XLAN(K+1) .NE. XYMIS) THEN
                   CALL CROSS(x1,y1,x2,y2,XLAN(K),YLAN(K),XLAN(K+1),YLAN(K+1),&
                              JACROS,SL,SM,XCR,YCR,CRP,jsferic, dmiss)
                   IF (JACROS .EQ. 1) THEN
                      LNU = L
                      NUMK = NUMK + 1
                      K3   = NUMK
                      ZCR  = SL*ZK(K2) + (1-SL)*ZK(K1)
                      CALL SETPOINT(XCR,YCR,ZCR,K3)
                      CALL ADDELEM(K1,K3,JA)
                      CALL ADDELEM(K2,K3,JA)
                   ENDIF
                ENDIF
                IF (K .LT. MXLAN) GOTO 10
            ENDIF
         ENDIF
      ENDDO
      RETURN
      END SUBROUTINE TIELDB
