      SUBROUTINE CLOSENETBNDLINK(XP1,YP1,N1)
      use m_netw
      use geometry_module, only: dlinedis
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none
      integer :: n1
      double precision :: xp1
      double precision :: yp1

      double precision :: dismin
      integer :: ja
      integer :: k1
      integer :: k2
      integer :: l
      double precision :: xa,ya,xb,yb,dis,xn,yn

      N1 = 0
      DISMIN = 9E+33
      DO L = 1,NUML
         IF (LNN(L) == 1) THEN
            K1 = KN(1,L) ; K2 = KN(2,L)
            XA = XK(K1)
            YA = YK(K1)
            XB = XK(K2)
            YB = YK(K2)
            CALL dLINEDIS(XP1,YP1,XA,YA,XB,YB,JA,DIS,XN,YN, jsferic, jasfer3D, dmiss)
            IF (JA .EQ. 1) THEN
               IF (DIS .LT. DISMIN) THEN
                  N1 = L
                  DISMIN = DIS
               ENDIF
            ENDIF
         ENDIF
      ENDDO

      END SUBROUTINE CLOSENETBNDLINK
