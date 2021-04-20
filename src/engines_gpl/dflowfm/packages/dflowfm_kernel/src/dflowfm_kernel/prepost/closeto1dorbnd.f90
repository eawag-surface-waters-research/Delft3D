      SUBROUTINE CLOSETO1DORBND(XP1,YP1,N1) ! IF NOT IN FLOWCELL, MAYBE CLOSE TO 1d OF BND
      use m_FLOWGEOM                        ! je moet dwars op een flow liggen, anders doe je niet mee
                                            ! misschien is dat soms wat streng
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D
      use geometry_module, only: dbdistance, dlinedis

      implicit none
      integer          :: n1
      double precision :: XP1, YP1
      double precision :: dismin
      integer :: ja
      integer :: k1
      integer :: k2
      integer :: l
      double precision :: xa,ya,xb,yb,dis,xn,yn

      N1 = 0
      DISMIN = 9E+33
      DO L = 1,LNX
         IF (L <= LNX1d .OR. L > LNXI) THEN
             K1 = LN(1,L) ; K2 = LN(2,L)
             XA = XZ(K1)
             YA = YZ(K1)
             XB = XZ(K2)
             YB = YZ(K2)
             CALL dLINEDIS(XP1,YP1,XA,YA,XB,YB,JA,DIS,XN,YN, jsferic, jasfer3D, dmiss)
             IF (JA .EQ. 1) THEN
                IF (DIS .LT. DISMIN) THEN
                   N1 = L
                   DISMIN = DIS
                ENDIF
             ENDIF
          ENDIF
      ENDDO ! TODO: HK/AvD: this now ALWAYS returns a cell, even if the boundary cell is far away. Do we want that?
      IF (N1 .NE. 0) THEN
          K1 = LN(1,n1) ; K2 = LN(2,n1)
          IF (dbdistance(XP1,YP1,XZ(K1),YZ(K1),jsferic, jasfer3D, dmiss) < dbdistance(XP1,YP1,XZ(K2),YZ(K2),jsferic, jasfer3D, dmiss) ) THEN
             N1 = K1
          ELSE
             N1 = K2
          ENDIF
      ENDIF

      END SUBROUTINE CLOSETO1DORBND
