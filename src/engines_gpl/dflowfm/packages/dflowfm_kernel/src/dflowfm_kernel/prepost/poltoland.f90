      SUBROUTINE POLTOLAND(L1,L2)               ! SHIFT POLYGON TO LANDBOUNDARY
      USE M_POLYGON
      USE M_MISSING
      USE M_LANDBOUNDARY
      implicit none
      integer :: l1
      integer :: l2

      integer :: in, jn
      integer :: l, j
      double precision :: xp, yp, xpn, ypn, dis, rL

      IN = 1 ; IF (L2 < L1) IN = -1
      DO L = L1,L2, IN
         XP = XPL(L)
         IF (XP .NE. XYMIS) THEN
            YP = YPL(L)
            CALL TOLAND(XP,YP, 1, MXLAN, 1, xpn, ypn, dis, j, rL)
            XPL(L) = xpn ; YPL(L) = ypn
         ENDIF
      ENDDO

      END SUBROUTINE POLTOLAND
