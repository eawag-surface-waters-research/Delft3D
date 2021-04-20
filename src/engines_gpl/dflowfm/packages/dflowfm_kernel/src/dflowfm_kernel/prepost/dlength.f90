      DOUBLE PRECISION FUNCTION DLENGTH(K1,K2)
      use m_netw
      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none
      integer :: K1, K2
      double precision :: XD,YD,ZD
      IF (NETFLOW == 1) THEN
         XD      = XK(K2) - XK(K1)
         YD      = YK(K2) - YK(K1)
         ZD      = ZK(K2) - ZK(K1)
         DLENGTH = SQRT(XD*XD + YD*YD + ZD*ZD)
      ELSE ! FLOW
         DLENGTH = DBDISTANCE(XK(K1), YK(K1), XK(K2), YK(K2), jsferic, jasfer3D, dmiss)
      ENDIF
      RETURN
      END FUNCTION DLENGTH
