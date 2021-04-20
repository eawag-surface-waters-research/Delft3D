      LOGICAL FUNCTION INVLIN(L)
      use m_netw
      implicit none
      integer :: k1
      integer :: k2
      integer :: l
      logical inview

      K1 = KN(1,L)
      K2 = KN(2,L)
	   INVLIN = INVIEW( XK(K1), YK(K1) ) .OR. INVIEW( XK(K2), YK(K2) )
      RETURN
      END
