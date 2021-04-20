      LOGICAL FUNCTION INVNOD(K)
      use m_netw
      use unstruc_display
      implicit none
      integer :: k
      logical inview
      INVNOD = INVIEW( XK(K), YK(K) )
      RETURN
      END
