   SUBROUTINE REMOVESAMPLESONTOPOFNETPOINTS(XS, YS, NS)
   use m_netw
   implicit none
   double precision :: XS(NS), YS(NS)
   integer :: ns

   double precision :: dx
   double precision :: dy
   integer :: jaontop
   integer :: k
   integer :: ks
   integer :: n
   double precision :: tolnet
   TOLNET = 0.1d0
   N = 0
   DO KS = 1,NS
      JAONTOP = 0
      DO K  = 1,NUMK
         DX = ABS( XK(K) - XS(KS) ) ; DY = ABS( YK(K) - YS(KS) )
         IF (DX < TOLNET .AND. DY < TOLNET) THEN
            JAONTOP = 1 ; CYCLE
         ENDIF
      ENDDO
      IF (JAONTOP == 0) THEN
         N = N + 1
         XS(N) = XS(KS) ; YS(N) = YS(KS)
      ENDIF
   ENDDO
   NS = N
   END SUBROUTINE REMOVESAMPLESONTOPOFNETPOINTS
