   SUBROUTINE FINDK(     XL,     YL,    ZL,  KV )
   use m_netw
   implicit none
   double precision :: XL, YL, ZL
   integer :: KV
   integer :: k

   double precision :: RMIN, R, &
                       DX, DY, DZ
   RMIN  = 99D+20

   KV = 0
   DO K = 1,NUMK
      IF (XK(K) .NE. 0) THEN
         DX = XL - XK(K)
         DY = YL - YK(K)
         DZ = ZL - ZK(K)
         R  = DX*DX + DY*DY  + DZ*DZ
         IF (R .LT. RMIN) THEN
            RMIN = R
            KV   = K
         ENDIF
      ENDIF
   ENDDO

   RETURN
   END SUBROUTINE FINDK
