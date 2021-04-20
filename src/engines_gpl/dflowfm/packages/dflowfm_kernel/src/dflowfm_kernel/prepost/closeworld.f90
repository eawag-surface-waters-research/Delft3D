   SUBROUTINE CLOSEWORLD()
   USE M_NETW
   USE M_SFERIC
   implicit none
   INTEGER :: K1, K2, ja
   double precision :: xmn, xmx

   IF (JSFERIC == 0) RETURN

   XMN = minval(XK(1:numk))
   XMX = maxval(XK(1:numk))

   IF (ABS(XMN) < 1D-10 .AND. ABS(XMX-360d0) < 1D-10 ) THEN  !MAKE YOUR OWN 0-360 CONNECTIONS, only once

      DO K1 = 1, NUMK
         IF (REAL (XK(K1)) == 0.0) THEN
            DO K2 = 1,NUMK
                IF (REAL (XK(K2)) == 360.0) THEN
                  IF (ABS(YK(K1) - YK(K2) ) < 1D-10 ) THEN
                     CALL MERGENODES(K2,K1,JA)
                     EXIT
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDDO

   ENDIF


   END SUBROUTINE CLOSEWORLD
