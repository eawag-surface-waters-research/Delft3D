   SUBROUTINE SELLLINKSINPOL(LIN,N)
   use m_netw
   use m_missing, only: dmiss, jins
   use geometry_module, only: pinpok
   implicit none
   integer :: N
   INTEGER :: LIN(N)

   integer :: in
   integer :: in2
   integer :: k1
   integer :: k2
   integer :: l
   double precision :: xp1
   double precision :: xp2
   double precision :: xplmax
   double precision :: xplmin
   double precision :: yp1
   double precision :: yp2
   double precision :: yplmax
   double precision :: yplmin

   IF (NPL < 3) THEN
      LIN = 1
   ELSE
      CALL MINMAXPOL(XplMIN, YplMIN, XplMAX, YplMAX)
      DO L  = 1,NUML
         K1 = KN(1,L) ; Xp1 = XK(K1)   ; Yp1 = yK(K1)
         K2 = KN(2,L) ; Xp2 = XK(K2)   ; Yp2 = yK(K2)
         IF (Xp1 >= XplMIN .AND. Xp1 <= XplMAX .AND. Yp1 >= YplMIN .AND. Yp1 <= YplMAX  .AND.   &
             Xp2 >= XplMIN .AND. Xp2 <= XplMAX .AND. Yp2 >= YplMIN .AND. Yp2 <= YplMAX ) THEN
            CALL PINPOK(Xp1, Yp1, NPL, XPL, YPL, IN, jins, dmiss)
            CALL PINPOK(Xp2, Yp2, NPL, XPL, YPL, IN2, jins, dmiss)
            LIN(L) = in*in2
         ELSE
            LIN(L) = 0
         ENDIF
      ENDDO
   ENDIF
   END SUBROUTINE SELLLINKSINPOL
