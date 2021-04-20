   SUBROUTINE externaltrianglestoouterquads()

   use m_netw
   use m_polygon
   use m_missing, only: jins, dmiss
   use geometry_module, only: dpinpok
   use gridoperations

   implicit none

   integer :: in
   integer :: k1
   integer :: k2
   integer :: k3
   integer :: k4
   integer :: kp
   integer :: l
   integer :: lnu
   double precision :: xp
   double precision :: yp
   double precision :: zp

   DOUBLE PRECISION :: XL, YL, ZL = 0D0

   DO L  = 1,NUML
      K1 = KN(1,L) ; K2 = KN(2,L)
      IF (NMK(K1) .LE. 3 .AND. NMK(K2) .LE. 3) THEN
         XL = 0.5D0*( XK(K1) + XK(K2) )
         YL = 0.5D0*( YK(K1) + YK(K2) )
         CALL DPINPOK(XL, YL, ZL, NPL, XPL, YPL, IN, jins, dmiss )
         IF (IN .EQ. 1) THEN
            CALL GETQUAD(L,K1,K2,K3,K4)
            IF (K3 .NE. 0) THEN
               XP = 0.5D0*( 2*XK(K1) - XK(K4) + 2*XK(K2) - XK(K3) )
               YP = 0.5D0*( 2*YK(K1) - YK(K4) + 2*YK(K2) - YK(K3) )
               CALL GIVENEWNODENUM(KP)
               CALL SETPOINT(XP,YP,ZP,KP)
               CALL CONNECTDB(K2,KP,LNU)
               CALL CONNECTDB(KP,K1,LNU)
            ENDIF
         ENDIF
      ENDIF
   ENDDO

   END SUBROUTINE externaltrianglestoouterquads
