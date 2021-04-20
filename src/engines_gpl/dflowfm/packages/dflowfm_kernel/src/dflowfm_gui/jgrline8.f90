   SUBROUTINE JGRLINE8(X,Y,N) ! TEKEN LIJN, INCL XYMISSEN, GEBRUIK VAN INVIEW EN PROJECTIE

   use m_missing
  ! use gridoperations

   implicit none
   double precision   :: X(N), Y(N)
   integer            :: n

   integer            :: i
   integer            :: in
   integer            :: k
   integer            :: l
   double precision   :: XA, YA
   integer, parameter :: KMAX=4096     ! BEPERKING VAN INTERACTER
   real               :: XX(KMAX), YY(KMAX)
   logical inview2

   K  = 0
   L  = 0
   IN = 0
   I=0
   DO WHILE (I .LT. N)
      I = I + 1
      IF ( X(I) .NE. dXYMIS) THEN
         IF ( INVIEW2( X(I) ,Y(I), XA, YA ) ) IN = 1
         IF (K .EQ. 0 .OR. IN .EQ. 1 .OR. I .EQ. L+1) K  = K + 1
         IF (K .EQ. 1 .OR. IN .EQ. 1 .OR. I .EQ. L+1) THEN
            XX(K) = XA
            YY(K) = YA
         ENDIF
         IF (IN .EQ. 1) L = I
      ENDIF
      IF (I .EQ. N .OR. X(I) .EQ. dXYMIS .OR. K .EQ. KMAX) THEN
         IF (K .NE. 0) THEN
            CALL POLYLINE(XX,YY,K)
            IF (K .EQ. KMAX) I = I - 1
            K = 0
            L = 0
            IN = 0
         ENDIF
      ENDIF
   ENDDO
   RETURN
   END SUBROUTINE JGRLINE8
